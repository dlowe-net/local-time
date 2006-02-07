;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOCAL-TIME
;;;
;;; A package for manipulating times and dates.
;;;
;;; Based on Erik Naggum's "A Long, Painful History of Time" (1999)
;;;
;;; Authored by Daniel Lowe <dlowe@sanctuary.org>
;;;
;;; Copyright (c) 2005-2006 Daniel Lowe
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage local-time
  (:use cl)
  (:export local-time
		   make-local-time
		   local-time-day
		   local-time-sec
		   local-time-msec
		   local-time-zone
		   local-time<
		   local-time<=
		   local-time>
		   local-time>=
		   local-time=
		   local-time/=
		   local-time-adjust
		   local-time-designator
		   encode-local-time
		   decode-local-time
		   parse-timestring
		   format-timestring
		   universal-time
		   internal-time
		   unix-time
		   timezone
		   local-timezone
		   define-timezone
		   *default-timezone*
           now))

(in-package local-time)

(defstruct timezone
  (transitions nil)
  (subzones nil)
  (leap-seconds nil)
  (path nil)
  (loaded nil))

(defun read-binary-integer (stream byte-count &optional (signed nil))
  "Read BYTE-COUNT bytes from the binary stream STREAM, and return an integer which is its representation in network byte order (MSB).  If SIGNED is true, interprets the most significant bit as a sign indicator."
  (loop for offset from (* (1- byte-count) 8) downto 0 by 8
	 with result = 0
	 do (setf (ldb (byte 8 offset) result) (read-byte stream))
	 finally (if (and signed (< #x80000000 result))
				 (return (- result #x100000000))
				 (return result))))

(defun string-from-unsigned-vector (vector offset)
  "Returns a string created from the vector of unsigned bytes VECTOR starting at OFFSET which is terminated by a 0."
  (let ((null-pos (or (position 0 vector :start offset) (length vector))))
	(with-output-to-string (str)
	  (loop for idx from offset upto (1- null-pos)
		 do (princ (code-char (aref vector idx)) str)))))

(defun realize-timezone (zone &optional reload)
  "If timezone has not already been loaded or RELOAD is non-NIL, loads the timezone information from its associated unix file."
  (when (or reload (not (timezone-loaded zone)))
    (with-open-file (inf (timezone-path zone)
                     :direction :input
                     :element-type 'unsigned-byte)
      ;; read and verify magic number
      (let ((magic-buf (make-array 4 :element-type 'unsigned-byte)))
        (read-sequence magic-buf inf :start 0 :end 4)
        (when (string/= (map 'string #'code-char magic-buf) "TZif" :end1 4)
          (error "~a is not a timezone file." (timezone-path zone))))
      ;; skip 16 bytes for "future use"
      (let ((ignore-buf (make-array 16 :element-type 'unsigned-byte)))
        (read-sequence ignore-buf inf :start 0 :end 16))
      ;; read header values
      (let ((utc-indicator-count (read-binary-integer inf 4))
            (wall-indicator-count (read-binary-integer inf 4))
            (leap-count (read-binary-integer inf 4))
            (transition-count (read-binary-integer inf 4))
            (type-count (read-binary-integer inf 4))
            (abbrev-length (read-binary-integer inf 4)))
        (let ((timezone-transitions
               ;; read transition times
               (loop for idx from 1 upto transition-count
                  collect (read-binary-integer inf 4 t)))
              ;; read local time indexes
              (local-time-indexes
               (loop for idx from 1 upto transition-count
                  collect (read-binary-integer inf 1)))
              ;; read local time info
              (local-time-info
               (loop for idx from 1 upto type-count
                  collect (list (read-binary-integer inf 4 t)
                                (/= (read-binary-integer inf 1) 0)
                                (read-binary-integer inf 1))))
              ;; read leap second info
              (leap-second-info
               (loop for idx from 1 upto leap-count
                  collect (list (read-binary-integer inf 4)
                                (read-binary-integer inf 4))))
              (abbreviation-buf (make-array abbrev-length :element-type 'unsigned-byte)))
          (read-sequence abbreviation-buf inf :start 0 :end abbrev-length)
          (let ((wall-indicators
                 ;; read standard/wall indicators
                 (loop for idx from 1 upto wall-indicator-count
                    collect (read-binary-integer inf 1)))
                ;; read UTC/local indicators
                (local-indicators
                 (loop for idx from 1 upto utc-indicator-count
                    collect (read-binary-integer inf 1))))
            (setf (timezone-transitions zone)
                  (nreverse
                   (mapcar
                    (lambda (info index)
                      (list info index))
                    timezone-transitions
                    local-time-indexes)))
            (setf (timezone-subzones zone)
                  (mapcar
                   (lambda (info wall utc)
                     (list (first info)
                           (second info)
                           (string-from-unsigned-vector abbreviation-buf (third info))
                           (/= wall 0)
                           (/= utc 0)))
                   local-time-info
                   wall-indicators
                   local-indicators))
            (setf (timezone-leap-seconds zone)
                  leap-second-info)))))
    (setf (timezone-loaded zone) t))
  zone)

(defmacro define-timezone (zone-name zone-file &key (load nil))
  "Define zone-name (a symbol or a string) as a new timezone, lazy-loaded from zone-file (a pathname designator relative to the zoneinfo directory on this system.  If load is true, load immediately."
  (let ((zone-sym (if (symbolp zone-name) zone-name (intern zone-name))))
    `(prog1
         (defparameter ,zone-sym (make-timezone :path ,zone-file))
       ,@(when load
           `((realize-timezone ,zone-sym))))))

(defvar *default-timezone*)
(eval-when (:load-toplevel :execute)
  (define-timezone *default-timezone* #p"/etc/localtime"))

(defstruct local-time
  (day 0)
  (sec 0)
  (msec 0)
  (zone *default-timezone*))

(defparameter +utc-zone+ (make-timezone :subzones '((0 nil "UTC" nil nil))
                                        :loaded t)
  "The zone for Coordinated Universal Time.")

(defun unix-time (local-time)
  "Return the Unix time corresponding to the LOCAL-TIME"
  (+ (* (+ (local-time-day local-time) 11017) 86400)
	 (local-time-sec local-time)))

(defun timezone (local-time &optional timezone)
  "Return as multiple values the time zone as the number of seconds east of UTC, a boolean daylight-saving-p, the customary abbreviation of the timezone, the starting time of this timezone, and the ending time of this timezone."
  (let* ((zone (realize-timezone
                (or timezone (local-time-zone local-time) *default-timezone*)))
		 (subzone-idx (or
					   (second (assoc (unix-time local-time)
									  (timezone-transitions zone)
									  :test #'>))
					   0))
		 (subzone (nth subzone-idx (timezone-subzones zone))))
	(values
	 (first subzone)
	 (second subzone)
	 (third subzone))))

(defun local-time-adjust (source timezone &optional (destination nil))
  "Returns two values, the values of new DAY and SEC slots, or, if DESTINATION is a LOCAL-TIME instance, fills the slots with the new values and returns the destination"
  (realize-timezone (local-time-zone source))
  (realize-timezone timezone)
  (let* ((old-offset (nth-value 0 (timezone source (local-time-zone source))))
		 (new-offset (nth-value 0 (timezone source timezone)))
		 (offset-diff (- new-offset old-offset))
		 (new-day (+ (local-time-day source) (floor offset-diff 86400)))
		 (new-sec (+ (local-time-sec source) (mod offset-diff 86400))))
  (cond
	(destination
	 (setf (local-time-msec destination) (local-time-msec source)
		   (local-time-sec destination) new-sec
		   (local-time-day destination) new-day)
	 destination)
	(t
	 (values new-day new-sec)))))

(defun local-time-diff (time-a time-b)
  "Returns a new LOCAL-TIME containing the difference between TIME-A and TIME-B"
  (multiple-value-bind (day-a sec-a)
	  (local-time-adjust time-a (local-time-zone time-b))
	(let ((msec (- (local-time-msec time-a) (local-time-msec time-b)))
		  (seconds (- sec-a (local-time-sec time-b)))
		  (days (- day-a (local-time-day time-b))))
	  (when (minusp msec)
		(decf seconds)
		(incf msec 1000))
	  (when (minusp seconds)
		(decf days)
		(incf seconds 86400))
	  (make-local-time :msec msec
					   :sec seconds
					   :day days
					   :zone (local-time-zone time-b)))))

(defun local-time-sum (time-a time-b)
  "Returns a new LOCAL-TIME containing the sum of TIME-A and TIME-B"
  (multiple-value-bind (day-a sec-a)
	  (local-time-adjust time-a (local-time-zone time-b))
	(let ((msec (+ (local-time-msec time-a) (local-time-msec time-b)))
		  (sec (+ sec-a (local-time-sec time-b)))
		  (day (+ day-a (local-time-day time-b))))
	  (when (> msec 1000)
		(decf msec 1000)
		(incf sec))
	  (when (> sec 86400)
		(decf sec 86400)
		(incf day))
	  (make-local-time :msec msec
					   :sec sec
					   :day day
					   :zone (local-time-zone time-b)))))

(defun local-time-compare (time-a time-b)
  "Returns the symbols <, >, or =, describing the relationship between TIME-A and TIME-b."
  (multiple-value-bind (day-a sec-a)
	  (local-time-adjust time-a (local-time-zone time-b))
	(cond
	  ((< day-a (local-time-day time-b)) '<)
	  ((> day-a (local-time-day time-b)) '>)
	  ((< sec-a (local-time-sec time-b)) '<)
	  ((> sec-a (local-time-sec time-b)) '>)
	  ((< (local-time-msec time-a) (local-time-msec time-b)) '<)
	  ((> (local-time-msec time-a) (local-time-msec time-b)) '>)
	  (t                                                     '=))))

(defparameter +month-days+
  (make-array 12 :initial-contents
			  (loop for length across #(0 31 30 31 30 31 31 30 31 30 31 31)
				 as days = 0 then (+ days length)
				 collect days)))

(defun month-days (month)
  (aref +month-days+ month))

(defun decode-month (day)
  (position day +month-days+ :from-end t :test #'>=))

(defun local-time-day-of-week (local-time)
  (mod (+ 3 (local-time-day local-time)) 7))

(defun encode-local-time (ms ss mm hh day month year &optional timezone)
  "Return a new LOCAL-TIME instance corresponding to the specified time elements."
  (let* ((int-month (if (< month 3) (+ month 9) (- month 3)))
		 (int-year (if (< month 3) (- year 2001) (- year 2000)))
		 (zone (realize-timezone (or timezone *default-timezone*)))
		 (result (make-local-time
				  :msec ms
				  :sec (+ (* hh 3600) (* mm 60) ss)
				  :day (+ (floor (* int-year 1461) 4)
						  (month-days int-month)
						  (1- day))
				  :zone zone)))
	result
    (local-time-adjust result zone result)))

(defun local-time (&key (universal nil) (internal nil) (unix nil) (msec 0) (zone nil))
  "Produce a LOCAL-TIME instance from the provided numeric time representation."
  (declare (ignorable internal))
  (cond
	(universal
	 (multiple-value-bind (sec minute hour date month year)
		 (decode-universal-time universal)
	   (encode-local-time 0 sec minute hour date month year
                          (realize-timezone (or zone *default-timezone*)))))
	(internal
	 ;; FIXME: How to portably convert between internal time?
     (error "Conversion of internal time not implemented"))
	(unix
	 (let* ((days (floor unix 86400))
		   (secs (- unix (* days 86400))))
       (make-local-time :day (- days 11017)
                        :sec secs
                        :msec msec
                        :zone (realize-timezone
                               (or zone *default-timezone*)))))))

(defun now ()
  (local-time :universal (get-universal-time)))

(defun local-time< (time-a time-b)
  "Returns T if TIME-A is less than TIME-B"
  (eql (local-time-compare time-a time-b) '<))

(defun local-time<= (time-a time-b)
  "Returns T if TIME-A is less than or equal to TIME-B"
  (not (null (member (local-time-compare time-a time-b) '(< =)))))

(defun local-time> (time-a time-b)
  "Returns T if TIME-A is greater than TIME-B"
  (eql (local-time-compare time-a time-b) '>))

(defun local-time>= (time-a time-b)
  "Returns T if TIME-A is greater than or equal to TIME-B"
  (not (null (member (local-time-compare time-a time-b) '(> =)))))

(defun local-time= (time-a time-b)
  "Returns T if TIME-A is equal to TIME-B"
  (eql (local-time-compare time-a time-b) '=))

(defun local-time/= (time-a time-b)
  "Returns T if TIME-A is not equal to TIME-B"
  (not (eql (local-time-compare time-a time-b) '=)))

(defun local-time-designator ()
  "Convert a designator (real number) as a LOCAL-TIME instance"
  nil)

(defun local-time-decoded-date (local-time)
  (multiple-value-bind (leap-cycle year-days)
      (floor (local-time-day local-time) 1461)
    (multiple-value-bind (years month-days)
        (floor year-days 365)
      (let* ((month (decode-month month-days))
             (day (1+ (- month-days (month-days month)))))
        (values
         (+ (* leap-cycle 4)
            years
            (if (>= month 10)
                2001
                2000))
         (if (>= month 10)
             (- month 9)
             (+ month 3))
         day)))))

(defun local-time-decoded-time (local-time)
  (multiple-value-bind (hours hour-remainder)
      (floor (local-time-sec local-time) 3600)
    (multiple-value-bind (minutes seconds)
        (floor hour-remainder 60)
      (values
       hours
       minutes
       seconds))))

(defparameter +leap-factor+ 1461)

(defun decode-local-time (local-time)
  "Returns the decoded time as multiple values: ms, ss, mm, hh, day, month, year, day-of-week, daylight-saving-time-p, timezone, and the customary timezone abbreviation."
  (multiple-value-bind (hours minutes seconds)
      (local-time-decoded-time local-time)
    (multiple-value-bind (year month day)
        (local-time-decoded-date local-time)
      (values
       (local-time-msec local-time)
       seconds minutes hours
       day month year
       (local-time-day-of-week local-time)
       (local-time-zone local-time)
       (nth-value 2 (timezone local-time))))))

(defun split-timestring (raw-string start end junk-allowed)
  (let ((timestring (subseq raw-string start end)))
    (multiple-value-bind (now-ms now-ss now-mm now-hh now-day now-month now-year)
        (decode-local-time (now))
      (let ((year (when (> end 4)
                    (parse-integer timestring :start 0 :end 4 :junk-allowed junk-allowed)))
            (month (when (> end 7)
                     (parse-integer timestring :start 5 :end 7 :junk-allowed junk-allowed)))
            (day (when (> end 10)
                   (parse-integer timestring :start 8 :end 10 :junk-allowed junk-allowed)))
            (hh (when (> end 13)
                  (parse-integer timestring :start 11 :end 13 :junk-allowed junk-allowed)))
            (mm (when (> end 17)
                  (parse-integer timestring :start 14 :end 16 :junk-allowed junk-allowed)))
            (ss (when (> end 19)
                  (parse-integer timestring :start 17 :end 19 :junk-allowed junk-allowed)))
            (ms (when (and (member (char timestring 19) '(#\. #\,)) (> end 20))
                  (parse-integer timestring :start 20 :junk-allowed t))))
        (list
         (or ms now-ms)
         (or ss now-ss)
         (or mm now-mm)
         (or hh now-hh)
         (or day now-day)
         (or month now-month)
         (or year now-year))))))

(defun parse-timestring (timestring &key (start 0) (end nil) (junk-allowed nil))
  "Parse a timestring and return the corresponding LOCAL-TIME"
  (declare (ignorable junk-allowed))
  (apply #'encode-local-time
         (split-timestring timestring
                           start
                           (or end (1- (length timestring)))
                           junk-allowed)))

(defun construct-timestring (local-time universal-p timezone-p
                             date-elements time-elements date-separator
                             time-separator internal-separator)
  (with-output-to-string (str)
      (multiple-value-bind (msec sec minute hour day month year day-of-week zone)
          (decode-local-time local-time)
        (declare (ignore day-of-week))
        (check-type date-elements (integer 0 3))
        (check-type time-elements (integer 0 4))
        (cond
          ((> date-elements 2)
           (format str "~:[~;-~]~4,'0d~c"
                   (minusp year)
                   (abs year)
                   date-separator))
          ((plusp date-elements)
           ;; if the year is not shown, but other parts of the date are,
           ;; the year is replaced with a hyphen
           (princ "-" str)))
        (when (> date-elements 1)
          (format str "~2,'0d~c" month date-separator))
        (when (> date-elements 0)
          (format str "~2,'0d" day))
        (when (and (plusp date-elements) (plusp time-elements))
          (princ internal-separator str))
        (when (> time-elements 0)
          (format str "~2,'0d" hour))
        (when (> time-elements 1)
          (format str "~c~2,'0d" time-separator minute))
        (when (> time-elements 2)
          (format str "~c~2,'0d" time-separator sec))
        (when (> time-elements 3)
          (format str ",~6,'0d" msec))
        (when timezone-p
          (let* ((zone (if universal-p +utc-zone+ zone))
                 (offset (local-timezone local-time zone)))
            (format str "~2,'0@d:~2,'0d"
                    (floor offset 3600)
                    (abs (mod offset 3600))))))))

(defun format-timestring (stream local-time universal-p timezone-p
						  &optional date-elements time-elements
                          date-separator time-separator internal-separator)
  "Produces on stream the timestring corresponding to the LOCAL-TIME with the given options.  If STREAM is NIL, returns a string containing what would have been output.  If STREAM is T, prints the string to *standard-output*."
  (let ((str (construct-timestring local-time universal-p timezone-p
                                   (or date-elements 3)
                                   (or time-elements 4)
                                   (or date-separator #\-)
                                   (or time-separator #\:)
                                   (cond
                                     ((eql internal-separator 0) "")
                                     (internal-separator internal-separator)
                                     (t "T")))))
    (cond
      (stream
       (princ str stream)
       nil)
      (t
       str))))

(defun universal-time (local-time)
  "Return the UNIVERSAL-TIME corresponding to the LOCAL-TIME"
  (multiple-value-bind (msec seconds minutes hours day month year)
	  (decode-local-time local-time)
	(declare (ignore msec))
	(encode-universal-time seconds minutes hours day month year)))

(defun internal-time (local-time)
  "Return the internal system time corresponding to the LOCAL-TIME"
  ;; FIXME: How to portably convert between internal and local time?
  (declare (ignorable local-time))
  (error "Not implemented"))

(defun local-timezone (adjusted-local-time
                       &optional (timezone *default-timezone*))
  "Return the local timezone adjustment applicable at the already adjusted-local-time.  Used to reverse the effect of TIMEZONE and LOCAL-TIME-ADJUST."
  (let* ((unix-time (unix-time adjusted-local-time))
         (subzone-idx (or
                       (second (find-if
                                (lambda (tuple)
                                  (> unix-time
                                     (- (first tuple)
                                        (first
                                         (nth (second tuple)
                                              (timezone-subzones timezone))))))
                                (timezone-transitions timezone)))
                       0)))
    (first (nth subzone-idx (timezone-subzones timezone)))))

(defun read-timestring (stream char)
  (declare (ignore char))
  `(parse-timestring
    ,(with-output-to-string (str)
       (loop for c = (read-char stream nil #\space)
          until (or (eql c #\space) (eql c #\)))
          do (princ c str)
          finally (unread-char c stream)))))

(defun read-universal-time (stream char arg)
  (declare (ignore char arg))
  `(local-time :universal
               ,(parse-integer
                 (with-output-to-string (str)
                   (loop for c = (read-char stream nil #\space)
                      while (digit-char-p c)
                      do (princ c str)
                      finally (unread-char c stream))))))

(defun enable-read-macros ()
  (set-macro-character #\@ 'read-timestring)
  (set-dispatch-macro-character #\# #\@ 'read-universal-time))

(defmethod print-object ((object local-time) stream)
  "Print the LOCAL-TIME object using the standard reader notation"
  (princ "@" stream)
  (format-timestring stream object nil nil))

(defmethod print-object ((object timezone) stream)
  "Print the TIMEZONE object in a reader-rejected manner."
  (format stream "#<TIMEZONE: ~:[UNLOADED~;~{~a~^ ~}~]>"
          (timezone-loaded object)
          (mapcar #'third (timezone-subzones object))))
