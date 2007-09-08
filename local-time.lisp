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


(defpackage :local-time
    (:use #:cl #:cl-fad)
  (:export #:local-time
           #:make-local-time
           #:day-of
           #:sec-of
           #:usec-of
           #:timezone-of
           #:local-time<
           #:local-time<=
           #:local-time>
           #:local-time>=
           #:local-time=
           #:local-time/=
           #:local-time-max
           #:local-time-min
           #:local-time-adjust
           #:local-time-whole-year-difference
           #:local-time-adjust-days
           #:maximize-time-part
           #:minimize-time-part
           #:first-day-of-year
           #:last-day-of-year
           #:local-time-designator
           #:encode-local-time
           #:decode-local-time
           #:parse-timestring
           #:parse-datestring
           #:valid-date-p
           #:format-timestring
           #:format-datestring
           #:format-rfc3339-timestring
           #:parse-rfc3339-timestring
           #:universal-time
           #:internal-time
           #:unix-time
           #:timezone
           #:local-timezone
           #:define-timezone
           #:*default-timezone*
           #:now
           #:today
           #:enable-read-macros
           #:+utc-zone+
           #:+month-names+
           #:+short-month-names+
           #:+day-names+
           #:+short-day-names+
           #:+seconds-per-day+
           #:+seconds-per-hour+
           #:+seconds-per-minute+
           #:+minutes-per-day+
           #:+minutes-per-hour+
           #:+hours-per-day+
           #:astronomical-julian-date
           #:modified-julian-date
           #:astronomical-modified-julian-date))

(in-package :local-time)

(declaim (inline now today local-time-day local-time-sec local-time-msec))

(defparameter *project-home-directory*
  (make-pathname :directory (pathname-directory
                             (if (find-package "ASDF")
                                 (eval (read-from-string "(asdf:system-definition-pathname
                                                            (asdf:find-system '#:local-time))"))
                                 *load-pathname*))))

;;; Month information
(defparameter +month-names+
  '("" "January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December"))
(defparameter +short-month-names+
  '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
    "Dec"))

(defconstant +seconds-per-day+ 86400)

(defconstant +seconds-per-hour+ 3600)

(defconstant +seconds-per-minute+ 60)

(defconstant +minutes-per-day+ 1440)

(defconstant +minutes-per-hour+ 60)

(defconstant +hours-per-day+ 24)

(defconstant +leap-factor+ 1461)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +rotated-month-days-without-leap-day+ #.(coerce #(31 30 31 30 31 31 30 31 30 31 31 28)
                                                                '(simple-array fixnum (*))))

  (defparameter +rotated-month-offsets-without-leap-day+
    (coerce
     (cons 0
           (loop with sum = 0
                 for days :across +rotated-month-days-without-leap-day+
                 collect (incf sum days)))
     '(simple-array fixnum (*)))))

;;; Day information
(defparameter +day-names+
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defparameter +day-names-as-keywords+
  '(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))

(defparameter +short-day-names+
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

;; The astronomical julian date offset is the number of days between
;; the current date and -4713-01-01T00:00:00+00:00
(defparameter +astronomical-julian-date-offset+ -2451605)

;; The modified julian date is the number of days between the current
;; date and 1858-11-17T12:00:00+00:00.  For the sake of simplicity,
;; we currently just do the date arithmetic and don't adjust for the
;; time of day.
(defparameter +modified-julian-date-offset+ -51604)

(defstruct timezone
  (transitions nil :type list)
  (subzones nil :type list)
  (leap-seconds nil :type list)
  (path nil)
  (name "anonymous" :type string)
  (loaded nil :type boolean))

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

(defparameter +utc-zone+ (make-timezone :subzones '((0 nil "UTC" nil nil))
                                        :name "UTC"
                                        :loaded t)
  "The zone for Coordinated Universal Time.")

(defmacro define-timezone (zone-name zone-file &key (load nil))
  "Define zone-name (a symbol or a string) as a new timezone, lazy-loaded from zone-file (a pathname designator relative to the zoneinfo directory on this system.  If load is true, load immediately."
  (declare (type (or string symbol) zone-name))
  (let ((zone-sym (if (symbolp zone-name) zone-name (intern zone-name))))
    `(prog1
      (defparameter ,zone-sym (make-timezone :path ,zone-file
                                             :name ,(if (symbolp zone-name)
                                                        (string-downcase (symbol-name zone-name))
                                                        zone-name)))
      ,@(when load
              `((realize-timezone ,zone-sym))))))

(defvar *default-timezone*)
(eval-when (:load-toplevel :execute)
  (let ((default-timezone-file #p"/etc/localtime"))
    (if (probe-file default-timezone-file)
        (define-timezone *default-timezone* default-timezone-file :load t)
        (defparameter *default-timezone* +utc-zone+))))

(defparameter *timezone-repository* nil "A list of (list \"Europe/Budapest\" timezone) entries")
(defparameter *timezone-offset->timezone* (make-hash-table))

(eval-when (:load-toplevel :execute)
  (defun reread-timezone-repository ()
    (let* ((root-directory (merge-pathnames "zoneinfo/" *project-home-directory*))
           (cutoff-position (length (princ-to-string root-directory)))
           (visitor (lambda (file)
                      (let* ((full-name (subseq (princ-to-string file) cutoff-position))
                             (name (pathname-name file))
                             (timezone (realize-timezone (make-timezone :path file :name name))))
                        (push (list full-name timezone) *timezone-repository*)
                        ;; TODO this entire *timezone-offset->timezone* is probably useless this way,
                        ;; we can't reverse map a +01:30 offset to a timezone struct, or can we?
                        (dolist (subzone (timezone-subzones timezone))
                          (pushnew timezone (gethash (first subzone) *timezone-offset->timezone*)))))))
      (setf *timezone-repository* nil)
      (setf *timezone-offset->timezone* (make-hash-table))
      (walk-directory root-directory visitor :directories nil
                      :test (lambda (file)
                              (not (find "Etc" (pathname-directory file) :test #'string=))))
      ;; walk the Etc dir last, so they will be the first entries in the *timezone-offset->timezone* map
      (walk-directory (merge-pathnames "Etc/" root-directory) visitor :directories nil)
      (setf *timezone-repository* (sort *timezone-repository* #'string< :key #'first)))))

(defclass local-time ()
  ((day :accessor day-of :initarg :day :initform 0 :type integer)
   (sec :accessor sec-of :initarg :sec :initform 0 :type integer)
   (usec :accessor usec-of :initarg :usec :initform 0 :type (integer 0 999999))
   (timezone :accessor timezone-of :initarg :timezone
             :initform *default-timezone*)))

(defmacro make-local-time (&rest args)
  `(make-instance 'local-time ,@args))

(defun local-time-day (local-time)
  "Deprecated function to retrieve the day field from the local-time"
  (declare (type local-time local-time))
  (day-of local-time))

(defun local-time-sec (local-time)
  "Deprecated function to retrieve the seconds field from the local-time"
  (declare (type local-time local-time))
  (sec-of local-time))

(defun local-time-msec (local-time)
  "Deprecated function to retrieve the milliseconds field from the local-time"
  (declare (type local-time local-time))
  (floor (usec-of local-time) 1000))

(defun unix-time (local-time)
  "Return the Unix time corresponding to the LOCAL-TIME"
  (declare (type local-time local-time))
  (+ (* (+ (day-of local-time) 11017) 86400)
     (sec-of local-time)))

(defun timezone (local-time &optional timezone)
  "Return as multiple values the time zone as the number of seconds east of UTC, a boolean daylight-saving-p, the customary abbreviation of the timezone, the starting time of this timezone, and the ending time of this timezone."
  (declare (type local-time local-time)
           (type (or null timezone) timezone))
  (let* ((zone (realize-timezone
                (or timezone (timezone-of local-time) *default-timezone*)))
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
  (declare (type local-time source)
           (type (or null local-time) destination))
  (realize-timezone (timezone-of source))
  (realize-timezone timezone)
  (if (and (eq timezone (timezone-of source))
           (eq source destination))
      source
      (let* ((offset-diff (- (timezone source timezone)
                             (timezone source (timezone-of source))))
             (offset-sign (signum offset-diff)))
        (multiple-value-bind (offset-day offset-sec)
            (floor (abs offset-diff) 86400)
          (let ((new-day (+ (day-of source) (* offset-sign offset-day)))
                (new-sec (+ (sec-of source) (* offset-sign offset-sec))))
            (cond ((minusp new-sec)
                   (incf new-sec 86400)
                   (decf new-day))
                  ((>= new-sec 86400)
                   (incf new-day)
                   (decf new-sec 86400)))
            (cond
              (destination
               (setf (usec-of destination) (usec-of source)
                     (sec-of destination) new-sec
                     (day-of destination) new-day
                     (timezone-of destination) timezone)
               destination)
              (t
               (values new-day new-sec))))))))

(defun maximize-time-part (local-time &key timezone into)
  "Return a local-time with the time part set to the end of the day."
  (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
      (decode-local-time local-time)
    (declare (ignore usec sec min hour day-of-week daylight-saving-time-p))
    (encode-local-time 0 59 59 23 day month year :timezone (or timezone original-timezone) :into into)))

(defun minimize-time-part (local-time &key timezone into)
  "Return a local-time with the time part set to the beginning of the day."
  (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
      (decode-local-time local-time)
    (declare (ignore usec sec min hour day-of-week daylight-saving-time-p))
    (encode-local-time 0 0 0 0 day month year :timezone (or timezone original-timezone) :into into)))

(defun first-day-of-year (local-time-or-year &key into)
  "Return a local-time date with the month and day set to the first day of the year the input refers to."
  (let ((year
         (etypecase local-time-or-year
           (local-time
            (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
                (decode-local-time local-time-or-year)
              (declare (ignore usec sec min hour day month day-of-week daylight-saving-time-p original-timezone))
              year))
           (integer
            local-time-or-year))))
    (encode-local-time 0 0 0 0 1 1 year :timezone +utc-zone+ :into into)))

(defun last-day-of-year (local-time-or-year &key into)
  "Return a local-time date with the month and day set to the last day of the year the input refers to."
  (let ((year
         (etypecase local-time-or-year
           (local-time
            (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
                (decode-local-time local-time-or-year)
              (declare (ignore usec sec min hour day month day-of-week daylight-saving-time-p original-timezone))
              year))
           (integer
            local-time-or-year))))
    (encode-local-time 0 0 0 0 31 12 year :timezone +utc-zone+ :into into)))

(defun astronomical-julian-date (local-time)
  (- (day-of local-time) +astronomical-julian-date-offset+))

(defun modified-julian-date (local-time)
  (- (day-of local-time) +modified-julian-date-offset+))

(defun local-time-diff (time-a time-b)
  "Returns a new LOCAL-TIME containing the difference between TIME-A and TIME-B"
  (multiple-value-bind (day-a sec-a)
      (local-time-adjust time-a (timezone-of time-b))
      (let ((usec (- (usec-of time-a) (usec-of time-b)))
            (seconds (- sec-a (sec-of time-b)))
            (days (- day-a (day-of time-b))))
        (when (minusp usec)
          (decf seconds)
          (incf usec 1000000))
        (when (minusp seconds)
          (decf days)
          (incf seconds 86400))
        (make-local-time :usec usec
                         :sec seconds
                         :day days))))

(defun local-time-sum (time-a time-b)
  "Returns a new LOCAL-TIME containing the sum of TIME-A and TIME-B"
  (multiple-value-bind (day-a sec-a)
      (local-time-adjust time-a (timezone-of time-b))
    (let ((usec (+ (usec-of time-a) (usec-of time-b)))
          (sec (+ sec-a (sec-of time-b)))
          (day (+ day-a (day-of time-b))))
      (when (> usec 1000000)
        (decf usec 1000000)
        (incf sec))
      (when (> sec 86400)
        (decf sec 86400)
        (incf day))
      (make-local-time :usec usec
                       :sec sec
                       :day day
                       :timezone (timezone-of time-b)))))

(defun local-time-compare (time-a time-b)
  "Returns the symbols <, >, or =, describing the relationship between TIME-A and TIME-b."
  (declare (type local-time time-a time-b))
  (multiple-value-bind (day-a sec-a)
      (local-time-adjust time-a (timezone-of time-b))
    (cond
      ((< day-a (day-of time-b)) '<)
      ((> day-a (day-of time-b)) '>)
      ((< sec-a (sec-of time-b)) '<)
      ((> sec-a (sec-of time-b)) '>)
      ((< (usec-of time-a) (usec-of time-b)) '<)
      ((> (usec-of time-a) (usec-of time-b)) '>)
      (t '=))))

(defun local-time-adjust-days (local-time offset &key into)
  "Add OFFSET to days unless it's a keyword symbol name of a week-day. In that case point the result to the previous day given by OFFSET."
  (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p timezone)
      (decode-local-time local-time)
    (declare (ignore daylight-saving-time-p))
    (if (symbolp offset)
        (let ((position (position offset +day-names-as-keywords+ :test #'eq)))
          (assert position (position) "~S is not a valid day name" offset)
          (incf day (+ (- (if (zerop day-of-week)
                              7
                              day-of-week))
                       position)))
        (incf day offset))
    (encode-local-time usec sec min hour day month year :timezone timezone :into into)))

(defun local-time-whole-year-difference (time-a time-b)
  "Returns the number of whole years elapsed between time-a and time-b (hint: anniversaries)."
  (declare (type local-time time-b time-a))
  (let ((modified-time-b (local-time-adjust time-b (timezone-of time-a) (make-local-time))))
    (multiple-value-bind (usec-a sec-a minute-a hour-a day-a month-a year-a)
        (decode-local-time modified-time-b)
      (multiple-value-bind (usec-b sec-b minute-b hour-b day-b month-b year-b day-of-week-b daylight-p-b zone-b)
          (decode-local-time time-a)
        (declare (ignore usec-b sec-b minute-b hour-b day-b month-b day-of-week-b daylight-p-b zone-b))
        (let ((year-difference (- year-b year-a)))
          (if (local-time<= (encode-local-time usec-a sec-a minute-a hour-a day-a month-a
                                               (+ year-difference year-a)
                                               :timezone (timezone-of time-a))
                            time-a)
              year-difference
              (1- year-difference)))))))

(defun local-time-day-of-week (local-time)
  (mod (+ 3 (day-of local-time)) 7))

(defun encode-local-time (us ss mm hh day month year &key (timezone *default-timezone*) into)
  "Return a new LOCAL-TIME instance corresponding to the specified time elements."
  (declare (type integer us ss mm hh day month year)
           (type (or null timezone) timezone))
  (let* ((0-based-rotated-month (if (>= month 3) (- month 3) (+ month 9)))
         (int-year (if (< month 3) (- year 2001) (- year 2000)))
         (zone (realize-timezone timezone))
         (sec (+ (* hh 3600) (* mm 60) ss))
         (day (+ (floor (* int-year +leap-factor+) 4)
                 (aref #.+rotated-month-offsets-without-leap-day+ 0-based-rotated-month)
                 (1- day)))
         (result (if into
                     (progn
                       (setf (day-of into) day)
                       (setf (sec-of into) sec)
                       (setf (timezone-of into) zone)
                       into)
                     (make-local-time
                      :usec us
                      :sec sec
                      :day day
                      :timezone zone))))
    result))

(defun local-time (&key (universal nil) (unix nil) (usec 0) (timezone nil))
  "Produce a LOCAL-TIME instance from the provided numeric time representation or try to extract the most accurate current time if none of them is provided."
  (cond
    (universal
     (multiple-value-bind (sec minute hour date month year)
         (decode-universal-time universal)
       (encode-local-time usec sec minute hour date month year
                          :timezone (realize-timezone (or timezone
                                                          *default-timezone*)))))
    (unix
     (let* ((days (floor unix 86400))
            (secs (- unix (* days 86400))))
       (make-local-time :day (- days 11017)
                        :sec secs
                        :usec usec
                        :timezone (realize-timezone
                                   (or timezone *default-timezone*)))))
    (t #+sbcl
       (multiple-value-bind (_ sec usec) (sb-unix:unix-gettimeofday)
         (declare (ignore _) (type (unsigned-byte 32) sec usec))
         (let ((result (local-time :unix sec :usec usec :timezone +utc-zone+)))
           (local-time-adjust result (realize-timezone (or timezone
                                                           *default-timezone*))
                              result)))
       #-sbcl
       (local-time :universal (get-universal-time)))))

(defun today ()
  (minimize-time-part (now) :timezone +utc-zone+))

(defun now ()
  (local-time))

(defmacro defcomparator (name &body body)
  (let ((pair-comparator-name (intern (concatenate 'string "%" (string name)))))
    `(progn
      (declaim (inline ,pair-comparator-name))
      (defun ,pair-comparator-name (time-a time-b)
        ,@body)
      (defun ,name (&rest times)
        (declare (dynamic-extent times))
        (loop for (time-a time-b) :on times
              while time-b
              always (,pair-comparator-name time-a time-b)))
      (define-compiler-macro ,name (&rest times)
        (let ((vars (loop for time :in times
                          for i :upfrom 0
                          collect (gensym (concatenate 'string "TIME-" (princ-to-string i) "-")))))
          `(let (,@(loop for var :in vars
                         for time :in times
                         collect (list var time)))
            ;; we could evaluate comparisons of local-time literals here
            (and ,@(loop for (time-a time-b) :on vars
                         while time-b
                         collect `(,',pair-comparator-name ,time-a ,time-b)))))))))

(defcomparator local-time<
  (eql (local-time-compare time-a time-b) '<))

(defcomparator local-time<=
  (not (null (member (local-time-compare time-a time-b) '(< =)))))

(defcomparator local-time>
  (eql (local-time-compare time-a time-b) '>))

(defcomparator local-time>=
  (not (null (member (local-time-compare time-a time-b) '(> =)))))

(defcomparator local-time=
  (eql (local-time-compare time-a time-b) '=))

(defcomparator local-time/=
  (not (eql (local-time-compare time-a time-b) '=)))

;; TODO local-time-min/max could have a compiler macro
(defun local-time-min (time &rest times)
  (loop with winner = time
        for candidate :in times
        while candidate do
        (if (and winner
                 (local-time< candidate winner))
            (setf winner candidate))
        finally (return winner)))

(defun local-time-max (time &rest times)
  (loop with winner = time
        for candidate :in times
        while candidate do
        (if (and winner
                 (local-time> candidate winner))
            (setf winner candidate))
        finally (return winner)))

(defun local-time-designator ()
  "Convert a designator (real number) as a LOCAL-TIME instance"
  nil)

(defun local-time-decode-date (local-time)
  (declare (type local-time local-time))
  (multiple-value-bind (leap-cycles leap-cycle-days)
      (floor (day-of local-time) +leap-factor+)
    (multiple-value-bind (leap-cycle-years year-days)
        (floor leap-cycle-days 365)
      (let* ((leap-day-p (and (= leap-cycle-years 4)
                              (= year-days 0)))
             (rotated-1-based-month (if leap-day-p
                                        12 ;; march is the first month and february is the last
                                        (position year-days #.+rotated-month-offsets-without-leap-day+ :test #'<)))
             (1-based-month (if (>= rotated-1-based-month 11)
                                (- rotated-1-based-month 10)
                                (+ rotated-1-based-month 2)))
             (1-based-day (if leap-day-p
                              29
                              (1+ (- year-days (aref #.+rotated-month-offsets-without-leap-day+
                                                     (1- rotated-1-based-month)))))))
        (values
         (+ (* leap-cycles 4)
            (if leap-day-p
                (1- leap-cycle-years)
                leap-cycle-years)
            (if (>= rotated-1-based-month 11) ;; january is in the next year
                2001
                2000))
         1-based-month
         1-based-day)))))

(defun local-time-decode-time (local-time)
  (declare (type local-time local-time))
  (multiple-value-bind (hours hour-remainder)
      (floor (sec-of local-time) 3600)
    (multiple-value-bind (minutes seconds)
        (floor hour-remainder 60)
      (values
       hours
       minutes
       seconds))))

(defun decode-local-time (local-time)
  "Returns the decoded time as multiple values: ms, ss, mm, hh, day, month, year, day-of-week, daylight-saving-time-p, timezone, and the customary timezone abbreviation."
  (declare (type local-time local-time))
  (multiple-value-bind (hours minutes seconds)
      (local-time-decode-time local-time)
    (multiple-value-bind (year month day)
        (local-time-decode-date local-time)
      (values
       (usec-of local-time)
       seconds minutes hours
       day month year
       (local-time-day-of-week local-time)
       (nth-value 1 (timezone local-time))
       (timezone-of local-time)
       (nth-value 2 (timezone local-time))))))

(defun split-timestring (str &rest args)
  (declare (inline))
  (apply #'%split-timestring (coerce str 'simple-string) args))

(defun %split-timestring (time-string &key (start 0) (end (length time-string))
                                      (fail-on-error t) (time-separator #\:)
                                      (date-separator #\-)
                                      (date-time-separator #\T)
                                      (allow-missing-elements-p t)
                                      (allow-missing-date-part-p allow-missing-elements-p)
                                      (allow-missing-time-part-p allow-missing-elements-p)
                                      (allow-missing-timezone-part-p allow-missing-elements-p))
  "Based on http://www.ietf.org/rfc/rfc3339.txt including the function names used. Returns (values year month day hour minute second usec offset-hour offset-minute). If the parsing fails, then either signals an error or returns nil based on FAIL-ON-ERROR."
  (declare (type character date-time-separator time-separator date-separator)
           (type (simple-array character) time-string)
           (optimize (speed 3)))
  (the list
    (let (year month day hour minute second usec offset-hour offset-minute)
      (declare (type (or null fixnum) start end year month day hour minute second usec offset-hour offset-minute))
      (macrolet ((passert (expression)
                   `(unless ,expression
                     (parse-error)))
                 (parse-integer-into (start-end place &optional low-limit high-limit)
                   (let ((entry (gensym "ENTRY"))
                         (value (gensym "VALUE"))
                         (pos (gensym "POS"))
                         (start (gensym "START"))
                         (end (gensym "END")))
                     `(let ((,entry ,start-end))
                       (if ,entry
                           (let ((,start (car ,entry))
                                 (,end (cdr ,entry)))
                             (multiple-value-bind (,value ,pos) (parse-integer time-string :start ,start :end ,end :junk-allowed t)
                               (passert (= ,pos ,end))
                               (setf ,place ,value)
                               ,(if (and low-limit high-limit)
                                    `(passert (<= ,low-limit ,place ,high-limit))
                                    (values))
                               (values)))
                           (progn
                             (passert allow-missing-elements-p)
                             (values))))))
                 (with-parts-and-count ((start end split-chars) &body body)
                   `(multiple-value-bind (parts count) (split ,start ,end ,split-chars)
                     (declare (ignorable count) (type fixnum count)
                      ;;(type #1=(cons (cons fixnum fixnum) (or null #1#)) parts)
                      (type list parts))
                     ,@body)))
        (labels ((split (start end chars)
                   (declare (type fixnum start end))
                   (unless (consp chars)
                     (setf chars (list chars)))
                   (loop with last-match = start
                         with match-count of-type (integer 0 #.most-positive-fixnum) = 0
                         for index of-type fixnum upfrom start
                         while (< index end)
                         when (member (aref time-string index) chars :test #'char-equal)
                         collect (prog1 (if (< last-match index)
                                            (cons last-match index)
                                            nil)
                                   (incf match-count)
                                   (setf last-match (1+ index)))
                                 into result
                         finally (return (values (if (zerop (- index last-match))
                                                     result
                                                     (prog1
                                                         (nconc result (list (cons last-match index)))
                                                       (incf match-count)))
                                                 match-count))))
                 (parse ()
                   (with-parts-and-count (start end date-time-separator)
                     (cond ((= count 2)
                            (if (first parts)
                                (full-date (first parts))
                                (passert allow-missing-date-part-p))
                            (if (second parts)
                                (full-time (second parts))
                                (passert allow-missing-time-part-p))
                            (done))
                           ((and (= count 1)
                                 allow-missing-date-part-p
                                 (find time-separator time-string
                                       :start (car (first parts))
                                       :end (cdr (first parts))))
                            (full-time (first parts))
                            (done))
                           ((and (= count 1)
                                 allow-missing-time-part-p
                                 (find date-separator time-string
                                       :start (car (first parts))
                                       :end (cdr (first parts))))
                            (full-date (first parts))
                            (done)))
                     (parse-error)))
                 (full-date (start-end)
                   (let ((parts (split (car start-end) (cdr start-end) date-separator)))
                     (passert (eql (list-length parts) 3))
                     (date-fullyear (first parts))
                     (date-month (second parts))
                     (date-mday (third parts))))
                 (date-fullyear (start-end)
                   (parse-integer-into start-end year))
                 (date-month (start-end)
                   (parse-integer-into start-end month 1 12))
                 (date-mday (start-end)
                   (parse-integer-into start-end day 1 31))
                 (full-time (start-end)
                   (let ((start (car start-end))
                         (end (cdr start-end)))
                     (with-parts-and-count (start end (list #\Z #\- #\+))
                       (let* ((zulup (find #\Z time-string :test #'char-equal :start start :end end))
                              (sign (unless zulup
                                      (if (find #\+ time-string :test #'char-equal :start start :end end)
                                          1
                                          -1))))
                         (passert (<= 1 count 2))
                         (unless (and (eq (first parts) nil)
                                      (not (rest parts)))
                           ;; not a single #\Z
                           (partial-time (first parts)))
                         (when zulup
                           (setf offset-hour 0
                                 offset-minute 0))
                         (if (= count 1)
                             (passert allow-missing-timezone-part-p)
                             (let* ((entry (second parts))
                                    (start (car entry))
                                    (end (cdr entry)))
                               (declare (type fixnum start end))
                               (passert (or zulup
                                            (not (zerop (- end start)))))
                               (unless zulup
                                 (time-offset (second parts) sign))))))))
                 (partial-time (start-end)
                   (with-parts-and-count ((car start-end) (cdr start-end) time-separator)
                     (passert (eql count 3))
                     (time-hour (first parts))
                     (time-minute (second parts))
                     (time-second (third parts))))
                 (time-hour (start-end)
                   (parse-integer-into start-end hour 0 23))
                 (time-minute (start-end)
                   (parse-integer-into start-end minute 0 59))
                 (time-second (start-end)
                   (with-parts-and-count ((car start-end) (cdr start-end) '(#\. #\,))
                     (passert (<= 1 count 2))
                     (let ((*read-eval* nil))
                       (parse-integer-into (first parts) second 0 59)
                       (if (> count 1)
                           (let* ((start (car (second parts)))
                                  (end (cdr (second parts))))
                             (declare (type (integer 0 #.array-dimension-limit) start end))
                             (passert (<= (- end start) 6))
                             (let ((new-end (position-if (lambda (el)
                                                           (not (char= #\0 el)))
                                                         time-string :start start :end end :from-end t)))
                               (when new-end
                                 (setf end (min (1+ new-end)))))
                             ;;(break "~S: ~S"  (subseq time-string start end) (- end start))
                             (setf usec (* (the fixnum (parse-integer time-string :start start :end end))
                                           (aref #.(coerce #(1000000 100000 10000 1000 100 10 1)
                                                           '(simple-array fixnum (7)))
                                                 (- end start)))))
                           (setf usec 0)))))
                 (time-offset (start-end sign)
                   (with-parts-and-count ((car start-end) (cdr start-end) time-separator)
                     (passert (or allow-missing-timezone-part-p (= count 2)))
                     (parse-integer-into (first parts) offset-hour 0 23)
                     (if (second parts)
                         (parse-integer-into (second parts) offset-minute 0 59)
                         (setf offset-minute 0))
                     (setf offset-hour (* offset-hour sign)
                           offset-minute (* offset-minute sign))))
                 (parse-error ()
                   (if fail-on-error
                       (error "Failed to parse ~S as an rfc3339 time" time-string)
                       (return-from %split-timestring nil)))
                 (done ()
                   (return-from %split-timestring (list year month day hour minute second usec offset-hour offset-minute))))
          (parse))))))

(defun parse-rfc3339-timestring (timestring &key (fail-on-error t)
                                            (allow-missing-time-part-p nil))
  (parse-timestring timestring :fail-on-error fail-on-error
                    :allow-missing-timezone-part-p nil
                    :allow-missing-time-part-p allow-missing-time-part-p :allow-missing-date-part-p nil))

(defun parse-timestring (timestring &rest args)
  "Parse a timestring and return the corresponding LOCAL-TIME. See split-timestring for details. Unspecified fields in the timestring are initialized to their lowest possible value."
  (destructuring-bind (year month day hour minute second usec offset-hour offset-minute)
      (apply #'split-timestring timestring args)
    ;; TODO should we assert on month and leap rules here?
    (let ((timezone (if offset-hour
                        (progn
                          (unless offset-minute
                            (setf offset-minute 0))
                          (let ((offset-in-sec (* (+ (* 60 offset-hour) offset-minute) 60)))
                            
                            (if (and (= offset-minute 0)
                                     (= offset-hour 0))
                                +utc-zone+
                                (or ;; TODO this reverse mapping may not not work at all
                                 #+nil(setf timezone (first (gethash offset-in-sec *timezone-offset->timezone*)))
                                 ;; as a last resort, create an anonymous timezone
                                 (make-timezone :subzones `((,offset-in-sec nil "anonymous" nil nil))
                                                :name "anonymous"
                                                :loaded t)))))
                        *default-timezone*)))
      (unless usec (setf usec 0))
      (unless second (setf second 0))
      (unless minute (setf minute 0))
      (unless hour (setf hour 0))
      (unless day (setf day 1))
      (unless month (setf month 3))
      (unless year (setf year 2000))
      (encode-local-time usec second minute hour day month year :timezone timezone))))

(defun parse-datestring (string)
  (let* ((*default-timezone* +utc-zone+)
         (date (parse-timestring string)))
    (unless (valid-date-p date)
      (error "~S is not a valid date string" string))
    date))

(defun valid-date-p (local-time)
  (and local-time
       (eq (timezone-of local-time) +utc-zone+)
       (zerop (sec-of local-time))
       (zerop (usec-of local-time))))

(defun format-rfc3339-timestring (local-time &rest args &key omit-date-part-p omit-time-part-p
                                             omit-timezone-part-p &allow-other-keys)
  (declare (ignore omit-date-part-p omit-time-part-p omit-timezone-part-p))
  (apply #'format-timestring local-time args))

(defun format-timestring (local-time &key destination timezone (omit-date-part-p nil)
                                     (omit-time-part-p nil) (omit-timezone-part-p omit-time-part-p)
                                     (use-zulu-p t)
                                     (date-elements (if omit-date-part-p 0 3)) (time-elements (if omit-time-part-p 0 4))
                                     (date-separator #\-) (time-separator #\:)
                                     (date-time-separator #\T))
  "Produces on stream the timestring corresponding to the LOCAL-TIME with the given options. If DESTINATION is NIL, returns a string containing what would have been output.  If DESTINATION is T, prints the string to *standard-output*."
  (declare (type (or null stream) destination)
           (type (integer 0 3) date-elements)
           (type (integer 0 4) time-elements)
           (type local-time local-time))
  (let ((str (with-output-to-string (str)
               (when timezone
                 (setf local-time (local-time-adjust local-time timezone (make-local-time))))
               (multiple-value-bind (usec sec minute hour day month year day-of-week daylight-p zone)
                   (decode-local-time local-time)
                 (declare (ignore day-of-week daylight-p))
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
                   (princ date-time-separator str))
                 (when (> time-elements 0)
                   (format str "~2,'0d" hour))
                 (when (> time-elements 1)
                   (format str "~c~2,'0d" time-separator minute))
                 (when (> time-elements 2)
                   (format str "~c~2,'0d" time-separator sec))
                 (when (and (> time-elements 3)
                            (not (zerop usec)))
                   (format str ".~6,'0d" usec))
                 (unless omit-timezone-part-p
                   (let* ((offset (local-timezone local-time zone)))
                     (if (and use-zulu-p
                              (eq zone +utc-zone+))
                         (princ #\Z str)
                         (format str "~c~2,'0d~c~2,'0d"
                                 (if (minusp offset) #\- #\+)
                                 (abs (floor offset 3600))
                                 time-separator
                                 (abs (floor (mod offset 3600) 60))))))))))
    (when destination
      (princ str destination))
    str))

(defun format-datestring (date)
  (format-timestring date :omit-time-part-p t))

(defun universal-time (local-time)
  "Return the UNIVERSAL-TIME corresponding to the LOCAL-TIME"
  (multiple-value-bind (usec seconds minutes hours day month year day-of-week daylight-saving-time-p timezone)
      (decode-local-time local-time)
    (declare (ignore usec day-of-week daylight-saving-time-p))
    (encode-universal-time seconds minutes hours day month year (floor (timezone local-time timezone) -3600))))

(defun internal-time (local-time)
  "Return the internal system time corresponding to the LOCAL-TIME"
  ;; FIXME: How to portably convert between internal and local time?
  (declare (ignorable local-time))
  (error "Not implemented"))

(defun local-timezone (adjusted-local-time
                       &optional (timezone *default-timezone*))
  "Return the local timezone adjustment applicable at the already adjusted-local-time. Used to reverse the effect of TIMEZONE and LOCAL-TIME-ADJUST."
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
  (parse-timestring
   (with-output-to-string (str)
     (loop for c = (read-char stream nil #\space)
           until (or (eql c #\space) (eql c #\)))
           do (princ c str)
           finally (unread-char c stream)))
   :allow-missing-elements-p t))

(defun read-universal-time (stream char arg)
  (declare (ignore char arg))
  (local-time :universal
              (parse-integer
               (with-output-to-string (str)
                 (loop for c = (read-char stream nil #\space)
                       while (digit-char-p c)
                       do (princ c str)
                       finally (unread-char c stream))))))

(defun enable-read-macros ()
  (set-macro-character #\@ 'read-timestring)
  (set-dispatch-macro-character #\# #\@ 'read-universal-time)
  (values))

(defmethod print-object ((object local-time) stream)
  "Print the LOCAL-TIME object using the standard reader notation"
  (when *print-escape*
    (princ "@" stream))
  (format-timestring object :destination stream))

(defmethod print-object ((object timezone) stream)
  "Print the TIMEZONE object in a reader-rejected manner."
  (format stream "#<TIMEZONE: ~:[UNLOADED~;~{~a~^ ~}~]>"
          (timezone-loaded object)
          (mapcar #'third (timezone-subzones object))))