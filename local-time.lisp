;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOCAL-TIME
;;;
;;; A package for manipulating times and dates.
;;;
;;; Based on Erik Naggum's "A Long, Painful History of Time" (1999)
;;;
;;; Authored by Daniel Lowe <dlowe@bitmuse.com>
;;;
;;; Copyright (c) 2005-2007 Daniel Lowe
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
    (:use #:cl)
  (:export #:local-time
           #:make-local-time
           #:day-of
           #:sec-of
           #:nsec-of
           #:timezone-of
           #:local-time<
           #:local-time<=
           #:local-time>
           #:local-time>=
           #:local-time=
           #:local-time/=
           #:local-time-max
           #:local-time-min
           #:adjust-local-time
           #:adjust-local-time!
           #:local-time-whole-year-difference
           #:days-in-month
           #:local-time-
           #:local-time+
           #:encode-duration
           #:decode-duration
           #:parse-duration
           #:format-duration
           #:maximize-time-part
           #:minimize-time-part
           #:first-day-of-year
           #:last-day-of-year
           #:encode-local-time
           #:decode-local-time
           #:with-decoded-local-time
           #:parse-timestring
           #:parse-datestring
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
           #:+days-per-week+
           #:astronomical-julian-date
           #:modified-julian-date
           #:astronomical-modified-julian-date))

(in-package :local-time)

(declaim (inline now today encode-duration format-rfc3339-timestring)
         (ftype (function * (values simple-base-string)) format-rfc3339-timestring)
         (ftype (function * (values simple-base-string)) format-timestring)
         (ftype (function * (values simple-base-string)) format-duration)
         (ftype (function * (values fixnum)) local-timezone)
         (ftype (function (local-time) (values (integer 0 999999999) (integer 0 59) (integer 0 59) (integer 0 23)
                                               (integer 1 31) (integer 1 12) (integer -1000000 1000000)
                                               t t t))
                decode-local-time))

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

(defconstant +days-per-week+ 7)

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
;; date and 1858-11-17T12:00:00+00:00. TODO: For the sake of simplicity,
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

(defun string-from-unsigned-byte-vector (vector offset)
  "Returns a string created from the vector of unsigned bytes VECTOR starting at OFFSET which is terminated by a 0."
  (declare (type (vector (unsigned-byte 8)) vector))
  (let* ((null-pos (or (position 0 vector :start offset) (length vector)))
         (result (make-string (- null-pos offset) :element-type 'base-char)))
    (loop for input-index :from offset :upto (1- null-pos)
          for output-index :upfrom 0
          do (setf (aref result output-index) (code-char (aref vector input-index))))
    result))

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
              (abbreviation-buf (make-array abbrev-length :element-type '(unsigned-byte 8))))
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
                           (string-from-unsigned-byte-vector abbreviation-buf (third info))
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

(defun timezone= (timezone-1 timezone-2)
  "Return two values indicating the relationship between timezone-1 and timezone-2. The first value is whether the two timezones are equal and the second value indicates whether it is sure or not.

   In other words:
   (values t t) means timezone-1 and timezone-2 are definitely equal.
   (values nil t) means timezone-1 and timezone-2 are definitely different.
   (values nil nil) means that it couldn't be determined."
  (if (or (eq timezone-1 timezone-2)
          (equalp timezone-1 timezone-2))
      (values t t)
      (values nil nil)))

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
      (cl-fad:walk-directory root-directory visitor :directories nil
                             :test (lambda (file)
                                     (not (find "Etc" (pathname-directory file) :test #'string=))))
      ;; walk the Etc dir last, so they will be the first entries in the *timezone-offset->timezone* map
      (cl-fad:walk-directory (merge-pathnames "Etc/" root-directory) visitor :directories nil)
      (setf *timezone-repository* (sort *timezone-repository* #'string< :key #'first)))))

(defclass local-time ()
  ((day :accessor day-of :initarg :day :initform 0 :type integer)
   (sec :accessor sec-of :initarg :sec :initform 0 :type integer)
   (nsec :accessor nsec-of :initarg :nsec :initform 0 :type (integer 0 999999999))
   (timezone :accessor timezone-of :initarg :timezone
             :initform *default-timezone*)))

(defmacro make-local-time (&rest args)
  `(make-instance 'local-time ,@args))

(defmacro clone-local-time (local-time)
  (let ((tmp (gensym "TMP")))
    `(let ((,tmp ,local-time))
       (make-instance 'local-time
                      :nsec (nsec-of ,tmp) :sec (sec-of ,tmp)
                      :day (day-of ,tmp) :timezone (timezone-of ,tmp)))))

(defun unix-time (local-time)
  "Return the Unix time corresponding to the LOCAL-TIME"
  (declare (type local-time local-time))
  (+ (* (+ (day-of local-time)
           11017)
        +seconds-per-day+)
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

(defun adjust-to-timezone (source timezone &optional (destination nil))
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
            (floor (abs offset-diff) +seconds-per-day+)
          (let ((new-day (+ (day-of source) (* offset-sign offset-day)))
                (new-sec (+ (sec-of source) (* offset-sign offset-sec))))
            (cond ((minusp new-sec)
                   (incf new-sec +seconds-per-day+)
                   (decf new-day))
                  ((>= new-sec +seconds-per-day+)
                   (incf new-day)
                   (decf new-sec +seconds-per-day+)))
            (cond
              (destination
               (setf (nsec-of destination) (nsec-of source)
                     (sec-of destination) new-sec
                     (day-of destination) new-day
                     (timezone-of destination) timezone)
               destination)
              (t
               (values new-sec new-day))))))))

(defun maximize-time-part (local-time &key timezone into)
  "Return a local-time with the time part set to the end of the day."
  (multiple-value-bind (nsec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
      (decode-local-time local-time)
    (declare (ignore nsec sec min hour day-of-week daylight-saving-time-p))
    (encode-local-time 0 59 59 23 day month year :timezone (or timezone original-timezone) :into into)))

(defun minimize-time-part (local-time &key timezone into)
  "Return a local-time with the time part set to the beginning of the day."
  (multiple-value-bind (nsec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
      (decode-local-time local-time)
    (declare (ignore nsec sec min hour day-of-week daylight-saving-time-p))
    (encode-local-time 0 0 0 0 day month year :timezone (or timezone original-timezone) :into into)))

(defun first-day-of-year (local-time-or-year &key into)
  "Return a local-time date with the month and day set to the first day of the year the input refers to."
  (let ((year
         (etypecase local-time-or-year
           (local-time
            (multiple-value-bind (nsec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
                (decode-local-time local-time-or-year)
              (declare (ignore nsec sec min hour day month day-of-week daylight-saving-time-p original-timezone))
              year))
           (integer
            local-time-or-year))))
    (encode-local-time 0 0 0 0 1 1 year :timezone +utc-zone+ :into into)))

(defun last-day-of-year (local-time-or-year &key into)
  "Return a local-time date with the month and day set to the last day of the year the input refers to."
  (let ((year
         (etypecase local-time-or-year
           (local-time
            (multiple-value-bind (nsec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
                (decode-local-time local-time-or-year)
              (declare (ignore nsec sec min hour day month day-of-week daylight-saving-time-p original-timezone))
              year))
           (integer
            local-time-or-year))))
    (encode-local-time 0 0 0 0 31 12 year :timezone +utc-zone+ :into into)))

(defun astronomical-julian-date (local-time)
  (- (day-of local-time) +astronomical-julian-date-offset+))

(defun modified-julian-date (local-time)
  (- (day-of local-time) +modified-julian-date-offset+))

(defmacro with-decoded-local-time ((&key nsec sec minute hour day month year day-of-week daylight-p timezone)
                                   local-time &body forms)
  (let ((ignores)
        (variables))
    (macrolet ((initialize (&rest vars)
                 `(progn
                    ,@(loop for var :in vars
                            collect `(progn
                                       (unless ,var
                                         (setf ,var (gensym))
                                         (push ,var ignores))
                                       (push ,var variables)))
                    (setf ignores (nreverse ignores))
                    (setf variables (nreverse variables)))))
      (initialize nsec sec minute hour day month year day-of-week daylight-p timezone))
    `(multiple-value-bind (,@variables) (decode-local-time ,local-time)
       (declare (ignore ,@ignores))
       ,@forms)))

(defun encode-duration (&key (nsec 0) (usec 0) (msec 0) (sec 0) (minute 0) (hour 0) (day 0) (week 0))
  (+ (* +seconds-per-minute+
        (+ (* +minutes-per-hour+
              (+ (* +hours-per-day+
                    (+ (* +days-per-week+
                          week)
                       day))
                 hour))
           minute))
     sec
     (/ (+ (/ (+ (/ nsec
                    1000)
                 usec)
              1000)
           msec)
        1000)))

(defun decode-duration (duration)
  "Returns the decoded duration as multiple values: nsec, usec, msec, sec, minute, hour, day, week."
  (multiple-value-bind (second-duration second-remainder)
      (floor duration)
    (multiple-value-bind (minute-duration second)
        (floor second-duration +seconds-per-minute+)
      (multiple-value-bind (hour-duration minute)
          (floor minute-duration +minutes-per-hour+)
        (multiple-value-bind (day-duration hour)
            (floor hour-duration +hours-per-day+)
          (multiple-value-bind (week day)
              (floor day-duration +days-per-week+)
            (multiple-value-bind (msec msec-remainder)
                (floor (* second-remainder 1000))
              (multiple-value-bind (usec usec-remainder)
                  (floor (* msec-remainder 1000))
                (multiple-value-bind (nsec nsec-remainder)
                    (floor (* usec-remainder 1000))
                  (declare (ignore nsec-remainder))
                  (values nsec usec msec second minute hour day week))))))))))

;; TODO: factor out useful parts from split-timestring and refactor this code
(defun parse-duration (durationstr &key (date-separator #\-) (date-time-separator #\T))
  (let* ((date-time-separator-index (position date-time-separator durationstr))
         (extra-sec
          (if date-time-separator-index
              (let ((datestr (subseq durationstr 0 date-time-separator-index)))
                (* +seconds-per-day+
                   (multiple-value-bind (first-integer pos)
                       (parse-integer datestr :junk-allowed t)
                     (if (= pos (length datestr))
                         first-integer
                         (progn
                           (assert (char= date-separator (elt datestr pos)))
                           (+ (* first-integer +days-per-week+)
                              (parse-integer datestr :start (1+ pos))))))))
              0)))
    (with-decoded-local-time (:nsec nsec :sec sec :minute minute :hour hour)
        (parse-timestring (subseq durationstr (or date-time-separator-index 0)))
      (encode-duration :nsec nsec :sec (+ extra-sec sec) :minute minute :hour hour))))

(defun format-duration (duration &key (omit-date-part-p nil) (omit-time-part-p nil)
                        (date-elements (if omit-date-part-p 0 2)) (time-elements (if omit-time-part-p 0 4))
                        (date-separator #\-) (time-separator #\:) (date-time-separator #\T))
  (multiple-value-bind (nsec usec msec second minute hour day week)
      (decode-duration duration)
    (with-output-to-string (str nil :element-type 'base-char)
      (when (zerop week)
        (decf date-elements)
        (when (zerop day)
          (decf date-elements)))
      (when (> date-elements 1)
        (format str "~2,'0d~c" week date-separator))
      (when (> date-elements 0)
        (format str "~2,'0d" day))
      (when (and (plusp date-elements) (plusp time-elements))
        (princ date-time-separator str))
      (when (and (zerop date-elements)
                 (zerop hour))
        (decf time-elements)
        (when (zerop minute)
          (decf time-elements)
          (when (zerop second)
            (decf time-elements))))
      (when (> time-elements 3)
        (format str "~2,'0d~c" hour time-separator))
      (when (> time-elements 2)
        (format str "~2,'0d~c" minute time-separator))
      (when (> time-elements 1)
        (format str "~2,'0d" second))
      (when (> time-elements 0)
        (when (= time-elements 1)
          (format str "0"))
        (format str ".~d" (+ (* 1000
                                (+ (* 1000 msec)
                                   usec))
                             nsec))))))

(defun local-time-compare (time-a time-b)
  "Returns the symbols <, >, or =, describing the relationship between TIME-A and TIME-b."
  (declare (type local-time time-a time-b))
  (multiple-value-bind (sec-a day-a)
      (adjust-to-timezone time-a (timezone-of time-b))
    (cond
      ((< day-a (day-of time-b)) '<)
      ((> day-a (day-of time-b)) '>)
      ((< sec-a (sec-of time-b)) '<)
      ((> sec-a (sec-of time-b)) '>)
      ((< (nsec-of time-a) (nsec-of time-b)) '<)
      ((> (nsec-of time-a) (nsec-of time-b)) '>)
      (t '=))))

(defun normalize-month-year-pair (month year)
  "Normalizes the month/year pair: in case month is < 1 or > 12 the month and year are corrected to handle the overflow."
  (multiple-value-bind (year-offset month-minus-one)
      (floor (1- month) 12)
    (values (1+ month-minus-one)
            (+ year year-offset))))

(defun days-in-month (month year)
  "Returns the number of days in the given month of the specified year."
  ;; TODO dumb implementation, awaits optimization
  (multiple-value-bind (month1 year1)
      (normalize-month-year-pair month year)
    (multiple-value-bind (month2 year2)
        (normalize-month-year-pair (1+ month) year)
      ;; month1-year1 pair represents the current month
      ;; month2-year2 pair represents the sequential month
      ;; now we get timestamps for the first days of these months
      ;; and find out what the difference in days is
      (let ((timestamp1 (encode-local-time 0 0 0 0 1 month1 year1))
            (timestamp2 (encode-local-time 0 0 0 0 1 month2 year2)))
        (- (day-of timestamp2) (day-of timestamp1))))))

;; TODO scan all uses of FIX-OVERFLOW-IN-DAYS and decide where it's ok to silently fix and where should be and error reported
(defun fix-overflow-in-days (day month year)
  "In case the day number is higher than the maximal possible for the given month/year pair, returns the last day of the month."
  (let ((max-day (days-in-month month year)))
    (if (> day max-day)
        max-day
        day)))

(eval-when (:compile-toplevel :load-toplevel)

(defun expand-adjust-local-time-changes (local-time changes visitor)
  (dolist (change changes)
    (unless (or (= (length change) 3)
                (and (= (length change) 4)
                     (symbolp (third change))
                     (or (string= (third change) "TO")
                         (string= (third change) "BY"))))
      (error "Syntax error in expression ~S" change))
    (let ((operation (first change))
          (part (second change))
          (value (if (= (length change) 3)
                     (third change)
                     (fourth change))))
      (unless (member part '(:nsec :sec :sec-of-day :hour :day :day-of-week :day-of-month :month :year :timezone))
        (error "Unknown local-time part ~S" part))
      (cond
        ((string= operation "SET")
         (funcall visitor `(%set-local-time-part ,local-time ,part ,value)))
        ((string= operation "OFFSET")
         (funcall visitor `(%offset-local-time-part ,local-time ,part ,value)))
        (t (error "Unexpected operation ~S" operation))))))

(defun expand-adjust-local-time (local-time changes &key functional)
  (let* ((old (gensym "OLD"))
         (new (if functional
                  (gensym "NEW")
                  old))
         (forms (list)))
    (expand-adjust-local-time-changes old changes
                                      (lambda (change)
                                        (push
                                         `(progn
                                            (multiple-value-bind (nsec sec day timezone)
                                                ,change
                                              (setf (nsec-of ,new) nsec)
                                              (setf (sec-of ,new) sec)
                                              (setf (day-of ,new) day)
                                              (setf (timezone-of ,new) timezone))
                                            ,@(when functional
                                                `((setf ,old ,new))))
                                         forms)))
    (setf forms (nreverse forms))
    `(let* ((,old ,local-time)
            ,@(when functional
                `((,new (clone-local-time ,old)))))
       ,@forms
       ,old)))
) ; eval-when

(defmacro adjust-local-time (local-time &body changes)
  (expand-adjust-local-time local-time changes :functional t))

(defmacro adjust-local-time! (local-time &body changes)
  (expand-adjust-local-time local-time changes :functional nil))

(defun %set-local-time-part (time part new-value)
  ;; TODO think about error signalling. when, how to disable if it makes sense, ...
  (case part
    ((:nsec :sec-of-day :day)
     (let ((nsec (nsec-of time))
           (sec (sec-of time))
           (day (day-of time))
           (timezone (timezone-of time)))
       (case part
         (:nsec (setf nsec (coerce new-value '(integer 0 999999999))))
         (:sec-of-day (setf sec (coerce new-value '(integer 0 #.+seconds-per-day+))))
         (:day (setf day new-value)))
       (values nsec sec day timezone)))
    (otherwise
     (if (eq part :timezone)
         (multiple-value-bind (new-sec new-day)
             (adjust-to-timezone time new-value)
           (values (nsec-of time) new-sec new-day new-value))
         (with-decoded-local-time (:nsec nsec :sec sec :minute minute :hour hour
                                   :day day :month month :year year :timezone timezone)
             time
           (ecase part
             (:sec (setf sec new-value))
             (:minute (setf minute new-value))
             (:hour (setf hour new-value))
             (:day-of-month (setf day new-value))
             (:month (setf month new-value)
                     (setf day (fix-overflow-in-days day month year)))
             (:year (setf year new-value)
                    (setf day (fix-overflow-in-days day month year))))
           (encode-local-time-into-values nsec sec minute hour day month year :timezone timezone))))))

(defun %offset-local-time-part (time part offset)
  "Returns a time adjusted by the specified OFFSET. Takes care of different kinds of overflows. The setting :day-of-week is possible using a keyword symbol name of a week-day (see +DAY-NAMES-AS-KEYWORDS+) as value. In that case point the result to the previous day given by OFFSET."
  (labels ((direct-adjust (part offset nsec sec day timezone)
             (cond ((eq part :day-of-week)
                    (with-decoded-local-time (:day-of-week day-of-week)
                        time
                      (let ((position (position offset +day-names-as-keywords+ :test #'eq)))
                        (assert position (position) "~S is not a valid day name" offset)
                        (let ((offset (+ (- (if (zerop day-of-week)
                                                7
                                                day-of-week))
                                         position)))
                          (values nsec sec (+ day offset) timezone)))))
                   ((zerop offset)
                    ;; The offset is zero, so just return the parts of the local-time object
                    (values nsec sec day timezone))
                   (t
                    (case part
                      (:nsec
                       (multiple-value-bind (sec-offset new-nsec)
                           (floor (+ offset nsec) 1000000000)
                         ;; the time might need to be adjusted a bit more if q != 0
                         (direct-adjust :sec sec-offset
                                        new-nsec sec day timezone)))
                      (:day
                       (values nsec sec (+ day offset) timezone))
                      (otherwise
                       (multiple-value-bind (days-offset new-sec)
                           (floor (+ sec (* offset (ecase part
                                                     (:sec 1)
                                                     (:minute +seconds-per-minute+)
                                                     (:hour +seconds-per-hour+))))
                                  +seconds-per-day+)
                         (direct-adjust :day days-offset
                                        nsec new-sec day timezone)))))))
           (safe-adjust (part offset time)
             (with-decoded-local-time (:nsec nsec :sec sec :hour hour :day day
                                       :month month :year year :timezone timezone)
                 time
               (multiple-value-bind (month-new year-new)
                   (normalize-month-year-pair
                    (+ (ecase part
                         (:month offset)
                         (:year (* 12 offset)))
                       month)
                    year)
                 ;; Almost there. However, it is necessary to check for
                 ;; overflows first
                 (encode-local-time-into-values nsec sec month hour
                                                (fix-overflow-in-days day month-new year-new)
                                                month-new year-new
                                                :timezone timezone)))))
    (ecase part
      ((:nsec :sec :minute :hour :day :day-of-week)
       (direct-adjust part offset
                      (nsec-of time)
                      (sec-of time)
                      (day-of time)
                      (timezone-of time)))
      ((:month :year) (safe-adjust part offset time)))))

(defun local-time-whole-year-difference (time-a time-b)
  "Returns the number of whole years elapsed between time-a and time-b (hint: anniversaries)."
  (declare (type local-time time-b time-a))
  (let ((modified-time-b (adjust-to-timezone time-b (timezone-of time-a) (make-local-time))))
    (multiple-value-bind (nsec-a sec-a minute-a hour-a day-a month-a year-a)
        (decode-local-time modified-time-b)
      (multiple-value-bind (nsec-b sec-b minute-b hour-b day-b month-b year-b day-of-week-b daylight-p-b zone-b)
          (decode-local-time time-a)
        (declare (ignore nsec-b sec-b minute-b hour-b day-b month-b day-of-week-b daylight-p-b zone-b))
        (let ((year-difference (- year-b year-a)))
          (if (local-time<= (encode-local-time nsec-a sec-a minute-a hour-a day-a month-a
                                               (+ year-difference year-a)
                                               :timezone (timezone-of time-a))
                            time-a)
              year-difference
              (1- year-difference)))))))

(defun local-time- (time-a time-b)
  "Returns the difference between TIME-A and TIME-B in seconds"
  (setf time-a (adjust-local-time time-a (set :timezone (timezone-of time-b))))
  (multiple-value-bind (sec-a day-a)
      (adjust-to-timezone time-a (timezone-of time-b))
    (let ((nsec (- (nsec-of time-a) (nsec-of time-b)))
          (second (- sec-a (sec-of time-b)))
          (day (- day-a (day-of time-b))))
      (when (minusp nsec)
        (decf second)
        (incf nsec 1000000000))
      (when (minusp second)
        (decf day)
        (incf second +seconds-per-day+))
      (encode-duration :nsec nsec :sec second :day day))))

(defun local-time+ (time seconds)
  "Returns a new LOCAL-TIME containing the sum of TIME and SECONDS"
  (adjust-local-time time (offset :sec seconds)))

(defun local-time-day-of-week (local-time)
  (mod (+ 3 (day-of local-time)) 7))

;; TODO read http://java.sun.com/j2se/1.4.2/docs/api/java/util/GregorianCalendar.html (or something else, sorry :)
;; this scheme only works back until 1582, the start of the gregorian calendar.
;; see also DECODE-LOCAL-TIME when fixing if fixing is desired at all.
;; TODO support a :overflow-allowed and signal an error for invalid values? (as opposed to silently adding to
;; the bigger place-value what we do now)
(defun encode-local-time-into-values (nsec sec minute hour day month year &key (timezone *default-timezone*))
  "Returns (VALUES NSEC SEC DAY ZONE) ready to be used for instantiating a new local-time object."
  (declare (type integer nsec sec minute hour day month year)
           (type (or null timezone) timezone))
  (if (> nsec 999999999)
      (multiple-value-bind (more-secs remaining-nsec)
          (floor nsec 1000000000)
        (setf nsec remaining-nsec)
        (incf sec more-secs)))
  (let* ((0-based-rotated-month (if (>= month 3)
                                    (- month 3)
                                    (+ month 9)))
         (internal-year (if (< month 3)
                            (- year 2001)
                            (- year 2000)))
         (years-as-days (years-to-days internal-year))
         (zone (realize-timezone timezone))
         (sec (+ (* hour +seconds-per-hour+)
                 (* minute +seconds-per-minute+)
                 sec))
         (days-from-zero-point (+ years-as-days
                                  (aref #.+rotated-month-offsets-without-leap-day+ 0-based-rotated-month)
                                  (1- day))))
    (values nsec sec days-from-zero-point zone)))

(defun encode-local-time (nsec sec minute hour day month year &key (timezone *default-timezone*) into)
  "Return a new LOCAL-TIME instance corresponding to the specified time elements."
  (declare (type integer nsec sec minute hour day month year)
           (type (or null timezone) timezone))
  (multiple-value-bind (nsec sec day timezone)
      (encode-local-time-into-values nsec sec minute hour day month year :timezone timezone)
    (if into
        (progn
          (setf (nsec-of into) nsec)
          (setf (sec-of into) sec)
          (setf (day-of into) day)
          (setf (timezone-of into) timezone)
          into)
        (make-local-time
         :nsec nsec
         :sec sec
         :day day
         :timezone timezone))))

(defun local-time (&key universal unix nsec timezone)
  "Produce a LOCAL-TIME instance from the provided numeric time representation or try to extract the most accurate current time if none of them is provided."
  (cond
    (universal
     (multiple-value-bind (sec minute hour date month year)
         (decode-universal-time universal)
       (encode-local-time (or nsec 0) sec minute hour date month year
                          :timezone (realize-timezone (or timezone
                                                          *default-timezone*)))))
    (unix
     (let* ((days (floor unix +seconds-per-day+))
            (secs (- unix (* days +seconds-per-day+))))
       (make-local-time :day (- days 11017)
                        :sec secs
                        :nsec (or nsec 0)
                        :timezone (realize-timezone
                                   (or timezone *default-timezone*)))))
    (t #+sbcl
       (multiple-value-bind (_ sec usec) (sb-unix:unix-gettimeofday)
         (declare (ignore _) (type (unsigned-byte 32) sec usec))
         (let ((result (local-time :unix sec
                                   :nsec (or nsec (* usec 1000))
                                   :timezone +utc-zone+)))
           (adjust-to-timezone result (realize-timezone (or timezone
                                                            *default-timezone*))
                               result)))
       #-sbcl
       (local-time :universal (get-universal-time) :nsec nsec))))

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

(eval-when (:compile-toplevel :load-toplevel)
  (defun years-to-days (years)
    (let* ((days (* years 365))
           (l1 (floor years 4))
           (l2 (floor years 100))
           (l3 (floor years 400)))
      (+ days l1 (- l2) l3))))

(defun days-to-years (days)
  "Returns (values years remaining-days-in-year)"
  (let ((remaining-days days))
    (multiple-value-bind (400-years remaining-days)
        (floor remaining-days 146097)
      (let* ((100-years (min 3 (floor remaining-days 36524)))
             (remaining-days (- remaining-days (* 100-years 36524))))
        (multiple-value-bind (4-years remaining-days)
            (floor remaining-days 1461)
          (let ((years (min 3 (floor remaining-days 365))))
            (values (+ (* 400-years 400)
                       (* 100-years 100)
                       (* 4-years 4)
                       years)
                    (- remaining-days
                       (* years 365))))))))
  ;; the above is the macroexpansion of the following. uses metabang BIND, but kept for clarity because the expansion is unreadable.
  #+nil
  (bind ((remaining-days days)
         ((values 400-years remaining-days) (floor remaining-days #.(years-to-days 400)))
         (100-years (min (floor remaining-days #.(years-to-days 100))
                         3))
         (remaining-days (- remaining-days
                            (* 100-years
                               #.(years-to-days 100))))
         ((values 4-years remaining-days) (floor remaining-days #.(years-to-days 4)))
         (years (min (floor remaining-days 365)
                     3)))
    (values (+ (* 400-years 400)
               (* 100-years 100)
               (* 4-years 4)
               years)
            (- remaining-days (* years 365)))))

(defun local-time-decode-date (time)
  (declare (type local-time time))
  (multiple-value-bind (years remaining-days)
      (days-to-years (day-of time))
    (let* ((leap-day-p (= remaining-days 365))
           (rotated-1-based-month (if leap-day-p
                                      12 ; march is the first month and february is the last
                                      (position remaining-days #.+rotated-month-offsets-without-leap-day+ :test #'<)))
           (1-based-month (if (>= rotated-1-based-month 11)
                              (- rotated-1-based-month 10)
                              (+ rotated-1-based-month 2)))
           (1-based-day (if leap-day-p
                            29
                            (1+ (- remaining-days (aref #.+rotated-month-offsets-without-leap-day+
                                                        (1- rotated-1-based-month)))))))
      (values
       (+ years
          (if (>= rotated-1-based-month 11) ; january is in the next year
              2001
              2000))
       1-based-month
       1-based-day))))

(defun local-time-decode-time (local-time)
  (declare (type local-time local-time))
  (multiple-value-bind (hours hour-remainder)
      (floor (sec-of local-time) +seconds-per-hour+)
    (multiple-value-bind (minutes seconds)
        (floor hour-remainder +seconds-per-minute+)
      (values
       hours
       minutes
       seconds))))

(defun decode-local-time (local-time)
  "Returns the decoded time as multiple values: nsec, ss, mm, hh, day, month, year, day-of-week, daylight-saving-time-p, timezone, and the customary timezone abbreviation."
  (declare (type local-time local-time))
  (multiple-value-bind (hours minutes seconds)
      (local-time-decode-time local-time)
    (multiple-value-bind (year month day)
        (local-time-decode-date local-time)
      (values
       (nsec-of local-time)
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
  "Based on http://www.ietf.org/rfc/rfc3339.txt including the function names used. Returns (values year month day hour minute second nsec offset-hour offset-minute). If the parsing fails, then either signals an error or returns nil based on FAIL-ON-ERROR."
  (declare (type character date-time-separator time-separator date-separator)
           (type simple-string time-string)
           (optimize (speed 3)))
  (the list
    (let (year month day hour minute second nsec offset-hour offset-minute)
      (declare (type (or null fixnum) start end year month day hour minute second offset-hour offset-minute)
               (type (or null (signed-byte 32)) nsec))
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
                             (passert (<= (- end start) 9))
                             (let ((new-end (position-if (lambda (el)
                                                           (not (char= #\0 el)))
                                                         time-string :start start :end end :from-end t)))
                               (when new-end
                                 (setf end (min (1+ new-end)))))
                             ;;(break "~S: ~S" (subseq time-string start end) (- end start))
                             (setf nsec (* (the (integer 0 999999999) (parse-integer time-string :start start :end end))
                                           (aref #.(coerce #(1000000000 100000000 10000000
                                                             1000000 100000 10000 1000 100 10 1)
                                                           '(simple-array (signed-byte 32) (10)))
                                                 (- end start)))))
                           (setf nsec 0)))))
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
                   (return-from %split-timestring (list year month day hour minute second nsec offset-hour offset-minute))))
          (parse))))))

(defun parse-rfc3339-timestring (timestring &key (fail-on-error t)
                                            (allow-missing-time-part-p nil))
  (parse-timestring timestring :fail-on-error fail-on-error
                    :allow-missing-timezone-part-p nil
                    :allow-missing-time-part-p allow-missing-time-part-p :allow-missing-date-part-p nil))

(defun parse-timestring (timestring &rest args)
  "Parse a timestring and return the corresponding LOCAL-TIME. See split-timestring for details. Unspecified fields in the timestring are initialized to their lowest possible value."
  (destructuring-bind (year month day hour minute second nsec offset-hour offset-minute)
      (apply #'split-timestring timestring args)
    ;; TODO should we assert on month and leap rules here?
    (let ((timezone (if offset-hour
                        (progn
                          (unless offset-minute
                            (setf offset-minute 0))
                          (let ((offset-in-sec (* (+ (* +minutes-per-hour+
                                                        offset-hour)
                                                     offset-minute)
                                                  +seconds-per-minute+)))
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
      (unless nsec (setf nsec 0))
      (unless second (setf second 0))
      (unless minute (setf minute 0))
      (unless hour (setf hour 0))
      (unless day (setf day 1))
      (unless month (setf month 3))
      (unless year (setf year 2000))
      (encode-local-time nsec second minute hour day month year :timezone timezone))))

(defun parse-datestring (string)
  (let* ((*default-timezone* +utc-zone+)
         (date (parse-timestring string)))
    (unless (and date
                 (eq (timezone-of date) +utc-zone+)
                 (zerop (sec-of date))
                 (zerop (nsec-of date)))
      (error "~S is not a valid date string" string))
    date))

(defun format-rfc3339-timestring (local-time &key destination omit-date-part-p omit-time-part-p
                                  omit-timezone-part-p (use-zulu-p t))
  (format-timestring local-time :destination destination
                     :omit-date-part-p omit-date-part-p :omit-time-part-p omit-time-part-p
                     :omit-timezone-part-p omit-timezone-part-p :use-zulu-p use-zulu-p))

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
           (type local-time local-time)
           (optimize (speed 3)))
  (let* ((*print-pretty* nil)
         (*print-circle* nil)
         (result))
    (setf result
          (with-output-to-string (str nil :element-type 'base-char)
            (when timezone
              (setf local-time (adjust-to-timezone local-time timezone (make-local-time))))
            (multiple-value-bind (nsec sec minute hour day month year day-of-week daylight-p zone)
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
                         (not (zerop nsec)))
                (format str ".~6,'0d" (floor nsec 1000)))
              (unless omit-timezone-part-p
                (let* ((offset (local-timezone local-time zone)))
                  (if (and use-zulu-p
                           (eq zone +utc-zone+))
                      (princ #\Z str)
                      (format str "~c~2,'0d~c~2,'0d"
                              (if (minusp offset) #\- #\+)
                              (abs (truncate offset +seconds-per-hour+))
                              time-separator
                              (abs (truncate (mod offset +seconds-per-hour+)
                                             +seconds-per-minute+)))))))))
    (when destination
      (write-string result destination))
    result))

(defun format-datestring (date)
  (format-timestring date :omit-time-part-p t))

(defun universal-time (local-time)
  "Return the UNIVERSAL-TIME corresponding to the LOCAL-TIME"
  (multiple-value-bind (nsec seconds minutes hours day month year day-of-week daylight-saving-time-p timezone)
      (decode-local-time local-time)
    (declare (ignore nsec day-of-week daylight-saving-time-p))
    (encode-universal-time seconds minutes hours day month year (floor (timezone local-time timezone) #.(- +seconds-per-hour+)))))

(defun internal-time (local-time)
  "Return the internal system time corresponding to the LOCAL-TIME"
  ;; FIXME: How to portably convert between internal and local time?
  (declare (ignorable local-time))
  (error "Not implemented"))

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
