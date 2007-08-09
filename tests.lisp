
(defpackage #:local-time.tests
  (:use :cl :local-time :5am))

(in-package #:local-time.tests)

(def-suite :local-time)

(in-suite* #:local-time.make :in :local-time)

(test make-local-time
  (let ((local-time (make-local-time :usec 1 :sec 2 :day 3 :timezone *default-timezone*)))
    (is-every =
      (usec-of local-time) 1
      (sec-of local-time) 2
      (day-of local-time) 3)))

(defmacro defcmptest (compare-with &body args)
  `(test ,compare-with
    (flet ((make (day &optional (sec 0) (usec 0))
             (make-local-time :day day :sec sec :usec usec)))
      ,@(loop for entry in args
              if (= (length entry) 1)
              do (push 'is entry)
              else do (setf (car entry) (if (member (car entry) '(t true is is-true) :test #'eq)
                                            'is
                                            'is-false))
              collect `(,(first entry) (,compare-with (make ,@(second entry))
                                        (make ,@(third entry))))))))
(defcmptest local-time<
  (true (1 0 0)
        (2 0 0))
  (true (0 1 0)
        (0 2 0))
  (true (0 0 1)
        (0 0 2))

  (false (2 0 0)
         (1 0 0))
  (false (0 2 0)
         (0 1 0))
  (false (0 0 2)
         (0 0 1)))

(defcmptest local-time<=
  (true (1 0 0)
        (2 0 0))
  (true (0 1 0)
        (0 2 0))
  (true (0 0 1)
        (0 0 2))
  (true (1 0 0)
        (1 0 0))
  (true (1 1 0)
        (1 1 0))
  (true (1 1 1)
        (1 1 1))

  (false (2 0 0)
         (1 0 0))
  (false (0 2 0)
         (0 1 0))
  (false (0 0 2)
         (0 0 1)))

(defcmptest local-time>
  (true (2 0 0)
        (1 0 0))
  (true (0 2 0)
        (0 1 0))
  (true (0 0 2)
        (0 0 1))

  (false (1 0 0)
         (2 0 0))
  (false (0 1 0)
         (0 2 0))
  (false (0 0 1)
         (0 0 2)))

(defcmptest local-time>=
  (true (2 0 0)
        (1 0 0))
  (true (0 2 0)
        (0 1 0))
  (true (0 0 2)
        (0 0 1))
  (true (1 0 0)
        (1 0 0))
  (true (1 1 0)
        (1 1 0))
  (true (1 1 1)
        (1 1 1))

  (false (1 0 0)
         (2 0 0))
  (false (0 1 0)
         (0 2 0))
  (false (0 0 1)
         (0 0 2)))

(defcmptest local-time=
  (true (1 0 0)
        (1 0 0))
  (true (1 1 0)
        (1 1 0))
  (true (1 1 1)
        (1 1 1))

  (false (1 0 0)
         (2 0 0))
  (false (0 1 0)
         (0 2 0))
  (false (0 0 1)
         (0 0 2)))

(test local-time=-1
  (is (local-time= (make-local-time) (make-local-time)))
  (is-false (local-time= (make-local-time) (make-local-time :usec 1))))

(test local-time/=
  (is (local-time/= (make-local-time) (make-local-time :usec 1)))
  (is-false (local-time/= (make-local-time) (make-local-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test encode-local-time
  (let ((local-time (encode-local-time 0 0 0 0 1 3 2000)))
    (is-every =
      (usec-of local-time) 0
      (day-of local-time) 0
      (sec-of local-time) 0))
  (let ((local-time (encode-local-time 0 0 0 0 29 2 2000)))
    (is-every =
      (usec-of local-time) 0
      (day-of local-time) -1
      (sec-of local-time) 0))
  (let ((local-time (encode-local-time 0 0 0 0 2 3 2000)))
    (is-every =
      (usec-of local-time) 0
      (sec-of local-time) 0
      (day-of local-time) 1))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 2000)))
    (is-every =
      (usec-of local-time) 0
      (sec-of local-time) 0
      (day-of local-time) -60))
  (let ((local-time (encode-local-time 0 0 0 0 1 3 2001)))
    (is-every =
      (usec-of local-time) 0
      (sec-of local-time) 0
      (day-of local-time) 365)))

(defmacro encode-decode-test (args &body body)
  `(let ((local-time (encode-local-time ,@(subseq args 0 7))))
    (is (equal (decode-local-time local-time)
               (values ,@args ,@(let ((stars nil))
                                     (dotimes (n (- 11 (length args)))
                                       (push '* stars))
                                     stars))))
    ,@body))

(test decode-local-time
  (encode-decode-test (5 5 5 5 5 5 1990 6))
  (encode-decode-test (0 0 0 0 1 3 2001 4))
  (encode-decode-test (0 0 0 0 1 3 1998 0))
  (encode-decode-test (1 2 3 4 5 6 2008 4)
    (is (eq (timezone-of local-time) *default-timezone*))
    (is (= 3 (length (multiple-value-list (timezone local-time))))))
  (encode-decode-test (0 0 0 0 1 1 0)
    (is (equal (multiple-value-list (decode-local-time local-time))
               `(0 0 0 0 1 1 0 5
                 ,(nth-value 1 (timezone local-time))
                 ,*default-timezone*
                 ,(nth-value 2 (timezone local-time))))))
  (let ((local-time (make-local-time
                     :day (- (random 65535) 36767)
                     :sec (random 86400)
                     :usec (random 1000))))
    (multiple-value-bind (ms ss mm hh day mon year) (decode-local-time local-time)
      (is (local-time= local-time (encode-local-time ms ss mm hh day mon year))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test format-timestring
  (is-every string=
    "2008-06-05T04:03:02.000001"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t)

    "2008-06-05T04:03:02.000001-05:00"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008
                                          :timezone (local-time::make-timezone
                                                     :subzones `((,(* (* 60 5) -60) nil "anonymous" nil nil))
                                                     :loaded t)))

    "2008-06-05T04:03:02.000001Z"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008 :timezone +utc-zone+) :use-zulu-p t)

    "2008-06-05T04:03:02.12345678+00:00"
    (format-timestring (encode-local-time 12345678 2 3 4 5 6 2008 :timezone +utc-zone+) :use-zulu-p nil)

    "-06-05T04:03:02.000001"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t :date-elements 2)

    "-05T04:03:02.000001"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t :date-elements 1)

    "04:03:02.000001"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t :omit-date-part-p t)

    "04:03:02"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t :omit-date-part-p t :time-elements 3)

    "-0005-06-05T04:03:02.000001"
    (format-timestring (encode-local-time 1 2 3 4 5 6 -5) :omit-timezone-part-p t)

    ""
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-time-part-p t :omit-date-part-p t)

    "04"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t :omit-date-part-p t :time-elements 1)

    "04:03"
    (format-timestring (encode-local-time 1 2 3 4 5 6 2008) :omit-timezone-part-p t :omit-date-part-p t :time-elements 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test local-timezone
  ;; In 2005, April 4th is the start of daylight savings time.  The
  ;; difference between daylight savings and non-daylight savings
  ;; is one hour (for now)
  (is (= 3600
         (- (local-timezone (encode-local-time 0 0 0 0 4 4 2005 :timezone +utc-zone+))
            (local-timezone (encode-local-time 0 0 0 0 3 4 2005 :timezone +utc-zone+))))))

(test unix-time
  (is (eql 0 (unix-time (encode-local-time 0 0 0 0 1 1 1970)))))

(test universal-time
  (is (equal (values 2 3 4 5 6 2008 3 * *)
             (decode-universal-time (universal-time (encode-local-time 1 2 3 4 5 6 2008))))))
  
(test local-time
  (let ((now (now)))
    (setf (usec-of now) 0)
    (is (local-time= now
                     (local-time :unix (unix-time now)))))
  (let ((now (get-universal-time)))
    (is (equal now
               (universal-time (local-time :universal now))))))

(test year-difference
  (let ((a (parse-timestring "2006-01-01T00:00:00"))
        (b (parse-timestring "2001-01-01T00:00:00")))
    (is (= 5 (local-time-whole-year-difference a b))))

  (let ((a (parse-timestring "2006-01-01T00:00:00"))
        (b (parse-timestring "2001-01-02T00:00:00")))
    (is (= 4 (local-time-whole-year-difference a b)))))

(test adjust-days
  (let ((sunday (parse-timestring "2006-12-17T01:02:03Z")))
    (is (local-time= (parse-timestring "2006-12-11T01:02:03Z")
                     (local-time-adjust-days sunday :monday)))
    (is (local-time= (parse-timestring "2006-12-20T01:02:03Z")
                     (local-time-adjust-days sunday 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test parse-timestring
  (let ((local-time (now)))
    (is (local-time= local-time
                     (parse-timestring
                      (format-timestring local-time)))))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 0)))
    (is (local-time= local-time
                     (parse-timestring "0000-01-01T00:00:00,0"))))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 0 :timezone +utc-zone+)))
    (is (local-time= local-time
                     (parse-timestring "0000-01-01T00:00:00Z"))))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 2006)))
    (is (local-time= local-time
                     (parse-timestring "2006-01-01T00:00:00,0"))))
  (is (eql (day-of (encode-local-time 0 0 0 0 1 1 2006))
           (day-of (parse-timestring "xxxx 2006-01-01T00:00:00,0 xxxx"
                                     :start 5
                                     :end 15))))
  (is (eql (day-of (parse-timestring "2006-06-06TZ")) 2288))
  (is (local-time= (encode-local-time 20000 3 4 5 6 7 2008)
                   (parse-timestring "2008-07-06T05:04:03,02")))
  (is (local-time= (encode-local-time 0 02 0 0 23 3 2000)
                   (parse-timestring "--23T::02" :allow-missing-elements-p t)))
  (is (local-time= (encode-local-time 80000 7 6 5 1 3 2000)
                   (parse-timestring "T05:06:07,08"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test local-time-adjust
  (let ((utc-1 (local-time::make-timezone :subzones '((-3600 NIL "UTC-1" T NIL))
                                          :loaded t))
        (utc+1 (local-time::make-timezone :subzones '((+3600 NIL "UTC+1" T NIL))
                                          :loaded t))
        (epoch (local-time :unix 0 :timezone +utc-zone+)))
    (is (equal (decode-local-time (local-time-adjust epoch utc-1 (make-local-time)))
               ;; ms ss mm hh day mon year wday ds-p zone abbrev
               (values 00 00 00 23 31 12 1969 3 nil utc-1 "UTC-1")))
    (let ((local-time (local-time :unix 3600 :timezone +utc-zone+)))
      (is (equal (decode-local-time (local-time-adjust local-time utc-1 (make-local-time)))
                 ;; ms ss mm hh day mon year
                 (values 00 00 00 00 01 01 1970 4 nil utc-1 "UTC-1"))))
    (is (equal (decode-local-time (local-time-adjust epoch utc+1 (make-local-time)))
               ;; ms ss mm hh day mon year
               (values 00 00 00 01 01 01 1970 4 nil utc+1 "UTC+1")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test read-timestring
  (let ((now (now)))
    (is (local-time= now
                     (with-input-from-string (ins (format-timestring now))
                       (local-time::read-timestring ins #\@))))))

(test read-universal-time
  (let ((now (now)))
    (setf (usec-of now) 0)
    (is (local-time= now
                     (with-input-from-string (ins (princ-to-string (universal-time now)))
                       (local-time::read-universal-time ins #\@ nil))))))

(test leap-year-printing
  (let ((local-time (parse-timestring "2004-02-29")))
    (is (local-time= local-time (parse-timestring (format-timestring local-time))))))

(test decode-date
  (loop for (total-day year month day) :in '((-1 2000 02 29)
                                             (0 2000 03 01)
                                             (1 2000 03 02)
                                             (364 2001 02 28)
                                             (365 2001 03 01)
                                             (366 2001 03 02)
                                             (#.(* 2 365) 2002 03 01)
                                             (#.(* 4 365) 2004 02 29)
                                             (#.(1+ (* 4 365)) 2004 03 01))
        do (multiple-value-bind (year* month* day*)
               (local-time::local-time-decode-date (make-local-time :day total-day))
             (is (= year year*))
             (is (= month month*))
             (is (= day day*)))))

(defun valid-date-p (year month day)
  (let ((month-days #(31 28 31 30 31 30 31 31 30 31 30 31)))
    (and (<= 1 month 12)
         (<= 1 day (+ (aref month-days (1- month))
                      (if (and (= month 2)
                               (zerop (mod year 4)))
                          1
                          0))))))

(test encode-decode-local-time
  (let ((*default-timezone* +utc-zone+))
    (loop for year :in '(1900 1975 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010) do
          (loop for month :from 1 :to 12 do
                (loop for day :in '(1 2 3 27 28 29 30 31) do
                      (when (valid-date-p year month day)
                        (multiple-value-bind (usec sec minute hour day* month* year* day-of-week daylight-p zone)
                            (decode-local-time (encode-local-time 0 0 0 0 day month year))
                          (declare (ignore usec sec minute hour day-of-week daylight-p zone))
                          (is (= year year*))
                          (is (= month month*))
                          (is (= day day*)))))))))
