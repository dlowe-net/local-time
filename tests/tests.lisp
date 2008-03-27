
(defpackage #:local-time.tests
  (:use :cl :local-time :5am))

(in-package #:local-time.tests)

(def-suite :local-time)

(in-suite* #:local-time.make :in :local-time)

(test make-timestamp
  (let ((timestamp (make-timestamp :nsec 1 :sec 2 :day 3)))
    (is-every =
      (nsec-of timestamp) 1
      (sec-of timestamp) 2
      (day-of timestamp) 3)))

(defmacro defcmptest (compare-with &body args)
  `(test ,compare-with
    (flet ((make (day &optional (sec 0) (nsec 0))
             (make-timestamp :day day :sec sec :nsec nsec)))
      ,@(loop for entry in args
              if (= (length entry) 1)
              do (push 'is entry)
              else do (setf (car entry) (if (member (car entry) '(t true is is-true) :test #'eq)
                                            'is
                                            'is-false))
              collect `(,(first entry) (,compare-with (make ,@(second entry))
                                        (make ,@(third entry))))))))
(defcmptest timestamp<
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

(defcmptest timestamp<=
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

(defcmptest timestamp>
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

(defcmptest timestamp>=
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

(defcmptest timestamp=
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

(test timestamp=-1
  (is (timestamp= (make-timestamp) (make-timestamp)))
  (is-false (timestamp= (make-timestamp) (make-timestamp :nsec 1))))

(test timestamp/=
  (is (timestamp/= (make-timestamp) (make-timestamp :nsec 1)))
  (is-false (timestamp/= (make-timestamp) (make-timestamp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test encode-timestamp
  (let ((timestamp (encode-timestamp 0 0 0 0 1 3 2000 :offset 0)))
    (is-every =
      (nsec-of timestamp) 0
      (day-of timestamp) 0
      (sec-of timestamp) 0))
  (let ((timestamp (encode-timestamp 0 0 0 0 29 2 2000 :offset 0)))
    (is-every =
      (nsec-of timestamp) 0
      (day-of timestamp) -1
      (sec-of timestamp) 0))
  (let ((timestamp (encode-timestamp 0 0 0 0 2 3 2000 :offset 0)))
    (is-every =
      (nsec-of timestamp) 0
      (sec-of timestamp) 0
      (day-of timestamp) 1))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 2000 :offset 0)))
    (is-every =
      (nsec-of timestamp) 0
      (sec-of timestamp) 0
      (day-of timestamp) -60))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 3 2001 :offset 0)))
    (is-every =
      (nsec-of timestamp) 0
      (sec-of timestamp) 0
      (day-of timestamp) 365)))

(defmacro encode-decode-test (args &body body)
  `(let ((timestamp (encode-timestamp ,@(subseq args 0 7) :offset 0)))
    (is (equal (values ,@args ,@(let ((stars nil))
                                     (dotimes (n (- 7 (length args)))
                                       (push '* stars))
                                     stars))
               (decode-timestamp timestamp :timezone local-time:+utc-zone+)))
    ,@body))

(test decode-timestamp
  (encode-decode-test (5 5 5 5 5 5 1990 6))
  (encode-decode-test (0 0 0 0 1 3 2001 4))
  (encode-decode-test (0 0 0 0 1 3 1998 0))
  (encode-decode-test (1 2 3 4 5 6 2008 4))
  (encode-decode-test (0 0 0 0 1 1 1 1)))

(test random-decode-timestamp
  (let ((timestamp (make-timestamp
                     :day (- (random 65535) 36767)
                     :sec (random 86400)
                     :nsec (random 1000000000))))
    (multiple-value-bind (ns ss mm hh day mon year)
        (decode-timestamp timestamp :timezone local-time:+utc-zone+)
      (is (timestamp= timestamp
                      (encode-timestamp ns ss mm hh day mon year :offset 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test format-timestring
  (let ((local-time::*default-timezone* local-time::+utc-zone+))
    (is-every string=
      "2008-06-05T04:03:02.000001"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t)

      "2008-06-05T04:03:02.000001-05:00"

      (let ((utc-5 (local-time::make-timezone :transitions nil
                                              :subzones '((-18000 nil "UTC-5"))
                                              :leap-seconds nil
                                              :path nil
                                              :name "UTC-5"
                                              :loaded t)))
        (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008
                                             :offset (* 3600 -5))
                           :timezone utc-5))

      "2008-06-05T04:03:02.000001Z"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0)
                         :timezone local-time::+utc-zone+
                         :use-zulu-p t)

      ;; note: nsec overflows here
      "2008-06-05T04:03:03.234567+00:00"
      (format-timestring (encode-timestamp 1234567890 2 3 4 5 6 2008 :offset 0) :use-zulu-p nil)

      "-06-05T04:03:02.000001"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t :date-elements 2)

      "-05T04:03:02.000001"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t :date-elements 1)

      "04:03:02.000001"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t :omit-date-part-p t)

      "04:03:02"
      (format-timestring (encode-timestamp 1 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t :omit-date-part-p t :time-elements 3)

      "-0005-06-05T04:03:02.000001"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 -5 :offset 0) :omit-timezone-part-p t)

      ""
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-time-part-p t :omit-date-part-p t)

      "04"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t :omit-date-part-p t :time-elements 1)

      "04:03"
      (format-timestring (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0) :omit-timezone-part-p t :omit-date-part-p t :time-elements 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(test timestamp-conversions
  (is (eql 0 (timestamp-to-unix
              (encode-timestamp 0 0 0 0 1 1 1970 :offset 0))))
  (is (equal (values 2 3 4 5 6 2008 3 * *)
             (decode-universal-time
              (timestamp-to-universal
               (encode-timestamp 1 2 3 4 5 6 2008 :offset 0)) 0)))
  (let ((now (now)))
    (setf (nsec-of now) 0)
    (is (timestamp= now
                     (unix-to-timestamp (timestamp-to-unix now)))))
  (let ((now (get-universal-time)))
    (is (equal now
               (timestamp-to-universal (universal-to-timestamp now))))))

(test year-difference
  (let ((a (parse-timestring "2006-01-01T00:00:00"))
        (b (parse-timestring "2001-01-01T00:00:00")))
    (is (= 5 (timestamp-whole-year-difference a b))))

  (let ((a (parse-timestring "2006-01-01T00:00:00"))
        (b (parse-timestring "2001-01-02T00:00:00")))
    (is (= 4 (timestamp-whole-year-difference a b)))))

#+nil
(test adjust-days
  (let ((sunday (parse-timestring "2006-12-17T01:02:03Z")))
    (is (timestamp= (parse-timestring "2006-12-11T01:02:03Z")
                     (adjust-timestamp sunday (offset :day-of-week :monday))))
    (is (timestamp= (parse-timestring "2006-12-20T01:02:03Z")
                     (adjust-timestamp sunday (offset :day 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test parse-timestring
  (let ((timestamp (now)))
    (is (timestamp= timestamp
                     (parse-timestring
                      (format-timestring timestamp)))))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 0)))
    (is (timestamp= timestamp
                     (parse-timestring "0000-01-01T00:00:00,0"))))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 0 :offset 0)))
    (is (timestamp= timestamp
                     (parse-timestring "0000-01-01T00:00:00Z"))))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 2006)))
    (is (timestamp= timestamp
                     (parse-timestring "2006-01-01T00:00:00,0"))))
  (is (eql (day-of (encode-timestamp 0 0 0 0 1 1 2006))
           (day-of (parse-timestring "xxxx 2006-01-01T00:00:00,0 xxxx"
                                     :start 5
                                     :end 15))))
  (is (eql (day-of (parse-timestring "2006-06-06TZ")) 2288))
  (is (timestamp= (encode-timestamp 20000000 3 4 5 6 7 2008)
                   (parse-timestring "2008-07-06T05:04:03,02")))
  (is (timestamp= (encode-timestamp 0 02 0 0 23 3 2000)
                   (parse-timestring "--23T::02" :allow-missing-elements-p t)))
  (is (timestamp= (encode-timestamp 80000000 7 6 5 1 3 2000)
                   (parse-timestring "T05:06:07,08")))
  (is (timestamp= (encode-timestamp 940703000 28 56 16 20 2 2008 :offset 0)
                   (parse-timestring "2008-02-20T16:56:28.940703Z"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test read-timestring
  (let ((now (now)))
    (is (timestamp= now
                     (with-input-from-string (ins (format-timestring now))
                       (local-time::read-timestring ins #\@))))))

(test read-universal-time
  (let ((now (now)))
    (setf (nsec-of now) 0)
    (is (timestamp= now
                     (with-input-from-string (ins (princ-to-string (timestamp-to-universal now)))
                       (local-time::read-universal-time ins #\@ nil))))))

(test leap-year-printing
  (let ((timestamp (parse-timestring "2004-02-29")))
    (is (timestamp= timestamp (parse-timestring (format-timestring timestamp))))))

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
               (local-time::timestamp-decode-date total-day)
             (is (= year year*))
             (is (= month month*))
             (is (= day day*)))))

(defun valid-date-p (year month day)
  ;; it works only on the gregorian calendar
  (let ((month-days #(31 28 31 30 31 30 31 31 30 31 30 31)))
    (and (<= 1 month 12)
         (<= 1 day (+ (aref month-days (1- month))
                      (if (and (= month 2)
                               (zerop (mod year 4))
                               (not (zerop (mod year 100)))
                               (zerop (mod year 400)))
                          1
                          0))))))

(test encode-decode-timestamp
  (let ((*default-timezone* +utc-zone+))
    (loop for year :in '(1900 1975 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010) do
          (loop for month :from 1 :to 12 do
                (loop for day :in '(1 2 3 27 28 29 30 31) do
                      (when (valid-date-p year month day)
                        (multiple-value-bind (nsec sec minute hour day* month* year* day-of-week)
                            (decode-timestamp (encode-timestamp 0 0 0 0 day month year :offset 0))
                          (declare (ignore nsec sec minute day-of-week))
                          (is (= hour 0))
                          (is (= year year*))
                          (is (= month month*))
                          (is (= day day*)))))))))

(test timestamp-uses-nsec
      (let ((universal-time (universal-to-timestamp (get-universal-time)
                                                    :nsec 123456789))
            (unix-time (unix-to-timestamp 0 :nsec 123456789))
            (now-time (now :nsec 123456789)))
        (is (= (nsec-of universal-time) 123456789))
        (is (= (nsec-of unix-time) 123456789))
        (is (= (nsec-of now-time) 123456789))))

(defun test-parse/format-consistency (&key (start-day -100000) (end-day 100000))
  (declare (optimize debug))
  (loop
     with time = (make-timestamp)
     for day from start-day upto end-day
     for index upfrom 0 do
       (setf (day-of time) day)
       (when (zerop (mod index 1000))
         (print time))
       (let ((parsed (parse-timestring (format-timestring time))))
         (unless (timestamp= parsed time)
           (error "oops, mismatch: ~A ~A" parsed time)))))
