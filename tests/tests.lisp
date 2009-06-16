
(defpackage #:local-time.tests
  (:use :cl :local-time :5am))

(in-package #:local-time.tests)

(def-suite :local-time)

(in-suite* #:local-time.make :in :local-time)

(reread-timezone-repository)

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

(test read-binary-integer
  (let ((tmp-file-path #p"/tmp/local-time-test"))
    (with-open-file (ouf tmp-file-path
                         :direction :output
                         :element-type 'unsigned-byte
                         :if-exists :supersede)
      (dotimes (i 14)
        (write-byte 200 ouf)))
  (with-open-file (inf tmp-file-path :element-type 'unsigned-byte)
    (is (eql (local-time::%read-binary-integer inf 1) 200))
    (is (eql (local-time::%read-binary-integer inf 1 t) -56))
    (is (eql (local-time::%read-binary-integer inf 2) 51400))
    (is (eql (local-time::%read-binary-integer inf 2 t) -14136))
    (is (eql (local-time::%read-binary-integer inf 4) 3368601800))
    (is (eql (local-time::%read-binary-integer inf 4 t) -926365496)))))

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
  (encode-decode-test (5 5 5 5 5 5 1990 6 nil 0 "UTC"))
  (encode-decode-test (0 0 0 0 1 3 2001 4 nil 0 "UTC"))
  (encode-decode-test (0 0 0 0 1 3 1998 0 nil 0 "UTC"))
  (encode-decode-test (1 2 3 4 5 6 2008 4 nil 0 "UTC"))
  (encode-decode-test (0 0 0 0 1 1 1 1 nil 0 "UTC")))

(test random-decode-timestamp
  (let ((timestamp (make-timestamp
                     :day (- (random 65535) 36767)
                     :sec (random 86400)
                     :nsec (random 1000000000))))
    (multiple-value-bind (ns ss mm hh day mon year)
        (decode-timestamp timestamp :timezone local-time:+utc-zone+)
      (is (timestamp= timestamp
                      (encode-timestamp ns ss mm hh day mon year :offset 0))))))


(local-time::define-timezone eastern-tz
    (merge-pathnames #p"zoneinfo/EST5EDT" local-time::*project-home-directory*))

(test decode-timestamp-dst
  ;; Testing DST calculation with a known timezone
  (let ((test-cases '(
                      ;; Spring forward
                      ((2008 3 9 6 58) (2008 3 9 1 58))
                      ((2008 3 9 6 59) (2008 3 9 1 59))
                      ((2008 3 9 7  0) (2008 3 9 3  0))
                      ((2008 3 9 7  1) (2008 3 9 3  1))
                      ;; Fall back
                      ((2008 11 2 5 59) (2008 11 2 1 59))
                      ((2008 11 2 6  0) (2008 11 2 1  0))
                      ((2008 11 2 6  1) (2008 11 2 1  1)))))
    (dolist (test-case test-cases)
      (is (equal 
         (let ((timestamp
                (apply 'local-time:encode-timestamp
                       `(0 0 ,@(reverse (first test-case)) :offset 0))))
           (local-time:decode-timestamp timestamp :timezone eastern-tz))
         (apply 'values `(0 0 ,@(reverse (second test-case)))))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test format-timestring/simple
  (let ((local-time::*default-timezone* local-time:+utc-zone+)
        (test-timestamp (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0)))
    (is-every string=
      "2008-06-05T04:03:02.000001Z"
      (format-timestring nil test-timestamp)

      "2008-06-05T04:03:02.000001-05:00"
      (let ((utc-5 (local-time::%make-simple-timezone "UTC-5" "UTC-5" -18000)))
        (format-timestring nil (encode-timestamp 1000 2 3 4 5 6 2008
                                                 :offset (* 3600 -5))
                           :timezone utc-5))

      "Thu Jun  5 04:03:02 2008"
      (format-timestring nil test-timestamp :format +asctime-format+)

      "Thu, 05 Jun 2008 04:03:02 UTC"
      (format-timestring nil test-timestamp :format +rfc-1123-format+)

      ""
      (format-timestring nil test-timestamp
                         :format nil)

      "04"
      (format-timestring nil test-timestamp
                         :format '((:hour 2)))

      "04:03"
      (format-timestring nil test-timestamp
                         :format '((:hour 2) #\: (:min 2))))))

(test format-timestring/errors
  (with-output-to-string (*standard-output*)
    (let ((*default-timezone* (find-timezone-by-location-name "UTC")))
      (finishes (print (now))))))

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

(test adjust-timestamp/bug1
  (let* ((timestamp (parse-timestring "2006-01-01T00:00:00Z"))
         (modified-timestamp (adjust-timestamp timestamp (offset :year 1))))
    (timestamp= (parse-timestring "2007-01-01T00:00:00Z") modified-timestamp)))

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
                      (format-timestring nil timestamp)))))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 1)))
    (is (timestamp= timestamp
                     (parse-timestring "0001-01-01T00:00:00,0"
                                       :offset (local-time::%get-default-offset)))))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 1 :offset 0)))
    (is (timestamp= timestamp
                     (parse-timestring "0001-01-01T00:00:00Z"))))
  (let ((timestamp (encode-timestamp 0 0 0 0 1 1 2006)))
    (is (timestamp= timestamp
                     (parse-timestring "2006-01-01T00:00:00,0"
                                       :offset (local-time::%get-default-offset)))))
  (is (eql (day-of (encode-timestamp 0 0 0 0 1 1 2006))
           (day-of (parse-timestring "xxxx 2006-01-01T00:00:00,0 xxxx"
                                     :start 5
                                     :end 15
                                     :offset (local-time::%get-default-offset)))))
  (is (eql (day-of (parse-timestring "2006-06-06TZ")) 2288))
  (is (timestamp= (encode-timestamp 20000000 3 4 5 6 7 2008)
                   (parse-timestring "2008-07-06T05:04:03,02"
                                     :offset (local-time::%get-default-offset))))
  (is (timestamp= (encode-timestamp 0 02 0 0 23 3 2000)
                   (parse-timestring "--23T::02"
                                     :allow-missing-elements t
                                     :offset (local-time::%get-default-offset))))
  (is (timestamp= (encode-timestamp 80000000 7 6 5 1 3 2000)
                   (parse-timestring "T05:06:07,08"
                                     :offset (local-time::%get-default-offset))))
  (is (timestamp= (encode-timestamp 940703000 28 56 16 20 2 2008 :offset 0)
                   (parse-timestring "2008-02-20T16:56:28.940703Z"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test read-timestring
  (let ((now (now)))
    (setf (nsec-of now) 123456000)
    (is (timestamp= now
                     (with-input-from-string (ins (format-timestring nil now))
                       (local-time::%read-timestring ins #\@))))))

(test read-universal-time
  (let ((now (now)))
    (setf (nsec-of now) 0)
    (is (timestamp= now
                     (with-input-from-string (ins (princ-to-string (timestamp-to-universal now)))
                       (local-time::%read-universal-time ins #\@ nil))))))

(test leap-year-printing
  (let ((timestamp (parse-timestring "2004-02-29")))
    (is (timestamp= timestamp (parse-timestring (format-timestring nil timestamp))))))

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
               (local-time::%timestamp-decode-date total-day)
             (is (= year year*))
             (is (= month month*))
             (is (= day day*)))))

(test timestamp-decoding-readers
  (let ((*default-timezone* +utc-zone+))
    (dolist (year '(1900 1975 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010))
      (dolist (month '(1 2 3 4 5 6 7 8 9 10 11 12))
        (dolist (day '(1 2 3 27 28 29 30 31))
          (when (valid-date-p year month day)
            (let ((hour (random 24))
                  (min (random 60))
                  (sec (random 60))
                  (nsec (random 1000000000)))
              (let ((time (encode-timestamp nsec sec min hour day month year :offset 0)))
                (is (= (floor year 10) (timestamp-decade time)))
                (is (= year (timestamp-year time)))
                (is (= month (timestamp-month time)))
                (is (= day (timestamp-day time)))
                (is (= hour (timestamp-hour time)))
                (is (= min (timestamp-minute time)))
                (is (= sec (timestamp-second time)))
                (is (= (floor nsec 1000000)
                       (timestamp-millisecond time)))
                (is (= (floor nsec 1000)
                       (timestamp-microsecond time)))))))))))

(test timestamp-century
  (let ((*default-timezone* +utc-zone+))
    (dolist (year-data '((-101 -2)
                         (-100 -1)
                         (-1 -1)
                         (1 1)
                         (100 1)
                         (101 2)
                         (1999 20)
                         (2000 20)
                         (2001 21)))
      (let ((time (encode-timestamp 0 0 0 0 1 1 (first year-data) :offset 0)))
        (is (= (second year-data) (timestamp-century time)))))))

(test timestamp-millennium
  (let ((*default-timezone* +utc-zone+))
    (dolist (year-data '((-101 -1)
                         (-100 -1)
                         (-1 -1)
                         (1 1)
                         (100 1)
                         (101 1)
                         (1001 2)
                         (1999 2)
                         (2000 2)
                         (2001 3)))
      (let ((time (encode-timestamp 0 0 0 0 1 1 (first year-data) :offset 0)))
        (is (= (second year-data) (timestamp-millennium time)))))))

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

(defun test-parse/format-consistency (&key (start-day -100000) (end-day 100000))
  (declare (optimize debug))
  (loop
     with time = (make-timestamp)
     for day from start-day upto end-day
     for index upfrom 0 do
       (setf (day-of time) day)
       (when (zerop (mod index 1000))
         (print time))
       (let ((parsed (parse-timestring (format-timestring nil time))))
         (unless (timestamp= parsed time)
           (error "oops, mismatch: ~A ~A" parsed time)))))
