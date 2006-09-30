
(defpackage #:local-time.tests
  (:use :cl :local-time :5am))

(in-package #:local-time.tests)

(def-suite :local-time)

(in-suite* #:local-time.make :in :local-time)

(test make-local-time-1
  (let ((local-time (make-local-time :epoch-usec 1 :epoch-sec 2 :epoch-day 3 :timezone *default-timezone*)))
    (is-equal (epoch-usec-of local-time) 1
              (epoch-sec-of local-time) 2
              (epoch-day-of local-time) 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest make-local-time-1 ()
	(let ((local-time (make-local-time :epoch-usec 1 :epoch-sec 2 :epoch-day 3 :timezone *default-timezone*)))
	  (values
	   (epoch-usec-of local-time)
	   (epoch-sec-of local-time)
	   (epoch-day-of local-time)))
  1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time<-1 ()
	(local-time< (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0)
				 (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0))
  t)
(deftest local-time<-2 ()
	(local-time< (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0)
				 (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0))
  nil)
(deftest local-time<-3 ()
	(local-time< (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0)
				 (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0))
  t)
(deftest local-time<-4 ()
	(local-time< (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0)
				 (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0))
  nil)
(deftest local-time<-5 ()
	(local-time< (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1)
				 (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2))
  t)
(deftest local-time<-6 ()
	(local-time< (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2)
				 (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time<=-1 ()
	(local-time<= (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0)
				  (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0))
  t)
(deftest local-time<=-2 ()
	(local-time<= (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0)
				  (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0))
  nil)
(deftest local-time<=-3 ()
	(local-time<= (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0)
				  (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0))
  t)
(deftest local-time<=-4 ()
	(local-time<= (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0)
				  (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0))
  nil)
(deftest local-time<=-5 ()
	(local-time<= (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1)
				  (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2))
  t)
(deftest local-time<=-6 ()
	(local-time<= (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2)
				  (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1))
  nil)
(deftest local-time<=-7 ()
	(local-time<= (make-local-time :epoch-day 1 :epoch-sec 2 :epoch-usec 3)
				  (make-local-time :epoch-day 1 :epoch-sec 2 :epoch-usec 3))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time>-1 ()
	(local-time> (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0)
				 (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0))
  t)
(deftest local-time>-2 ()
	(local-time> (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0)
				 (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0))
  nil)
(deftest local-time>-3 ()
	(local-time> (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0)
				 (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0))
  t)
(deftest local-time>-4 ()
	(local-time> (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0)
				 (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0))
  nil)
(deftest local-time>-5 ()
	(local-time> (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2)
				 (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1))
  t)
(deftest local-time>-6 ()
	(local-time> (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1)
				 (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time>=-1 ()
	(local-time>= (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0)
				  (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0))
  t)
(deftest local-time>=-2 ()
	(local-time>= (make-local-time :epoch-day 1 :epoch-sec 0 :epoch-usec 0)
				  (make-local-time :epoch-day 2 :epoch-sec 0 :epoch-usec 0))
  nil)
(deftest local-time>=-3 ()
	(local-time>= (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0)
				  (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0))
  t)
(deftest local-time>=-4 ()
	(local-time>= (make-local-time :epoch-day 0 :epoch-sec 1 :epoch-usec 0)
				  (make-local-time :epoch-day 0 :epoch-sec 2 :epoch-usec 0))
  nil)
(deftest local-time>=-5 ()
	(local-time>= (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2)
				  (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1))
  t)
(deftest local-time>=-6 ()
	(local-time>= (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 1)
				  (make-local-time :epoch-day 0 :epoch-sec 0 :epoch-usec 2))
  nil)
(deftest local-time>=-7 ()
	(local-time>= (make-local-time :epoch-day 1 :epoch-sec 2 :epoch-usec 3)
				  (make-local-time :epoch-day 1 :epoch-sec 2 :epoch-usec 3))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time=-1 ()
	(local-time= (make-local-time) (make-local-time))
  t)
(deftest local-time=-2 ()
	(local-time= (make-local-time) (make-local-time :epoch-usec 1))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time/=-1 ()
	(local-time/= (make-local-time) (make-local-time))
  nil)
(deftest local-time/=-2 ()
	(local-time/= (make-local-time) (make-local-time :epoch-usec 1))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest encode-local-time-1 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 3 2000)))
	  (values
	   (epoch-usec-of local-time)
	   (epoch-sec-of local-time)
	   (epoch-day-of local-time)))
  0 0 0)

(deftest encode-local-time-2 ()
	(let ((local-time (encode-local-time 0 0 0 0 29 2 2000)))
	  (values
	   (epoch-usec-of local-time)
	   (epoch-sec-of local-time)
	   (epoch-day-of local-time)))
  0 0 -1)

(deftest encode-local-time-3 ()
	(let ((local-time (encode-local-time 0 0 0 0 2 3 2000)))
	  (values
	   (epoch-usec-of local-time)
	   (epoch-sec-of local-time)
	   (epoch-day-of local-time)))
  0 0 1)

(deftest encode-local-time-4 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 1 2000)))
	  (values
	   (epoch-usec-of local-time)
	   (epoch-sec-of local-time)
	   (epoch-day-of local-time)))
  0 0 -60)

(deftest encode-local-time-5 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 3 2001)))
	  (values
	   (epoch-usec-of local-time)
	   (epoch-sec-of local-time)
	   (epoch-day-of local-time)))
  0 0 365)

(test local-time=-1
  (is (local-time= (make-local-time) (make-local-time)))
  (is-false (local-time= (make-local-time) (make-local-time :epoch-usec 1))))

(test local-time/=
  (is (local-time/= (make-local-time) (make-local-time :epoch-usec 1)))
  (is-false (local-time/= (make-local-time) (make-local-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test encode-local-time
  (let ((local-time (encode-local-time 0 0 0 0 1 3 2000)))
    (is-equal (epoch-usec-of local-time) 0
              (epoch-day-of local-time) 0
              (epoch-sec-of local-time) 0))
  (let ((local-time (encode-local-time 0 0 0 0 29 2 2000)))
    (is-equal (epoch-usec-of local-time) 0
              (epoch-day-of local-time) -1
              (epoch-sec-of local-time) 0))
  (let ((local-time (encode-local-time 0 0 0 0 2 3 2000)))
    (is-equal (epoch-usec-of local-time) 0
              (epoch-sec-of local-time) 0
              (epoch-day-of local-time) 1))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 2000)))
    (is-equal (epoch-usec-of local-time) 0
              (epoch-sec-of local-time) 0
              (epoch-day-of local-time) -60))
  (let ((local-time (encode-local-time 0 0 0 0 1 3 2001)))
    (is-equal (epoch-usec-of local-time) 0
              (epoch-sec-of local-time) 0
              (epoch-day-of local-time) 365)))

(deftest decode-local-time-5 ((local-time (make-local-time
                                           :epoch-day (- (random 65535) 36767)
                                           :epoch-sec (random 86400)
                                           :epoch-usec (random 1000))))
  (multiple-value-bind (ms ss mm hh day mon year)
      (decode-local-time local-time)
    (local-time= local-time (encode-local-time ms ss mm hh day mon year)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test format-timestring
  (is-string= (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil)
              "2008-06-05T04:03:02.000001"

              ;; This test only works on CDT (so far)
              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil t)
              "2008-06-05T04:03:02.000001-5:00"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008 +utc-zone+) t t)
              "2008-06-05T04:03:02.000001+0:00"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 2)
              "-06-05T04:03:02.000001"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 1)
              "-05T04:03:02.000001"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0)
              "04:03:02.000001"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 3)
              "04:03:02"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 -5) nil nil)
              "-0005-06-05T04:03:02.000001"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 0)
              ""

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 1)
              "04"

              (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 2)
              "04:03"))

(deftest parse-timestring-4 ()
  (epoch-day-of (parse-timestring "xxxx 2006-01-01T00:00:00,0 xxxx"
                                    :start 5
                                    :end 15))
  (epoch-day-of (encode-local-time 0 0 0 0 1 1 2006)))

(test local-timezone
    ;; In 2005, April 4th is the start of daylight savings time.  The
    ;; difference between daylight savings and non-daylight savings
    ;; is one hour (for now)
  (is-equal (- (local-timezone (encode-local-time 0 0 0 0 4 4 2005 +utc-zone+))
               (local-timezone (encode-local-time 0 0 0 0 3 4 2005 +utc-zone+)))
            3600))

(test unix-time
  (is-equal (unix-time (encode-local-time 0 0 0 0 1 1 1970)) 0))

(test universal-time
  (is-equal (decode-universal-time (universal-time (encode-local-time 1 2 3 4 5 6 2008)))
            (values 2 3 4 5 6 2008 3 * *)))
  
(test local-time
  (let ((now (now)))
    (is (local-time= (local-time :unix (unix-time now))
                     now)))
  (let ((now (get-universal-time)))
    (is-equal (universal-time (local-time :universal now))
              now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test parse-timestring
  (let ((local-time (now)))
    (is (local-time= (parse-timestring
                      (format-timestring nil local-time nil nil))
                     local-time)))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 0)))
    (is (local-time= (parse-timestring "0000-01-01T00:00:00,0")
                     local-time)))
  (let ((local-time (encode-local-time 0 0 0 0 1 1 2006)))
    (is (local-time= (parse-timestring "2006-01-01T00:00:00,0")
                     local-time)))
  (is-equal (epoch-day-of (parse-timestring "xxxx 2006-01-01T00:00:00,0 xxxx"
                                            :start 5
                                            :end 15))
            (epoch-day-of (encode-local-time 0 0 0 0 1 1 2006)))
  (is (local-time= (parse-timestring "2008-07-06T05:04:03,02")
                   (encode-local-time 20000 3 4 5 6 7 2008)))
  (is (local-time= (parse-timestring "--23T::02")
                   (multiple-value-bind (ms ss mm hh day mon year)
                       (decode-local-time (now))
                     (declare (ignore ss day))
                     (encode-local-time ms 02 mm hh 23 mon year))))
  (is (local-time= (parse-timestring "T05:06:07,08")
                   (multiple-value-bind (ms ss mm hh day mon year)
                       (decode-local-time (now))
                     (declare (ignore ms ss mm hh))
                     (encode-local-time 80000 7 6 5 day mon year)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test local-time-adjust
  (let ((utc-1 (local-time::make-timezone :subzones '((-3600 NIL "UTC-1" T NIL))
                                          :loaded t))
        (utc+1 (local-time::make-timezone :subzones '((+3600 NIL "UTC+1" T NIL))
                                          :loaded t))
        (epoch (local-time :unix 0 :timezone +utc-zone+)))
    (is-equal (decode-local-time (local-time-adjust epoch utc-1 (make-local-time)))
              ;; ms ss mm hh day mon year wday ds-p zone abbrev
              (values 00 00 00 23 31 12 1969 3 nil utc-1 "UTC-1"))
    (let ((local-time (local-time :unix 3600 :timezone +utc-zone+)))
      (is-equal (decode-local-time (local-time-adjust local-time utc-1 (make-local-time)))
                ;; ms ss mm hh day mon year
                (values 00 00 00 00 01 01 1970 4 nil utc-1 "UTC-1")))
    (is-equal (decode-local-time (local-time-adjust epoch utc+1 (make-local-time)))
              ;; ms ss mm hh day mon year
              (values 00 00 00 01 01 01 1970 4 nil utc+1 "UTC+1"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest read-timestring-1 ((now (now)))
  (with-input-from-string (ins (format-timestring nil now nil nil))
    (read-timestring ins #\@))
  now)

(deftest read-universal-time-1 ((now (now)))
  (with-input-from-string (ins (format nil "~a" (universal-time now)))
    (read-universal-time ins #\@ nil))
  now)

