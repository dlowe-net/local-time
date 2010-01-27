(in-package #:local-time.test)

(defsuite* (parsing :in test))

(deftest test/parsing/parse-format-consistency/range (&key (start-day -100000) (end-day 100000))
  (declare (optimize debug))
  (without-test-progress-printing
    (loop
      :with time = (make-timestamp)
      :for day :from start-day :upto end-day
      :for index :upfrom 0 :do
        (setf (day-of time) day)
        (when (zerop (mod index 10000))
          (print time))
        (let ((parsed (parse-timestring (format-timestring nil time))))
          (is (timestamp= parsed time))))))

(deftest test/parsing/bug/1 ()
  ;; This test depends on the clock of the machine, but as of writing,
  ;; this test fails, 2009.  oct. 29. (non-summer time).
  ;; FIXME attila: this test is probably wrong, because (local-time::%get-default-offset)
  ;; returns the default offset based on the _current time_ of the computer, whereas
  ;; ENCODE-TIMESTAMP scans the timezone definition.
  ;; this test fails for me in october (no daylight saving) in CET (Europe/Budapest, +01:00)
  (let* ((*default-timezone* (find-timezone-by-location-name "Europe/Budapest"))
         (timestamp (encode-timestamp 0 0 0 0 1 1 1)))
    (is (timestamp= timestamp
                    (parse-timestring "0001-01-01T00:00:00,0"
                                      :offset (local-time::%get-default-offset))))))

(deftest test/parsing/parse-format-consistency ()
  (let ((timestamp (now)))
    (is (timestamp= timestamp
                    (parse-timestring
                     (format-timestring nil timestamp)))))
  ;; FIXME see comment at parse-timestring/bug/1; it applies to most of the asserts below...
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

(deftest test/parsing/reader ()
  (let ((now (now)))
    (setf (nsec-of now) 123456000)
    (is (timestamp= now
                     (with-input-from-string (ins (format-timestring nil now))
                       (local-time::%read-timestring ins #\@))))))

(deftest test/parsing/read-universal-time ()
  (let ((now (now)))
    (setf (nsec-of now) 0)
    (is (timestamp= now
                     (with-input-from-string (ins (princ-to-string (timestamp-to-universal now)))
                       (local-time::%read-universal-time ins #\@ nil))))))

