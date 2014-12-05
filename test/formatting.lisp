(in-package #:local-time.test)

(defsuite* (formatting :in test))

(deftest test/formatting/format-timestring/1 ()
  (let ((*default-timezone* local-time:+utc-zone+)
        (test-timestamp (encode-timestamp 1000 2 3 4 5 6 2008 :offset 0)))
    (macrolet ((frob (&rest args)
                 `(progn
                    ,@(loop
                        :for (a b) :on args :by #'cddr
                        :collect `(is (string= ,a ,b))))))
      (frob
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
                          :format '((:hour 2) #\: (:min 2)))

       "5th"
       (format-timestring nil test-timestamp
                          :format '(:ordinal-day))))))

(deftest test/formatting/format-timestring/2 ()
  (with-output-to-string (*standard-output*)
    (let ((*default-timezone* (find-timezone-by-location-name "UTC")))
      (finishes (print (now))))))

(deftest test/formatting/ordinals ()
  (flet ((format-ordinal (day)
           (format-timestring nil (encode-timestamp 0 0 0 0 day 1 2008)
                              :format '(:ordinal-day))))
    (string= "31st" (format-ordinal 31))
    (string= "11th" (format-ordinal 11))
    (string= "22nd" (format-ordinal 22))
    (string= "3rd"  (format-ordinal 3))))

(deftest test/formatting/bug/1 ()
  (let ((*default-timezone* (find-timezone-by-location-name "Pacific/Auckland")))
    (finishes (format-timestring nil (now)))))

(deftest test/formatting/leap-year ()
  (let ((timestamp (parse-timestring "2004-02-29")))
    (is (timestamp= timestamp (parse-timestring (format-timestring nil timestamp))))))
