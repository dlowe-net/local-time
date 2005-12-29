(in-package local-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-funcs* (make-hash-table :test 'equal))
(defvar *test-values* (make-hash-table :test 'equal))

(defmacro deftest (name form &rest values)
  (when (null values)
	(error "Test must specify at least one expected value."))
  `(setf (gethash ',name *test-funcs*) (compile nil (lambda () ,form))
		 (gethash ',name *test-values*) ',values))

(defun run-tests (&key (noisy nil))
  (let ((total 0) (passed 0) (last-err nil))
	(maphash
	 (lambda (name func)
	   (incf total)
	   (let ((result (handler-case
					   (multiple-value-list (funcall func))
					   (error (str)
						 (setf last-err str)
						 nil))))
		 (cond
		   ((null result)
			(format t "Test ~a failed due to error: ~a~%" name last-err))
		   ((equalp result (gethash name *test-values*))
			(incf passed)
			(when noisy
			  (format t "Test ~a passed.~%" name)))
		   (t
			(format t "Test ~a failed!~%Expected values: ~a~%Actual values: ~a~%"
					name
					(gethash name *test-values*)
					result)))))
	 *test-funcs*)
	(format t "~d/~d tests passed.~%" passed total)))

(defun reset-tests ()
  (clrhash *test-funcs*)
  (clrhash *test-values*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest make-local-time-1
	(let ((local-time (make-local-time :msec 1 :sec 2 :day 3 :zone *default-timezone*)))
	  (values
	   (local-time-msec local-time)
	   (local-time-sec local-time)
	   (local-time-day local-time)))
  1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time<-1
	(local-time< (make-local-time :day 1 :sec 0 :msec 0)
				 (make-local-time :day 2 :sec 0 :msec 0))
  t)
(deftest local-time<-2
	(local-time< (make-local-time :day 2 :sec 0 :msec 0)
				 (make-local-time :day 1 :sec 0 :msec 0))
  nil)
(deftest local-time<-3
	(local-time< (make-local-time :day 0 :sec 1 :msec 0)
				 (make-local-time :day 0 :sec 2 :msec 0))
  t)
(deftest local-time<-4
	(local-time< (make-local-time :day 0 :sec 2 :msec 0)
				 (make-local-time :day 0 :sec 1 :msec 0))
  nil)
(deftest local-time<-5
	(local-time< (make-local-time :day 0 :sec 0 :msec 1)
				 (make-local-time :day 0 :sec 0 :msec 2))
  t)
(deftest local-time<-6
	(local-time< (make-local-time :day 0 :sec 0 :msec 2)
				 (make-local-time :day 0 :sec 0 :msec 1))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time<=-1
	(local-time<= (make-local-time :day 1 :sec 0 :msec 0)
				  (make-local-time :day 2 :sec 0 :msec 0))
  t)
(deftest local-time<=-2
	(local-time<= (make-local-time :day 2 :sec 0 :msec 0)
				  (make-local-time :day 1 :sec 0 :msec 0))
  nil)
(deftest local-time<=-3
	(local-time<= (make-local-time :day 0 :sec 1 :msec 0)
				  (make-local-time :day 0 :sec 2 :msec 0))
  t)
(deftest local-time<=-4
	(local-time<= (make-local-time :day 0 :sec 2 :msec 0)
				  (make-local-time :day 0 :sec 1 :msec 0))
  nil)
(deftest local-time<=-5
	(local-time<= (make-local-time :day 0 :sec 0 :msec 1)
				  (make-local-time :day 0 :sec 0 :msec 2))
  t)
(deftest local-time<=-6
	(local-time<= (make-local-time :day 0 :sec 0 :msec 2)
				  (make-local-time :day 0 :sec 0 :msec 1))
  nil)
(deftest local-time<=-7
	(local-time<= (make-local-time :day 1 :sec 2 :msec 3)
				  (make-local-time :day 1 :sec 2 :msec 3))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time>-1
	(local-time> (make-local-time :day 2 :sec 0 :msec 0)
				 (make-local-time :day 1 :sec 0 :msec 0))
  t)
(deftest local-time>-2
	(local-time> (make-local-time :day 1 :sec 0 :msec 0)
				 (make-local-time :day 2 :sec 0 :msec 0))
  nil)
(deftest local-time>-3
	(local-time> (make-local-time :day 0 :sec 2 :msec 0)
				 (make-local-time :day 0 :sec 1 :msec 0))
  t)
(deftest local-time>-4
	(local-time> (make-local-time :day 0 :sec 1 :msec 0)
				 (make-local-time :day 0 :sec 2 :msec 0))
  nil)
(deftest local-time>-5
	(local-time> (make-local-time :day 0 :sec 0 :msec 2)
				 (make-local-time :day 0 :sec 0 :msec 1))
  t)
(deftest local-time>-6
	(local-time> (make-local-time :day 0 :sec 0 :msec 1)
				 (make-local-time :day 0 :sec 0 :msec 2))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time>=-1
	(local-time>= (make-local-time :day 2 :sec 0 :msec 0)
				  (make-local-time :day 1 :sec 0 :msec 0))
  t)
(deftest local-time>=-2
	(local-time>= (make-local-time :day 1 :sec 0 :msec 0)
				  (make-local-time :day 2 :sec 0 :msec 0))
  nil)
(deftest local-time>=-3
	(local-time>= (make-local-time :day 0 :sec 2 :msec 0)
				  (make-local-time :day 0 :sec 1 :msec 0))
  t)
(deftest local-time>=-4
	(local-time>= (make-local-time :day 0 :sec 1 :msec 0)
				  (make-local-time :day 0 :sec 2 :msec 0))
  nil)
(deftest local-time>=-5
	(local-time>= (make-local-time :day 0 :sec 0 :msec 2)
				  (make-local-time :day 0 :sec 0 :msec 1))
  t)
(deftest local-time>=-6
	(local-time>= (make-local-time :day 0 :sec 0 :msec 1)
				  (make-local-time :day 0 :sec 0 :msec 2))
  nil)
(deftest local-time>=-7
	(local-time>= (make-local-time :day 1 :sec 2 :msec 3)
				  (make-local-time :day 1 :sec 2 :msec 3))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time=-1
	(local-time= (make-local-time) (make-local-time))
  t)
(deftest local-time=-2
	(local-time= (make-local-time) (make-local-time :msec 1))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time/=-1
	(local-time/= (make-local-time) (make-local-time))
  nil)
(deftest local-time/=-2
	(local-time/= (make-local-time) (make-local-time :msec 1))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest encode-local-time-1
	(let ((local-time (encode-local-time 0 0 0 0 1 3 2000)))
	  (values
	   (local-time-msec local-time)
	   (local-time-sec local-time)
	   (local-time-day local-time)))
  0 0 0)

(deftest encode-local-time-2
	(let ((local-time (encode-local-time 0 0 0 0 29 2 2000)))
	  (values
	   (local-time-msec local-time)
	   (local-time-sec local-time)
	   (local-time-day local-time)))
  0 0 -1)

(deftest encode-local-time-3
	(let ((local-time (encode-local-time 0 0 0 0 2 3 2000)))
	  (values
	   (local-time-msec local-time)
	   (local-time-sec local-time)
	   (local-time-day local-time)))
  0 0 1)

(deftest encode-local-time-4
	(let ((local-time (encode-local-time 0 0 0 0 1 1 2000)))
	  (values
	   (local-time-msec local-time)
	   (local-time-sec local-time)
	   (local-time-day local-time)))
  0 0 -60)

(deftest decode-local-time-1
	(let ((local-time (encode-local-time 1 2 3 4 5 6 2008)))
	  (equal (multiple-value-list (decode-local-time local-time))
			 `(1 2 3 4 5 6 2008 4
				 ,*default-timezone*
				 ,(nth-value 2 (timezone local-time)))))
  t)

(deftest decode-local-time-2
	(let ((local-time (encode-local-time 0 0 0 0 1 1 0)))
	  (equal (multiple-value-list (decode-local-time local-time))
			 `(0 0 0 0 1 1 0 5
               ,*default-timezone*
               ,(nth-value 2 (timezone local-time)))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest format-timestring-1
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil)
  "2008-06-05T04:03:02,000001")
(deftest format-timestring-2
    ;; This test only works on CDT (so far)
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil t)
  "2008-06-05T04:03:02,000001-5:00")
(deftest format-timestring-3
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008 +utc-zone+) t t)
  "2008-06-05T04:03:02,000001+0:00")
(deftest format-timestring-4
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 2)
  "-06-05T04:03:02,000001")
(deftest format-timestring-5
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 1)
  "-05T04:03:02,000001")
(deftest format-timestring-6
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0)
  "04:03:02,000001")
(deftest format-timestring-7
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 3)
  "04:03:02")
(deftest format-timestring-8
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 2)
  "04:03")
(deftest format-timestring-8
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 1)
  "04")
(deftest format-timestring-8
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 0)
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest local-timezone-1
    ;; In 2005, April 4th is the start of daylight savings time.  The
    ;; difference between daylight savings and non-daylight savings
    ;; is one hour (for now)
    (- (local-timezone (encode-local-time 0 0 0 0 4 4 2005))
       (local-timezone (encode-local-time 0 0 0 0 3 4 2005)))
  3600)

(deftest unix-time-1
    (unix-time (encode-local-time 0 0 0 0 1 1 1970))
  0)
(deftest universal-time-1
    (decode-universal-time (universal-time (encode-local-time 1 2 3 4 5 6 2008)))
  2 3 4 5 6 2008 3 t 6)
  
(deftest local-time-1
    (let ((now (now)))
      (local-time= (local-time :unix (unix-time now))
                   now))
  t)
(deftest local-time-2
    (let ((now (get-universal-time)))
      (eql (universal-time (local-time :universal now))
           now))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftest parse-timestring-1
    (let ((local-time (now)))
      (local-time= (parse-timestring
                    (format-timestring nil local-time nil nil))
                   local-time))
  t)

(deftest parse-timestring-2
    (let ((local-time (encode-local-time 0 0 0 0 1 1 0)))
      (local-time= (parse-timestring "0000-01-01T00:00:00,0")
                   local-time))
  t)

(deftest parse-timestring-3
    (let ((local-time (encode-local-time 0 0 0 0 1 1 2006)))
      (local-time= (parse-timestring "2006-01-01T00:00:00,0")
                   local-time))
  t)

(run-tests)
