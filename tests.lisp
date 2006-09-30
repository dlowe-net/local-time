(in-package :local-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-funcs* (make-hash-table :test 'equal))

(defmacro deftest (name vars test-form &rest result-forms)
  "Defines a test NAME which tests TEST-FORM with VARS defined, and compare the result to a number of RESULT-FORMS.  If no RESULT-FORMS are given, the test is assumed to succeed if it returns non-NIL."
  (let ((result-sym (gensym "RESULT"))
        (noisy-sym (gensym "NOISY"))
        (expected-forms (or result-forms '(t))))
    `(setf (gethash ',name *test-funcs*)
      (compile nil
       (lambda (,noisy-sym)
         (let* (,@vars
                (,result-sym (multiple-value-list ,test-form)))
           ;; Ensure that there are enough result values
           (when (< (length ,result-sym) ,(length result-forms))
             (format t "Test ~a failed!~%Expected ~a results, got ~a.~%"
                     ,(string name) ,(length result-forms) (length ,result-sym)))
           ;; Match each evaluated value
           (cond
             ,@(loop for value in expected-forms
                     as idx from 0
                     collect
                     `((not (equalp (nth ,idx ,result-sym) ,value))
                       (format t "Test ~a failed for value ~a!~%Expected value: ~s~%Actual value:   ~s~%"
                        ,(string name)
                        ,idx
                        ,value
                        (nth ,idx ,result-sym))
                       nil))
             (t
              (when ,noisy-sym
                (format t "Test ~a passed.~%" ,(string name)))
              t))))))))

(defun run-tests (&key (noisy nil))
  (let ((total 0) (passed 0) (last-err nil))
	(maphash
	 (lambda (name func)
	   (incf total)
	   (let* ((error-result (gensym "ERROR"))
              (result (handler-case
					   (funcall func noisy)
					   (error (str)
						 (setf last-err str)
						 error-result))))
		 (cond
		   ((eql result error-result)
			(format t "Test ~a failed due to error: ~a~%" name last-err))
		   (result
			(incf passed)))))
	 *test-funcs*)
	(format t "~d/~d tests passed.~%" passed total)))

(defun reset-tests ()
  (clrhash *test-funcs*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest make-local-time-1 ()
	(let ((local-time (make-local-time :usec 1 :sec 2 :day 3 :timezone *default-timezone*)))
	  (values
	   (usec-of local-time)
	   (sec-of local-time)
	   (day-of local-time)))
  1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time<-1 ()
	(local-time< (make-local-time :day 1 :sec 0 :usec 0)
				 (make-local-time :day 2 :sec 0 :usec 0))
  t)
(deftest local-time<-2 ()
	(local-time< (make-local-time :day 2 :sec 0 :usec 0)
				 (make-local-time :day 1 :sec 0 :usec 0))
  nil)
(deftest local-time<-3 ()
	(local-time< (make-local-time :day 0 :sec 1 :usec 0)
				 (make-local-time :day 0 :sec 2 :usec 0))
  t)
(deftest local-time<-4 ()
	(local-time< (make-local-time :day 0 :sec 2 :usec 0)
				 (make-local-time :day 0 :sec 1 :usec 0))
  nil)
(deftest local-time<-5 ()
	(local-time< (make-local-time :day 0 :sec 0 :usec 1)
				 (make-local-time :day 0 :sec 0 :usec 2))
  t)
(deftest local-time<-6 ()
	(local-time< (make-local-time :day 0 :sec 0 :usec 2)
				 (make-local-time :day 0 :sec 0 :usec 1))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time<=-1 ()
	(local-time<= (make-local-time :day 1 :sec 0 :usec 0)
				  (make-local-time :day 2 :sec 0 :usec 0))
  t)
(deftest local-time<=-2 ()
	(local-time<= (make-local-time :day 2 :sec 0 :usec 0)
				  (make-local-time :day 1 :sec 0 :usec 0))
  nil)
(deftest local-time<=-3 ()
	(local-time<= (make-local-time :day 0 :sec 1 :usec 0)
				  (make-local-time :day 0 :sec 2 :usec 0))
  t)
(deftest local-time<=-4 ()
	(local-time<= (make-local-time :day 0 :sec 2 :usec 0)
				  (make-local-time :day 0 :sec 1 :usec 0))
  nil)
(deftest local-time<=-5 ()
	(local-time<= (make-local-time :day 0 :sec 0 :usec 1)
				  (make-local-time :day 0 :sec 0 :usec 2))
  t)
(deftest local-time<=-6 ()
	(local-time<= (make-local-time :day 0 :sec 0 :usec 2)
				  (make-local-time :day 0 :sec 0 :usec 1))
  nil)
(deftest local-time<=-7 ()
	(local-time<= (make-local-time :day 1 :sec 2 :usec 3)
				  (make-local-time :day 1 :sec 2 :usec 3))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time>-1 ()
	(local-time> (make-local-time :day 2 :sec 0 :usec 0)
				 (make-local-time :day 1 :sec 0 :usec 0))
  t)
(deftest local-time>-2 ()
	(local-time> (make-local-time :day 1 :sec 0 :usec 0)
				 (make-local-time :day 2 :sec 0 :usec 0))
  nil)
(deftest local-time>-3 ()
	(local-time> (make-local-time :day 0 :sec 2 :usec 0)
				 (make-local-time :day 0 :sec 1 :usec 0))
  t)
(deftest local-time>-4 ()
	(local-time> (make-local-time :day 0 :sec 1 :usec 0)
				 (make-local-time :day 0 :sec 2 :usec 0))
  nil)
(deftest local-time>-5 ()
	(local-time> (make-local-time :day 0 :sec 0 :usec 2)
				 (make-local-time :day 0 :sec 0 :usec 1))
  t)
(deftest local-time>-6 ()
	(local-time> (make-local-time :day 0 :sec 0 :usec 1)
				 (make-local-time :day 0 :sec 0 :usec 2))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time>=-1 ()
	(local-time>= (make-local-time :day 2 :sec 0 :usec 0)
				  (make-local-time :day 1 :sec 0 :usec 0))
  t)
(deftest local-time>=-2 ()
	(local-time>= (make-local-time :day 1 :sec 0 :usec 0)
				  (make-local-time :day 2 :sec 0 :usec 0))
  nil)
(deftest local-time>=-3 ()
	(local-time>= (make-local-time :day 0 :sec 2 :usec 0)
				  (make-local-time :day 0 :sec 1 :usec 0))
  t)
(deftest local-time>=-4 ()
	(local-time>= (make-local-time :day 0 :sec 1 :usec 0)
				  (make-local-time :day 0 :sec 2 :usec 0))
  nil)
(deftest local-time>=-5 ()
	(local-time>= (make-local-time :day 0 :sec 0 :usec 2)
				  (make-local-time :day 0 :sec 0 :usec 1))
  t)
(deftest local-time>=-6 ()
	(local-time>= (make-local-time :day 0 :sec 0 :usec 1)
				  (make-local-time :day 0 :sec 0 :usec 2))
  nil)
(deftest local-time>=-7 ()
	(local-time>= (make-local-time :day 1 :sec 2 :usec 3)
				  (make-local-time :day 1 :sec 2 :usec 3))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time=-1 ()
	(local-time= (make-local-time) (make-local-time))
  t)
(deftest local-time=-2 ()
	(local-time= (make-local-time) (make-local-time :usec 1))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest local-time/=-1 ()
	(local-time/= (make-local-time) (make-local-time))
  nil)
(deftest local-time/=-2 ()
	(local-time/= (make-local-time) (make-local-time :usec 1))
  t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest encode-local-time-1 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 3 2000)))
	  (values
	   (usec-of local-time)
	   (sec-of local-time)
	   (day-of local-time)))
  0 0 0)

(deftest encode-local-time-2 ()
	(let ((local-time (encode-local-time 0 0 0 0 29 2 2000)))
	  (values
	   (usec-of local-time)
	   (sec-of local-time)
	   (day-of local-time)))
  0 0 -1)

(deftest encode-local-time-3 ()
	(let ((local-time (encode-local-time 0 0 0 0 2 3 2000)))
	  (values
	   (usec-of local-time)
	   (sec-of local-time)
	   (day-of local-time)))
  0 0 1)

(deftest encode-local-time-4 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 1 2000)))
	  (values
	   (usec-of local-time)
	   (sec-of local-time)
	   (day-of local-time)))
  0 0 -60)

(deftest encode-local-time-5 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 3 2001)))
	  (values
	   (usec-of local-time)
	   (sec-of local-time)
	   (day-of local-time)))
  0 0 365)

(deftest decode-local-time-1 ((local-time (encode-local-time 1 2 3 4 5 6 2008)))
  (decode-local-time local-time)
  1 2 3 4 5 6 2008 4
  (nth-value 1 (timezone local-time))
  *default-timezone*
  (nth-value 2 (timezone local-time)))

(deftest decode-local-time-2 ()
	(let ((local-time (encode-local-time 0 0 0 0 1 1 0)))
	  (equal (multiple-value-list (decode-local-time local-time))
			 `(0 0 0 0 1 1 0 5
               ,(nth-value 1 (timezone local-time))
               ,*default-timezone*
               ,(nth-value 2 (timezone local-time)))))
  t)

(deftest decode-local-time-3 
    ((local-time (encode-local-time 0 0 0 0 1 3 2001)))
  (decode-local-time local-time)
  0 0 0 0 1 3 2001 4
  (nth-value 1 (timezone local-time))
  *default-timezone*
  (nth-value 2 (timezone local-time)))

(deftest decode-local-time-4
    ((local-time (encode-local-time 0 0 0 0 1 3 1998)))
  (decode-local-time local-time)
  0 0 0 0 1 3 1998 0
  (nth-value 1 (timezone local-time))
  *default-timezone*
  (nth-value 2 (timezone local-time)))

(deftest decode-local-time-5 ((local-time (make-local-time
                                           :day (- (random 65535) 36767)
                                           :sec (random 86400)
                                           :usec (random 1000))))
  (multiple-value-bind (ms ss mm hh day mon year)
      (decode-local-time local-time)
    (local-time= local-time (encode-local-time ms ss mm hh day mon year)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest format-timestring-1 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil)
  "2008-06-05T04:03:02.000001")
(deftest format-timestring-2 ()
    ;; This test only works on CDT (so far)
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil t)
  "2008-06-05T04:03:02.000001-5:00")
(deftest format-timestring-3 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008 +utc-zone+) t t)
  "2008-06-05T04:03:02.000001+0:00")
(deftest format-timestring-4 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 2)
  "-06-05T04:03:02.000001")
(deftest format-timestring-5 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 1)
  "-05T04:03:02.000001")
(deftest format-timestring-6 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0)
  "04:03:02.000001")
(deftest format-timestring-7 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 3)
  "04:03:02")
(deftest format-timestring-8 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 2)
  "04:03")
(deftest format-timestring-9 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 1)
  "04")
(deftest format-timestring-10 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 2008) nil nil 0 0)
  "")
(deftest format-timestring-11 ()
    (format-timestring nil (encode-local-time 1 2 3 4 5 6 -5) nil nil)
  "-0005-06-05T04:03:02.000001")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest local-timezone-1 ()
    ;; In 2005, April 4th is the start of daylight savings time.  The
    ;; difference between daylight savings and non-daylight savings
    ;; is one hour (for now)
    (- (local-timezone (encode-local-time 0 0 0 0 4 4 2005))
       (local-timezone (encode-local-time 0 0 0 0 3 4 2005)))
  3600)

(deftest unix-time-1 ()
    (unix-time (encode-local-time 0 0 0 0 1 1 1970))
  0)
(deftest universal-time-1 ()
    (decode-universal-time (universal-time (encode-local-time 1 2 3 4 5 6 2008)))
  2 3 4 5 6 2008 3 t 6)
  
(deftest local-time-1 ()
    (let ((now (now)))
      (local-time= (local-time :unix (unix-time now))
                   now))
  t)
(deftest local-time-2 ()
    (let ((now (get-universal-time)))
      (eql (universal-time (local-time :universal now))
           now))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest parse-timestring-1 ()
    (let ((local-time (now)))
      (local-time= (parse-timestring
                    (format-timestring nil local-time nil nil))
                   local-time))
  t)

(deftest parse-timestring-2 ()
    (let ((local-time (encode-local-time 0 0 0 0 1 1 0)))
      (local-time= (parse-timestring "0000-01-01T00:00:00,0")
                   local-time))
  t)

(deftest parse-timestring-3 ()
    (let ((local-time (encode-local-time 0 0 0 0 1 1 2006)))
      (local-time= (parse-timestring "2006-01-01T00:00:00,0")
                   local-time))
  t)

(deftest parse-timestring-4 ()
  (day-of (parse-timestring "xxxx 2006-01-01T00:00:00,0 xxxx"
                                    :start 5
                                    :end 15))
  (day-of (encode-local-time 0 0 0 0 1 1 2006)))

(deftest parse-timestring-5 ()
  (local-time= (parse-timestring "2008-07-06T05:04:03,02")
               (encode-local-time 20000 3 4 5 6 7 2008)))

(deftest parse-timestring-6 ()
  (local-time= (parse-timestring "--23T::02")
               (multiple-value-bind (ms ss mm hh day mon year)
                   (decode-local-time (now))
                 (declare (ignore ss day))
                 (encode-local-time ms 02 mm hh 23 mon year))))

(deftest parse-timestring-7 ()
  (local-time= (parse-timestring "T05:06:07,08")
               (multiple-value-bind (ms ss mm hh day mon year)
                   (decode-local-time (now))
                 (declare (ignore ms ss mm hh))
                 (encode-local-time 80000 7 6 5 day mon year))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest local-time-adjust-1 ((test-zone (make-timezone
                                          :subzones '((-3600 NIL "UTC-1" T NIL))
                                          :loaded t)))
  (let ((epoch (local-time :unix 0 :timezone +utc-zone+)))
    (decode-local-time (local-time-adjust epoch test-zone (make-local-time))))
  ;; ms ss mm hh day mon year wday ds-p zone abbrev
  00 00 00 23 31 12 1969 3 nil test-zone)

(deftest local-time-adjust-2 ()
  (let ((test-zone (make-timezone :subzones '((-3600 NIL "UTC-1" T NIL))
                                   :loaded t))
         (epoch (local-time :unix 3600 :timezone +utc-zone+)))
    (decode-local-time (local-time-adjust epoch test-zone (make-local-time))))
  ;; ms ss mm hh day mon year
  00 00 00 00 01 01 1970)

(deftest local-time-adjust-3 ()
  (let ((test-zone (make-timezone :subzones '((+3600 NIL "UTC+1" T NIL))
                                   :loaded t))
         (epoch (local-time :unix 0 :timezone +utc-zone+)))
    (decode-local-time (local-time-adjust epoch test-zone (make-local-time))))
  ;; ms ss mm hh day mon year
  00 00 00 01 01 01 1970)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest read-timestring-1 ((now (now)))
  (with-input-from-string (ins (format-timestring nil now nil nil))
    (local-time= (read-timestring ins #\@) now))
  t)

(deftest read-universal-time-1 ((now (now)))
  (with-input-from-string (ins (format nil "~a" (universal-time now)))
    (local-time= (read-universal-time ins #\@ nil) now))
  t)

