(in-package #:local-time.test)

(defsuite* (timezone :in test))
(eval-when (:load-toplevel :execute)
  (local-time::define-timezone eastern-tz
      (merge-pathnames #p"EST5EDT" local-time::*default-timezone-repository-path*)))

(deftest test/timezone/decode-timestamp-dst ()
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
