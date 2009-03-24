(cl:in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :asdf-system-connections nil)
    (asdf:operate 'asdf:load-op :asdf-system-connections)))

(defpackage #:local-time.system
  (:use :common-lisp :asdf))

(in-package #:local-time.system)

(defsystem :local-time
  :name "local-time"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "A library for manipulating dates and times, based on a paper by Erik Naggum"
  :components ((:module "src"
                        :components ((:file "local-time"))))
  :depends-on (:cl-fad))

(defsystem #:local-time.test
  :name "local-time.test"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Testing code for the local-time library"
  :depends-on (:local-time :fiveam)
  :components ((:file "tests" :pathname "tests/tests")))

#+lispworks
(defmethod perform :around ((op compile-op) c)
  ;; KLUDGE: Installing an :around method on a type that has not been defined by
  ;; us is not nice, but it's needed to make %unix-gettimeofday compile under
  ;; Lispworks, which doesn't ignore the #_ reader macro in #_gettimeofday.
  ;; It should be removed eventually.
  (let ((*readtable* (copy-readtable)))
    (set-dispatch-macro-character #\# #\_ (constantly nil))
    (call-next-method)))

(defmethod perform ((op test-op) (system (eql (find-system :local-time))))
  (operate 'load-op '#:local-time.test)
  (funcall (read-from-string "5am:run!")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :local-time))))
  nil)

#+:asdf-system-connections
(defsystem-connection cl-postgres-and-local-time
  :requires (:cl-postgres :local-time)
  :components ((:module "src"
                        :components ((:module "integration"
                                      :components ((:file "cl-postgres")))))))
