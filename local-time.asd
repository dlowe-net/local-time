(defpackage #:local-time.system
  (:use :common-lisp :asdf))

(in-package #:local-time.system)

(defsystem :local-time
  :name "local-time"
  :version "0.9.1"
  :author "Daniel Lowe <dlowe@sanctuary.org>"
  :description "A library for manipulating dates and times, based on a paper by Erik Naggum"
  :components ((:file "local-time"))
  :depends-on (:cl-fad))

(defsystem #:local-time.test
  :name "local-time.test"
  :version "0.9.1"
  :author "Daniel Lowe <dlowe@sanctuary.org>"
  :description "Testing code for the local-time library"
  :depends-on (:local-time :fiveam)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (system (eql (find-system :local-time))))
  (operate 'load-op '#:local-time.test)
  (funcall (read-from-string "5am:run!")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :local-time))))
  nil)

