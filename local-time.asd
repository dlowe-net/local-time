(defpackage #:local-time.system
  (:use :common-lisp :asdf))

(in-package #:local-time.system)

(defsystem :local-time
  :name "local-time"
  :version "0.9.1"
  :author "Daniel Lowe <dlowe@sanctuary.org>"
  :description "A library for manipulating dates and times, based on a paper by Erik Naggum"
  :components ((:file "local-time")))

(defsystem #:local-time.test
  :name "local-time.test"
  :version "0.9.1"
  :author "Daniel Lowe <dlowe@sanctuary.org>"
  :description "Testing code for the local-time library"
  :depends-on (:local-time) 
  :components ((:file "tests")))

(defmethod perform ((op test-op) (system (eql (find-system :local-time))))
  (operate 'load-op '#:local-time.test)
  (funcall (read-from-string "local-time::run-tests")))

(defmethod perform :after ((op load-op) (system (eql (find-system :local-time))))
  (in-package :local-time))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :local-time))))
  nil)

