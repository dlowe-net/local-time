(cl:in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (asdf:find-system :asdf-system-connections nil)
    (error "The local-time system requires asdf-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions."))
  (asdf:operate 'asdf:load-op :asdf-system-connections))

(defpackage #:local-time.system
  (:use :common-lisp :asdf :asdf-system-connections))

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
  :components ((:module "tests"
                        :components ((:file "tests")))))

(defmethod perform ((op test-op) (system (eql (find-system :local-time))))
  (operate 'load-op '#:local-time.test)
  (funcall (read-from-string "5am:run!")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :local-time))))
  nil)

(defsystem-connection cl-postgres-and-local-time
  :requires (:cl-postgres :local-time)
  :components ((:module "src"
                        :components ((:module "integration"
                                      :components ((:file "cl-postgres")))))))
