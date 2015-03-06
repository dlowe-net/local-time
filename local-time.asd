(defsystem #:local-time
  :name "local-time"
  :version "1.0.5"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "A library for manipulating dates and times, based on a paper by Erik Naggum"
  :depends-on (:cl-fad :collectors)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "string-format")
                             (:file "local-time")))))

(defmethod perform ((op test-op) (system (eql (find-system :local-time))))
  (asdf:load-system :local-time.test)
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'local-time.test::test)")))
