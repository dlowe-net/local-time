(defsystem :local-time
  :name "local-time"
  :version "1.0.1"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "A library for manipulating dates and times, based on a paper by Erik Naggum"
  :components ((:module "src"
                :components ((:file "local-time"))))
  :depends-on (:cl-fad))

(defmethod perform ((op test-op) (system (eql (find-system :local-time))))
  (asdf:load-system :local-time.test)
  (eval (read-from-string "(hu.dwim.stefil:funcall-test-with-feedback-message 'local-time.test::test)")))
