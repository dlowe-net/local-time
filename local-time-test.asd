(asdf:defsystem local-time-test
  :name "local-time-test"
  :version "0.5.0"
  :author "Daniel Lowe <dlowe@sanctuary.org>"
  :description "Testing code for local-time library"
  :components ((:file "local-time")
               (:file "local-time-test" :depends-on ("local-time"))))
