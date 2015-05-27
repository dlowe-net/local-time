(defsystem :local-time.test
  :name "local-time.test"
  :version "1.0.6"
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Testing code for the local-time library"
  :depends-on (:stefil
               :local-time)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "simple")
                             (:file "comparison")
                             (:file "formatting")
                             (:file "parsing")
                             (:file "timezone")))))

