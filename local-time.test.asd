(defsystem :local-time.test
  :name "local-time.test"
  :version "1.0.0d"
  :author "Daniel Lowe <dlowe@bitmuse.com>"
  :description "Testing code for the local-time library"
  :depends-on (:hu.dwim.stefil+swank
               :local-time)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "simple")
                             (:file "comparison")
                             (:file "formatting")
                             (:file "parsing")
                             (:file "timezone")))))

