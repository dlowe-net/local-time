(defsystem :cl-postgres+local-time
  :depends-on (:cl-postgres :local-time)
  :components ((:module "src"
                :components ((:module "integration"
                              :components ((:file "cl-postgres")))))))
