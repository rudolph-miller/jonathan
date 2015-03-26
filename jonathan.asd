(in-package :cl-user)
(defpackage jonathan-asd
  (:use :cl :asdf))
(in-package :jonathan-asd)

(defsystem jonathan
  :version "0.1"
  :author "Rudolph-Miller"
  :license ""
  :depends-on (:clack
               :clack-middleware-csrf
               :caveman2
               :envy
               :cl-ppcre
               :fast-io
               :jsown
               :cl-cookie

               ;; HTML Template
               :cl-emb

               ;; for CL-DBI
               :datafly
               :sxql
               :cl-syntax)
  :components ((:module "src"
                :components
                ((:file "jonathan" :depends-on ("config" "app"))
                 (:file "web" :depends-on ("util"))
                 (:file "util" :depends-on ("config"))
                 (:file "config")
                 (:file "app" :depends-on ("web")))))
  :description ""
  :in-order-to ((test-op (test-op jonathan-test))))
