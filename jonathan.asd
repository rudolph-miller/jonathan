(in-package :cl-user)
(defpackage jonathan-asd
  (:use :cl :asdf))
(in-package :jonathan-asd)

(defsystem jonathan
  :version "0.1"
  :author "Rudolph-Miller"
  :license "MIT"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :fast-io
               :trivial-types
               :babel
               :proc-parse
               :cl-ppcre
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "jonathan" :depends-on ("encode" "decode" "helper" "error"))
                 (:file "helper" :depends-on ("encode" "decode"))
                 (:file "encode" :depends-on ("util" "error"))
                 (:file "decode" :depends-on ("util"))
                 (:file "util")
                 (:file "error"))))
  :description "High performance JSON encoder and decoder. Currently support: SBCL, CCL."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op jonathan-test))))
