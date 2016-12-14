(in-package :cl-user)
(defpackage jonathan-test-asd
  (:use :cl :asdf))
(in-package :jonathan-test-asd)

(defsystem jonathan-test
  :author "Rudolph-Miller"
  :license "MIT"
  :description "Tests of Jonathan."
  :depends-on (:jonathan
               :prove
               :legion)
  :components ((:module "t"
                :components
                ((:test-file "jonathan")
                 (:test-file "util")
                 (:test-file "encode")
                 (:test-file "decode")
                 (:test-file "helper")
                 (:test-file "thread"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                      (unwind-protect
                           (funcall (intern #.(string :run-test-system) :prove.asdf) c))))
