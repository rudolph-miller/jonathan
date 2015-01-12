(in-package :cl-user)
(defpackage jonathan-test-asd
  (:use :cl :asdf))
(in-package :jonathan-test-asd)

(defsystem jonathan-test
  :author "Rudolph-Miller"
  :license ""
  :depends-on (:jonathan
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "jonathan"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
