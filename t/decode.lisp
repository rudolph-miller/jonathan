(in-package :cl-user)
(defpackage jonathan-test.decode
  (:use :cl
        :prove
        :jonathan.util
        :jonathan.encode
        :jonathan.decode))
(in-package :jonathan-test.decode)

(plan 6)

(diag "jonathan-test.decode")

(defun plist-alist (plist)
  (if (my-plist-p plist)
      (mapcar #'(lambda (item)
                  (cons (symbol-name (car item))
                        (plist-alist (cdr item))))
              (alexandria:plist-alist plist))
      plist))

(defmacro parse-test (target comment)
  `(progn
     (subtest ,comment
       (is (parse (to-json ,target))
           ,target
           ":as :plist.")
       (is (parse (to-json ,target) :as :alist)
           (if (my-plist-p ,target)
               (plist-alist ,target)
               ,target)
           ":as :alist."))))

(parse-test "\"Rudolph\""
            "with string.")

(parse-test "[\"Rudolph\",\"Miller\"]"
            "with list.")

(parse-test "1"
            "with integer.")

(parse-test "-1"
            "with negative.")

(parse-test "0.1"
            "with float.")

(parse-test '(:Rudolph "Miller")
            "with object.")

(finalize)
