(in-package :cl-user)
(defpackage jonathan-test.util
  (:use :cl
        :prove
        :jonathan.util)
  (:import-from :jonathan.util
                :my-plist-p))
(in-package :jonathan-test.util)

(plan nil)

(defun plist-alist (plist)
  (if (my-plist-p plist)
      (mapcar #'(lambda (item)
                  (cons (symbol-name (car item))
                        (plist-alist (cdr item))))
              (alexandria:plist-alist plist))
      plist))

(subtest "to-json"
  (is (to-json "Rudolph")
      "\"Rudolph\""
      "with string.")

  (is (to-json '("Rudolph" "Miller"))
      "[\"Rudolph\",\"Miller\"]"
      "with list.")

  (is (to-json 1)
      "1"
      "with integer.")

  (is (to-json -1)
      "-1"
      "with negative.")

  (is (to-json (/ 1 10))
      "0.1"
      "with rational.")

  (is (to-json 1.1)
      "1.1"
      "with float.")

  (is (to-json '(:|Rudolph| "Miller"))
      "{\"Rudolph\":\"Miller\"}"
      "with plist."))

(defmacro parse-test (target comment)
  `(progn
     (subtest ,comment
       (is (parse1 (to-json ,target))
           ,target
           ":as :plist.")
       (is (parse1 (to-json ,target) :as :alist)
           (if (my-plist-p ,target)
               (plist-alist ,target)
               ,target)
           ":as :alist."))))


(subtest "parse1"
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
              "with object."))

(finalize)
