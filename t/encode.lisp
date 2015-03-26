(in-package :cl-user)
(defpackage jonathan-test.encode
  (:use :cl
        :prove
        :jonathan.encode))
(in-package :jonathan-test.encode)

(plan 7)

(diag "jonathan-test.encode")

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
    "with plist.")

(finalize)
