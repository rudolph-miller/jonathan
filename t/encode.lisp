(in-package :cl-user)
(defpackage jonathan-test.encode
  (:use :cl
        :prove
        :jonathan))
(in-package :jonathan-test.encode)

(diag "jonathan-test.encode")

(plan 17)

(is (to-json t)
    "true"
    "with T.")

(is (to-json nil)
    "[]"
    "with NIL.")

(is (to-json :false)
    "false"
    "with :false.")

(is (to-json "Rudolph")
    "\"Rudolph\""
    "with string.")

(is (to-json '("Rudolph" "Miller"))
    "[\"Rudolph\",\"Miller\"]"
    "with list.")

(is (to-json #("Rudolph" "Miller"))
    "[\"Rudolph\",\"Miller\"]"
    "with simple-vector.")

(let ((hash (make-hash-table)))
  (setf (gethash :|Rudolph| hash) "Miller")
  (is (to-json hash)
      "{\"Rudolph\":\"Miller\"}"
      "with hash-talbe."))

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

(is (to-json '(:|Rudolph| "Miller") :octets t)
    #(123 34 82 117 100 111 108 112 104 34 58 34 77 105 108 108 101 114 34 125)
    :test #'equalp
    ":octet T.")

(is (to-json '((:|Rudolph| . "Miller")) :from :alist)
    "{\"Rudolph\":\"Miller\"}"
    ":from :alist.")

(is (to-json '(:obj (:|Rudolph| . "Miller")) :from :jsown)
    "{\"Rudolph\":\"Miller\"}"
    ":from :jsown.")

(defclass user ()
  ((id :type integer :initarg :id)
   (name :type string :initarg :name)))

(defmethod %to-json ((user user))
  (%write-char #\{)
  (%to-json "id")
  (%write-char #\:)
  (%to-json (slot-value user 'id))
  (%write-char #\,)
  (%to-json "name")
  (%write-char #\:)
  (%to-json (slot-value user 'name))
  (%write-char #\}))

(is (to-json (make-instance 'user :id 1 :name "Rudolph"))
    "{\"id\":1,\"name\":\"Rudolph\"}"
    "customizable.")

(finalize)
