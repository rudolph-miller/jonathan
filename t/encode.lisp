(in-package :cl-user)
(defpackage jonathan-test.encode
  (:use :cl
        :prove
        :integral
        :jonathan.encode))
(in-package :jonathan-test.encode)

(plan 10)

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

(is (to-json '(:|Rudolph| "Miller") :octet t)
    #(123 34 82 117 100 111 108 112 104 34 58 34 77 105 108 108 101 114 34 125)
    :test #'equalp
    ":octet T.")

(is (to-json '((:|Rudolph| . "Miller")) :from :alist)
    "{\"Rudolph\":\"Miller\"}"
    ":from :alist.")

(defclass user ()
  ((id :type integer :primary-key t :initarg :id)
   (name :type strign :initarg :name))
  (:metaclass <dao-table-class>))

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
