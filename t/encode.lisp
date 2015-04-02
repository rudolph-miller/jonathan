(in-package :cl-user)
(defpackage jonathan-test.encode
  (:use :cl
        :prove
        :jonathan))
(in-package :jonathan-test.encode)

(diag "jonathan-test.encode")

(plan 24)

(defclass user ()
  ((id :initarg :id)
   (name :initarg :name)))

(subtest "with-object"
  (defmethod %to-json ((user user))
    (with-slots ((id id) (name name)) user
      (with-object
        (write-key "id")
        (write-value id)
        (write-key-value "name" name))))
  (is (to-json (make-instance 'user :id 1 :name "Rudolph"))
      "{\"id\":1,\"name\":\"Rudolph\"}"
      "can handle write-key, write-value and write-key-value.")

  (defmethod %to-json ((user user))
    (with-slots ((id id) (name name)) user
      (with-object
        (write-key "id")
        (write-value id)
        (write-key "information")
        (write-value
         (with-object
           (write-key "name")
           (write-value name))))))
  (is (to-json (make-instance 'user :id 1 :name "Rudolph"))
      "{\"id\":1,\"information\":{\"name\":\"Rudolph\"}}"
      "can handle nested macro."))

(subtest "with-array"
  (defmethod %to-json ((user user))
    (with-slots ((id id) (name name)) user
      (with-array
        (write-item id)
        (write-item name))))
  (is (to-json (make-instance 'user :id 1 :name "Rudolph"))
      "[1,\"Rudolph\"]"
      "can handle write-item.")

  (defmethod %to-json ((user user))
    (with-slots ((id id) (name name)) user
      (with-array
        (write-item
         (with-array
           (write-item "id")
           (write-item id)))
        (write-item
         (with-array
           (write-item "name")
           (write-item name))))))
  (is (to-json (make-instance 'user :id 1 :name "Rudolph"))
      "[[\"id\",1],[\"name\",\"Rudolph\"]]"
      "can handle nested macro."))

(subtest "with-output"
  (is (with-output-to-string (stream)
        (with-output (stream)
          (with-object
            (write-key-value "key" "value"))))
      "{\"key\":\"value\"}"
      "can write into stream."))

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

(is (to-json (format nil "Rudo~alph" #\Newline))
    "\"Rudo\\nlph\""
    "with #\Newline.")

(is (to-json (format nil "Rudo~alph" #\Return))
    "\"Rudo\\rlph\""
    "with #\Return.")

(is (to-json (format nil "Rudo~alph" #\Tab))
    "\"Rudo\\tlph\""
    "with #\Tab.")

(is (to-json (format nil "Rudo~alph" #\"))
    "\"Rudo\\\"lph\""
    "with #\".")

(is (to-json (format nil "Rudo~alph" #\\))
    "\"Rudo\\\\lph\""
    "with #\\.")

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
  (with-object
    (write-key "id")
    (write-value (slot-value user 'id))
    (write-key-value "name" (slot-value user 'name))))

(is (to-json (make-instance 'user :id 1 :name "Rudolph"))
    "{\"id\":1,\"name\":\"Rudolph\"}"
    "customizable.")

(finalize)
