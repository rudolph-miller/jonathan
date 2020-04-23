(in-package :cl-user)
(defpackage jonathan-test.encode
  (:use :cl
        :prove
        :jonathan))
(in-package :jonathan-test.encode)

(diag "jonathan-test.encode")

(plan 27)

(subtest "with-object"
  (is-print
   (with-object
     (write-key "key1")
     (write-value "value1")
     (write-key-value "key2" "value2"))
   "{\"key1\":\"value1\",\"key2\":\"value2\"}"
   "can handle write-key, write-value and write-key-value.")

  (is-print
   (with-object
     (write-key "key1")
     (write-value
      (with-object
        (write-key "key2")
        (write-value "value"))))
   "{\"key1\":{\"key2\":\"value\"}}"
   "can handle nested macro."))

(subtest "with-array"
  (is-print
   (with-array
     (write-item 1)
     (write-item 2))
   "[1,2]"
   "can handle write-item.")

  (is-print
   (with-array
     (write-item 1)
     (write-item
      (with-array
        (write-item 2)
        (write-item 3))))
     "[1,[2,3]]"
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

(is (to-json 'rudolph)
    "\"RUDOLPH\""
    "with symbol.")

(is (to-json "Rudolph")
    "\"Rudolph\""
    "with simple-string.")

(is (to-json (let* ((simple-string "Rudolph")
                    (not-so-simple-string
                      (make-array (length simple-string)
                                  :element-type 'character
                                  :displaced-to simple-string
                                  :displaced-index-offset 0)))
               not-so-simple-string))
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
    "with vector.")

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

(is (to-json 1.1d0)
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

(subtest "nested alist with array value"
  (let ((a0 '(("a" . (("aa" . 1)))))
        (a1 '(("a" . ((("aa" . 1))))))
        (a2 '(("a" . ((("aa" . 1) ("bb" . 2))))))
        (a3 '(("a" . ((("aa" . 1)) (("bb" . 2)))))))
    (is (to-json a0 :from :alist)
        "{\"a\":{\"aa\":1}}"
        "value is alist")
    (is (to-json a1 :from :alist)
        "{\"a\":[{\"aa\":1}]}"
        "value is array of alist")
    (is (to-json a2 :from :alist)
        "{\"a\":[{\"aa\":1,\"bb\":2}]}"
        "value is array of alist")
    (is (to-json a3 :from :alist)
        "{\"a\":[{\"aa\":1},{\"bb\":2}]}"
        "value is array of alists")))

(is (to-json '(:obj (:|Rudolph| . "Miller")) :from :jsown)
    "{\"Rudolph\":\"Miller\"}"
    ":from :jsown.")

(is (let ((user "bob"))
      (to-json (list :a user :b user)))
    "{\"A\":\"bob\",\"B\":\"bob\"}")

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
