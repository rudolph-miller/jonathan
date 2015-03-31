(in-package :cl-user)
(defpackage jonathan-test.helper
  (:use :cl
        :prove
        :jonathan))
(in-package :jonathan-test.helper)

(diag "jonathan-test.helper")

(plan 5)

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

(subtest "with-output-to-string*"
  (is (with-output-to-string*
        (with-object
          (write-key-value "key" "value")))
      "{\"key\":\"value\"}"
      "can return encoded string."))

(subtest "compile-encoder"
  (let ((encoder1 (compile-encoder () (value)
                    (list :key value)))
        (encoder2 (compile-encoder (:from :alist) (value)
                    `(("key" . ,value))))
        (encoder3 (compile-encoder (:octets t) (value)
                    (list :key value))))

    (is (funcall encoder1 "value")
        "{\"KEY\":\"value\"}"
        "without options.")

    (is (funcall encoder2 "value")
        "{\"key\":\"value\"}"
        "with :from.")

    (is (funcall encoder3 "value")
        #(123 34 75 69 89 34 58 34 118 97 108 117 101 34 125)
        "with :octets T."
        :test #'equalp)

    (is-error (eval '(compile-encoder () (value value)))
              'simple-error
              "can raise the error with duplicate variables.")

    (is-error (eval '(compile-encoder () ("value")))
              'simple-error
              "can raise the error with non-symbol in lambda-list.")

    (is-error (eval '(compile-encoder () (:value)))
              'simple-error
              "can raise the error with keyword in lambda-list.")))

(finalize)
