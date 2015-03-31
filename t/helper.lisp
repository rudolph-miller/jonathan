(in-package :cl-user)
(defpackage jonathan-test.helper
  (:use :cl
        :prove
        :jonathan
        :jonathan.helper))
(in-package :jonathan-test.helper)

(diag "jonathan-test.helper")

(plan nil)

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

(finalize)

