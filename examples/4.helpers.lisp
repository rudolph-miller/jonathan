(defclass user ()
  ((id :initarg :id)
   (name :initarg :name)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-object

(defmethod %to-json ((user user))
  (with-object
    (write-key "id")
    (write-value (slot-value user 'id))
    (write-key-value "name" (slot-value user 'name))))

(to-json (make-instance 'user :id 1 :name "Rudolph"))
;; => "{\"id\":1,\"name\":\"Rudolph\"}"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-array

(defmethod %to-json ((user user))
  (with-array
    (write-item "id")
    (write-item (slot-value user 'id))
    (write-item "name")
    (write-item (slot-value user 'name))))

(to-json (make-instance 'user :id 1 :name "Rudolph"))
;; => "[\"id\",1,\"name\",\"Rudolph\"]"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-output

(with-output-to-string (stream)
  (with-output (stream)
    (with-object
      (write-key-value "key" "value"))))
;; => "{\"key\":\"value\"}"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-output-to-string*

(with-output-to-string*
  (with-object
    (write-key-value "key" "value"))))
;; => "{\"key\":\"value\"}"
