;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage

(to-json '(:name "Common Lisp" :born 1984 :impls (SBCL KCL)))
;; => "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}"

(to-json '(:name "Common Lisp" :born 1984 :impls (SBCL KCL))
         :octets t)
;; => #(123 34 78 65 77 69 34 58 34 67 111 109 109 111 110 32 76 ...)

(to-json '((:name . "Common Lisp") (:born . 1984) (:impls SBCL KCL))
         :from :alist)
;; => "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}"

(to-json '(:obj (:name . "Common Lisp") (:born . 1984) (:impls SBCL KCL))
         :from :jsown)
;; => "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Property List Samples

(to-json '(:name :age :born :impls))
;; => "{\"NAME\":\"AGE\",\"BORN\":\"IMPLS\"}"
;; not "[\"NAME\",\"AGE\",\"BORN\",\"IMPLS\"]"

(to-json '(:name "Common Lisp" :born))
;; => "{\"NAME\":\"Common Lisp\",\"BORN\":[]}"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend to-json

(defclass user ()
  ((id :type integer :initarg :id)
   (name :type string :initarg :name)))

(defmethod %to-json ((user user))
  (with-object
    (write-key-value "id" (slot-value user 'id))
    (write-key-value "name" (slot-value user 'name))))

(to-json (make-instance 'user :id 1 :name "Rudolph"))
;; => "{\"id\":1,\"name\":\"Rudolph\"}"
