(in-package :cl-user)
(defpackage jonathan.error
  (:use :cl)
  (:export :<jonathan-error>
           :<jonathan-unexpected-eof-error>
           :<jonathan-incomplete-json-error>))
(in-package :jonathan.error)

(define-condition <jonathan-error> (simple-error)
  ()
  (:documentation "Base condition of jonathan-errors."))

(define-condition <jonathan-unexpected-eof-error> (<jonathan-error>)
  ((object :initarg :object :documentation "Parsing object slot."))
  (:report
   (lambda (condition stream)
     (with-slots (object) condition
       (format stream "Unexpected EOF found:~%~a~%" object))))
  (:documentation "Unexpecded EOF error."))

(define-condition <jonathan-incomplete-json-error> (<jonathan-error>)
  ((object :initarg :object :documentation "Parsing objcet slot."))
  (:report
   (lambda (condition stream)
     (with-slots (object) condition
       (format stream "Incomplete JSON string:~%~a~%" object))))
  (:documentation "Incomplete JSON string error."))
