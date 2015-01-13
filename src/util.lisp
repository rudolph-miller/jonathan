(in-package :cl-user)
(defpackage jonathan.util
  (:use :cl)
  (:import-from :jonathan.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :defroute)
  (:import-from :clack.response
                :headers)
  (:import-from :cl-emb
                :*escape-type*
                :*case-sensitivity*
                :*function-package*
                :execute-emb)
  (:import-from :datafly
                :encode-json)
  (:export :render-json))
(in-package :jonathan.util)

(syntax:use-syntax :annot)

(defun render-json (object)
  (setf (headers *response* :content-type) "application/json")
  (encode-json (convert-object)))

(defun convert-object (object)
  (if (typep object 'CONS)
      (if (typep (car object) 'KEYWORD)
          (loop for (key value) on object by #'cddr
             nconcing (list key (convert-object value)))
          (coerce object 'simple-vector))
      object))

(defmacro define-method (method)
  (let ((name (intern (concatenate 'string (symbol-name method) "API"))))
    `(defmacro ,name (function-form)
       (let* ((func-name (cadr function-form))
              (func-args (caddr function-form))
              (func-body (cadddr function-form))
              (path-name (concatenate 'string "/api/" (string-downcase (symbol-name func-name)))))
         (unless (gethash *package* caveman2.app::*package-app-map*)
           (setf (gethash *package* caveman2.app::*package-app-map*)
                 (gethash (find-package :jonathan.web) caveman2.app::*package-app-map*)))
         `(defroute ,func-name (,path-name :method ,,method) (,@func-args) (render-json ,func-body))))))

@export
(define-method :GET)

@export
(define-method :POST)

@export
(define-method :PUT)

@export
(define-method :DELETE)
