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
  (:import-from :fast-io
                :fast-write-byte
                :make-output-buffer
                :finish-output-buffer)
  (:import-from :datafly
                :encode-json)
  (:export :render-json
           :to-json))
(in-package :jonathan.util)

(syntax:use-syntax :annot)

(defun render-json (object)
  (setf (headers *response* :content-type) "application/json")
  (encode-json (convert-object object)))

(defun convert-object (object)
  (if (typep object 'CONS)
      (if (typep (car object) 'KEYWORD)
          (loop for (key value) on object by #'cddr
             nconcing (list key (convert-object value)))
          (coerce object 'simple-vector))
      object))

(defmacro define-method (method)
  (let ((name (intern (concatenate 'string (symbol-name method) "API"))))
    `(defmacro ,name (&body body)
       (let* ((function-form (if (= (length body) 1)
                                 (cdar body)
                                 body))
              (func-name (car function-form))
              (func-args (cadr function-form))
              (func-body (caddr function-form))
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

(defun plist-p (list)
  (typecase list
    (null t)
    (cons (loop for (key val next) on list by #'cddr
                if (not (keywordp key))
                  return nil
                else
                  unless next return t))))

(defvar *stream* nil)
(defvar *octet* nil)

(defun %write-string (string)
  (if *octet*
      (loop for c across string
            do (fast-write-byte (char-code c) *stream*))
      (write-string string *stream*)))

(defun %write-char (char)
  (if *octet*
      (fast-write-byte (char-code char) *stream*)
      (write-char char *stream*)))

(defun to-json (obj &key octet)
  "Converting object to JSON String."
  (let ((*stream* (if octet (make-output-buffer)
                      (make-string-output-stream)))
        (*octet* octet))
    (%to-json obj)
    (if octet
        (finish-output-buffer *stream*)
        (get-output-stream-string *stream*))))

(defgeneric %to-json (obj))

(defmethod %to-json ((string string))
  (%write-char #\")
  (loop for char across string
        do (case char
             (#\newline (%write-string "\\n"))
             (#\return (%write-string "\\r"))
             (#\tab (%write-string "\\t"))
             (#\" (%write-string "\\\""))
             (#\\ (%write-string "\\\\"))
             (t (%write-char char))))
  (%write-char #\"))

(defmethod %to-json ((number number))
  (%write-string (princ-to-string number)))

(defmethod %to-json ((ratio ratio))
  (%to-json (coerce ratio 'float)))

(defmethod %to-json ((list list))
  (if (plist-p list)
      (progn (%write-char #\{)
             (loop for (key val next) on list by #'cddr
                   do (%to-json (princ-to-string key))
                   do (%write-char #\:)
                   do (%to-json val)
                   when next do (%write-char #\,))
             (%write-char #\}))
      (progn (%write-char #\[)
             (loop for (item next) on list
                   do (%to-json item)
                   when next do (%write-char #\,))
             (%write-char #\]))))

(defmethod %to-json ((symbol symbol))
  (%to-json (symbol-name symbol)))

(defmethod %to-json ((true (eql t)))
  (declare (ignore true))
  (%write-string "true"))

(defmethod %to-json ((true (eql :t)))
  (declare (ignore true))
  (%write-string "true"))

(defmethod %to-json ((true (eql :true)))
  (declare (ignore true))
  (%write-string "true"))

(defmethod %to-json ((false (eql :false)))
  (declare (ignore false))
  (%write-string "false"))

(defmethod %to-json ((false (eql :f)))
  (declare (ignore false))
  (%write-string "false"))

(defmethod %to-json ((false (eql :null)))
  (declare (ignore false))
  (%write-string "null"))

(defmethod %to-json ((false (eql :n)))
  (declare (ignore false))
  (%write-string "null"))

(defmethod %to-json ((n (eql nil)))
  (declare (ignore n))
  (%write-string "[]"))
