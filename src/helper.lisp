(in-package :cl-user)
(defpackage jonathan.helper
  (:use :cl
        :jonathan.encode)
  (:import-from :babel)
  (:import-from :fast-io
                :fast-write-sequence
                :make-output-buffer
                :finish-output-buffer)
  (:import-from :ppcre
                :scan)
  (:import-from :alexandria
                :ensure-list)
  (:export :write-key
           :write-value
           :write-key-value
           :with-object
           :with-array
           :write-item
           :with-output
           :with-output-to-string*
           :compile-encoder))
(in-package :jonathan.helper)

(defparameter *compile-encoder-prefix* "jonathan-encoder")

(defun with-macro-p (list)
  (and (consp list)
       (member (car list) '(with-object with-array))))

(defmacro write-key (key)
  (declare (ignore key)))

(defmacro write-value (value)
  (declare (ignore value)))

(defmacro write-key-value (key value)
  (declare (ignore key value)))

(defmacro with-object (&body body)
  (let ((first (gensym "first")))
    `(let ((,first t))
       (macrolet ((write-key (key)
                    `(progn
                       (if ,',first
                           (setq ,',first nil)
                           (%write-char #\,))
                       (%to-json (princ-to-string ,key))))
                  (write-value (value)
                    `(progn
                       (%write-char #\:)
                       ,(if (with-macro-p value)
                            value
                            `(%to-json ,value))))
                  (write-key-value (key value)
                    `(progn
                       (write-key ,key)
                       (write-value ,value))))
         (%write-char #\{)
         ,@body
         (%write-char #\})))))

(defmacro write-item (item)
  (declare (ignore item)))

(defmacro with-array (&body body)
  (let ((first (gensym "first")))
    `(let ((,first t))
       (macrolet ((write-item (item)
                    `(progn
                       (if ,',first
                           (setq ,',first nil)
                           (%write-char #\,))
                       ,(if (with-macro-p item)
                            item
                            `(%to-json ,item)))))
         (%write-char #\[)
         ,@body
         (%write-char #\])))))

(defmacro with-output ((stream) &body body)
  `(let ((*stream* ,stream))
     ,@body))

(defmacro with-output-to-string* (&body body)
  `(with-output-to-string (stream)
     (with-output (stream)
       ,@body)))

(defmacro compile-encoder ((&key octet from) (&rest args) &body body)
  (let ((lambda-list-hash (make-hash-table :test #'equal)))
    (map nil
         (lambda (sym)
           (let ((genstr (format nil "~a~a" *compile-encoder-prefix* (random 1000000))))
             (setf (gethash (symbol-name sym) lambda-list-hash)
                   genstr)
             (setf (gethash genstr lambda-list-hash)
                   (symbol-name sym))))
                   
         args)
    `(let ,(mapcar #'(lambda (sym)
                       (list sym
                             (gethash (symbol-name sym) lambda-list-hash)))
                   args)
       (locally
           (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
         (handler-bind
             (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
           ,@(mapcar #'(lambda (sym)
                         `(defmethod %to-json ((item (eql ,sym)))
                            (%write-string
                             ,(gethash (symbol-name sym) lambda-list-hash))))
                     args)))
       (let ((result (list (to-json (progn ,@body) :from ,from))))
         ,@(mapcar #'(lambda (sym)
                       `(setq result
                              (loop for item in result
                                    when (stringp item)
                                      do (multiple-value-bind (start end) (scan ,(gethash (symbol-name sym) lambda-list-hash) item)
                                           (when (and start end)
                                             (setf item
                                                   (list (subseq item 0 start)
                                                         ',sym
                                                         (subseq item end)))))
                                    nconc (ensure-list item))))
                   args)
         (eval
          `(lambda (,@',args)
             (let ((*stream* ,(if ,octet
                                  (make-output-buffer)
                                  (make-string-output-stream)))
                   (*octet* ,,octet))
               ,@(loop for item in result
                       if (stringp item)
                         collecting (if ,octet
                                        `(fast-write-sequence ,(babel:string-to-octets item) *stream*)
                                        `(write-string ,item *stream*))
                       else
                         collecting `(%to-json ,item))
               ,(if ,octet
                    `(finish-output-buffer *stream*)
                    `(get-output-stream-string *stream*)))))))))

