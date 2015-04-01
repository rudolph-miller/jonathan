(in-package :cl-user)
(defpackage jonathan.helper
  (:use :cl
        :jonathan.encode)
  (:import-from :babel
                :string-to-octets)
  (:import-from :fast-io
                :fast-write-sequence
                :make-output-buffer
                :finish-output-buffer)
  (:import-from :ppcre
                :scan)
  (:import-from :alexandria
                :length=
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

(defun check-args (args)
  (let ((passed))
    (dolist (item args)
    (etypecase item
      (keyword (error (format nil "~s is a keyword, and cannot be used as a local variable." item)))
      (symbol t)
      (t (error (format nil "Required argument is not a symbol: ~s" item))))
      (if (member item passed)
          (error (format nil "The variable ~s occurs more than once in the lambda list." item))
          (push item passed)))
    t))

(defun check-duplicates (list)
  (not (length= list (remove-duplicates list))))

(defmacro compile-encoder ((&key octets from) (&rest args) &body body)
  (check-args args)
  `(let* ,(append (mapcar #'(lambda (sym)
                              (list sym
                                    (symbol-name (gensym *compile-encoder-prefix*))))
                          args)
                  `((result (list (to-json (progn ,@body) :from ,from)))))
     ,@(mapcar #'(lambda (sym)
                   `(setq result
                          (loop for item in result
                                when (stringp item)
                                  do (multiple-value-bind (start end)
                                         (scan (with-output-to-string*
                                                 (%to-json ,sym))
                                               item)
                                       (when (and start end)
                                         (setf item
                                               (list (subseq item 0 start)
                                                     ',sym
                                                     (subseq item end)))))
                                nconc (ensure-list item))))
               args)
     (eval
      `(lambda (,@',args)
         (let ((*stream* ,(if ,octets
                              (make-output-buffer :output :vector)
                              (make-string-output-stream)))
               (*octets* ,,octets))
           ,@(loop for item in result
                   if (stringp item)
                     collecting (if ,octets
                                    `(fast-write-sequence ,(string-to-octets item) *stream*)
                                    `(write-string ,item *stream*))
                   else
                     collecting `(%to-json ,item))
           ,(if ,octets
                `(finish-output-buffer *stream*)
                `(get-output-stream-string *stream*)))))))
