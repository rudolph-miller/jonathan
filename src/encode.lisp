(in-package :cl-user)
(defpackage jonathan.encode
  (:use :cl
        :annot.doc
        :jonathan.util)
  (:import-from :fast-io
                :fast-write-byte
                :make-output-buffer
                :finish-output-buffer)
  (:import-from :trivial-types
                :association-list-p)
  (:export :write-key
           :write-value
           :write-key-value
           :with-object
           :with-array
           :write-item
           :with-output
           :*octets*
           :*from*
           :*stream*
           :to-json
           :%to-json
           :%write-char
           :%write-string))
(in-package :jonathan.encode)

(syntax:use-syntax :cl-annot)

@doc
"Default value of octets used by #'to-json."
(defvar *octets* nil)

@doc
"Default value of from used by #'to-json."
(defvar *from* nil)

@doc
"Stream used by #'to-json."
(defvar *stream* nil)

(declaim (inline %write-string))
@doc
"Write string to *stream*."
(defun %write-string (string)
  (declare (type simple-string string)
           (optimize (speed 3) (safety 0) (debug 0)))
  (if *octets*
      (loop for c across string
            do (fast-write-byte (char-code c) *stream*))
      (write-string string *stream*))
  nil)

(declaim (inline %write-char))
@doc
"Write character to *stream*."
(defun %write-char (char)
  (declare (type character char)
           (optimize (speed 3) (safety 0) (debug 0)))
  (if *octets*
      (fast-write-byte (char-code char) *stream*)
      (write-char char *stream*))
  nil)

(declaim (inline string-to-json))
(defun string-to-json (string)
  (declare (type simple-string string)
           (optimize (speed 3) (safety 0) (debug 0)))
  (macrolet ((escape (char pairs)
               (declare (type list pairs))
               (let* ((sorted (sort (copy-list pairs) #'char<= :key #'car))
                      (min-char (caar sorted))
                      (max-char (caar (last sorted))))
                 `(if (and (char<= ,char ,max-char)
                           (char>= ,char ,min-char))
                      (cond
                        ,@(mapcar #'(lambda (pair)
                                      `((char= ,char ,(car pair))
                                        (%write-string ,(cdr pair))))
                                  pairs)
                        (t (%write-char ,char)))
                      (%write-char ,char)))))
    (%write-char #\")
    (loop for char of-type character across string
          do (escape char ((#\Newline . "\\n")
                           (#\Return . "\\r")
                           (#\Tab . "\\t")
                           (#\" . "\\\"")
                           (#\\ . "\\\\"))))
    (%write-char #\")))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-macro-p (list)
  `(and (consp ,list)
        (member (car ,list) '(with-object with-array)))))

#-allegro
(defmacro with-macro-p (list)
  `(and (consp ,list)
        (member (car ,list) '(with-object with-array))))

@doc
"Write key part of object."
(defmacro write-key (key)
  (declare (ignore key)))

@doc
"Write value part of object."
(defmacro write-value (value)
  (declare (ignore value)))

@doc
"Write key and value of object."
(defmacro write-key-value (key value)
  (declare (ignore key value)))

@doc
"Make writing object safe."
(defmacro with-object (&body body)
  (let ((first (gensym "first")))
    `(let ((,first t))
       (macrolet ((write-key (key)
                    `(progn
                       (if ,',first
                           (setq ,',first nil)
                           (%write-char #\,))
                       (string-to-json (princ-to-string ,key))))
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

@doc
"Write item of array."
(defmacro write-item (item)
  (declare (ignore item)))

@doc
"Make writing array safe."
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

@doc
"Bind *stream* to stream."
(defmacro with-output ((stream) &body body)
  `(let ((*stream* ,stream))
     ,@body))

(declaim (inline alist-to-json))
(defun alist-to-json (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-object
    (loop for (key . value) in list
          do (write-key-value key value))))

(declaim (inline plist-to-json))
(defun plist-to-json (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-object
    (loop for (key value) on list by #'cddr
          do (write-key-value key value))))

(declaim (inline list-to-json))
(defun list-to-json (list)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-array
    (loop for item in list
          do (write-item item))))

@doc
"Convert LISP object to JSON String."
(defun to-json (obj &key (octets *octets*) (from *from*))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((*stream* (if octets
                      (make-output-buffer :output :vector)
                      (make-string-output-stream)))
        (*octets* octets)
        (*from* from))
    (%to-json obj)
    (if octets
        (finish-output-buffer *stream*)
        (get-output-stream-string *stream*))))

@doc
"Write obj as JSON string."
(defgeneric %to-json (obj))

(defmethod %to-json ((string string))
  (if (typep string 'simple-string)
      (string-to-json string)
      (string-to-json (coerce string 'simple-string))))

(defmethod %to-json ((number number))
  (%write-string (princ-to-string number)))

(defmethod %to-json ((float float))
  (%write-string (format nil "~f" float)))

(defmethod %to-json ((ratio ratio))
  (%write-string (princ-to-string (coerce ratio 'float))))

(defmethod %to-json ((list list))
  (cond
    ((and (eq *from* :alist)
          (association-list-p list)
          ;; check if is alist key atom.
          (atom (caar list)))
     (alist-to-json list))
    ((and (eq *from* :jsown)
          (eq (car list) :obj))
     (alist-to-json (cdr list)))
    ((and (or (eq *from* :plist)
              (null *from*))
          (my-plist-p list))
     (plist-to-json list))
    (t (list-to-json list))))

(defmethod %to-json ((vector vector))
  (with-array
    (loop for item across vector
          do (write-item item))))

(defmethod %to-json ((hash hash-table))
  (with-object
    (loop for key being the hash-key of hash
            using (hash-value value)
          do (write-key-value key value))))

(defmethod %to-json ((symbol symbol))
  (string-to-json (symbol-name symbol)))

(defmethod %to-json ((_ (eql t)))
  (%write-string "true"))

(defmethod %to-json ((_ (eql :false)))
  (%write-string "false"))

(defmethod %to-json ((_ (eql :null)))
  (%write-string "null"))

(defmethod %to-json ((_ (eql :empty)))
  (%write-string "{}"))

(defmethod %to-json ((_ (eql nil)))
  (%write-string "[]"))
