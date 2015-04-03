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
                :ensure-list
                :starts-with-subseq)
  (:import-from :trivial-types
                :proper-list-p)
  (:export :with-output-to-string*
           :compile-encoder))
(in-package :jonathan.helper)

(defvar *compile-encoder-prefix* "jonathan-encoder")

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

(defmacro compile-encoder ((&key octets from return-form unsafely) (&rest args) &body body)
  (check-args args)
  `(let* ,(append (mapcar #'(lambda (sym)
                              (list sym
                                    (symbol-name (gensym *compile-encoder-prefix*))))
                          args)
                  `((result (list (to-json (progn ,@body) :from ,from :dont-compile t)))))
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
     (let ((form `(let ((*stream* ,(let ((make-buffer (if ,octets
                                                     `(make-output-buffer :output :vector)
                                                     `(make-string-output-stream :element-type 'character))))
                                     (if (or (not ,return-form) ,unsafely)
                                         (eval make-buffer)
                                         make-buffer)))
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
                         `(get-output-stream-string *stream*)))))
       (if ,return-form
           form
           (eval `(lambda (,@',args) ,form))))))

(defun variable-p (sym)
  (handler-case
      (typecase sym
        (keyword nil)
        (symbol (when (or (constantp sym)
                          (symbol-function sym))
                  nil))
        (t (let ((str (princ-to-string sym)))
             (if (starts-with-subseq "," str)
                 (intern (subseq sym 1))
                 nil))))
    (undefined-function () sym)))

(defun collect-variables (list)
  (remove-duplicates
   (if (consp list)
       (if (and (symbolp (car list))
                (equal (symbol-name (car list))
                       "QUASIQUOTE"))
           (collect-variables (cdr list))
           (if (and (atom (car list))
                    (symbolp (car list))
                    (special-operator-p (car list)))
               nil
               (loop for item on list
                     nconc (collect-variables (car item))
                     when (and (not (consp (cdr item))) (not (null (cdr item))))
                       nconc (collect-variables (cdr item))
                     while (consp (cdr item)))))
       (ensure-list (variable-p list)))))

(define-compiler-macro to-json (&whole form args &key from octets dont-compile compile-unsafely)
  (handler-case
      (if (not dont-compile)
          (let ((variables (collect-variables args)))
            (eval
             `(compile-encoder (:from ,from :octets ,octets :return-form t :unsafely ,compile-unsafely) ,variables
                ,args)))
          form)
    (error () form)))
