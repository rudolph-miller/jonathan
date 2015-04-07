(in-package :cl-user)
(defpackage jonathan.helper
  (:use :cl
        :jonathan.util
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
                :last-elt
                :starts-with-subseq)
  (:import-from :trivial-types
                :proper-list-p)
  (:import-from :annot.util
                :progn-form-last
                :progn-form-replace-last)
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

(defun normalize-form (object)
  (flet ((uncomma-when-comma (item)
           #+sbcl
           (if (comma-p item)
               (normalize-form (comma-expr item))
               (if (or (keywordp item)
                       (stringp item))
                   item
                   (list 'quote item)))
           #-sbcl
           (error "Not supported."))
         (make-quote (sym)
           (if (or (keywordp sym)
                   (stringp sym))
               sym
               (list 'quote sym)))
         (map-dotted-list (fn list)
           (loop for (item . rest) on list
                 collecting (funcall fn item)
                 when (not (consp rest))
                   collecting (funcall fn rest))))
    (if (consp object)
        (let ((sym-name (symbol-name (car object))))
          (cond
            ((or (equal sym-name "LIST")
                 (equal sym-name "LIST*"))
             (mapcar #'normalize-form object))
            ((equal sym-name "CONS")
             (cons 'list* (cdr object)))
            ((equal sym-name "QUOTE")
             (cond
               ((proper-list-p (cadr object))
                (cons 'list (mapcar #'make-quote (cadr object))))
               ((consp (cadr object))
                (cons 'list* (mapcar #'make-quote (cadr object))))
               (t object)))
            ((equal sym-name "QUASIQUOTE")
             (cond
               ((proper-list-p (cadr object))
                (cons 'list (mapcar #'(lambda (item)
                                        (cond
                                          ((proper-list-p item)
                                           (normalize-form (list *quasiquote* item)))
                                          ((consp item)
                                           (cons 'list* (map-dotted-list #'uncomma-when-comma item)))
                                          (t (uncomma-when-comma item))))
                                    (cadr object))))
               ((consp (cadr object))
                (cons 'list* (map-dotted-list #'uncomma-when-comma (cadr object))))
               (t object)))
            (t object)))
        object)))

(defun replace-form-with-placeholders (form)
  (let ((placeholders (make-hash-table :test #'equal)))
    (flet ((genstr () (symbol-name (gensym *compile-encoder-prefix*))))
      (labels ((sub (object)
                 (etypecase object
                   (string object)
                   (keyword object)
                   (symbol (setf (gethash object placeholders) (genstr)))
                   (cons (let ((sym-name (symbol-name (car object))))
                           (cond
                             ((equal sym-name "LIST*")
                              (cons 'list* (mapcar #'sub (cdr object))))
                             ((equal sym-name "LIST")
                              (cons 'list (mapcar #'sub (cdr object))))
                             ((equal sym-name "QUOTE")
                              object)
                             (t (setf (gethash object placeholders) (genstr)))))))))
        (values (sub form) placeholders)))))

(defmacro compile-encoder ((&key octets from return-form) (&rest args) &body body)
  (check-args args)
  (let* ((main (last-elt body))
         (progn-p (and (consp main)
                       (eql (car main) 'progn))))
    (when progn-p
      (setq main (progn-form-last main)))
    (multiple-value-bind (form placeholders) (replace-form-with-placeholders (normalize-form main))
      `(let* ((*from* ,from)
              (result (list (with-output-to-string* (%to-json ,form)))))
         ,@(loop for key being the hash-keys of placeholders
                   using (hash-value val)
                 collecting `(setq result
                                   (loop for item in result
                                         when (stringp item)
                                           do (multiple-value-bind (start end)
                                                  (scan (with-output-to-string*
                                                          (%to-json ,val))
                                                        item)
                                                (when (and start end)
                                                  (setf item
                                                        (list (subseq item 0 start)
                                                              ',key
                                                              (subseq item end)))))
                                         nconc (ensure-list item))))
         (let ((form `(let ((*stream* (load-time-value
                                       ,(if ,octets
                                            `(make-output-buffer :output :vector)
                                            `(make-string-output-stream :element-type 'character))))
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
           (when ,progn-p
             (setq form (progn-form-replace-last form ',(last-elt body))))
           (setf (last-elt ',body) form)
           (if ,return-form
               ',body
               (eval `(lambda (,@',args) ,@',body))))))))

(defun variable-p (sym)
  (handler-case
      (typecase sym
        (keyword nil)
        (symbol (when (or (constantp sym)
                          (symbol-function sym))
                  nil))
        (t (let ((str (princ-to-string sym)))
             (if (starts-with-subseq "," str)
                 (intern (subseq str 1))
                 nil))))
    (undefined-function () sym)))

(defun collect-variables (list)
  (remove-duplicates
   (if (consp list)
       (if (and (atom (car list))
                (symbolp (car list))
                (special-operator-p (car list)))
           (collect-variables (cdr list))
           (loop for item on list
                 nconc (collect-variables (car item))
                 when (and (not (consp (cdr item))) (not (null (cdr item))))
                   nconc (collect-variables (cdr item))
                 while (consp (cdr item))))
       (ensure-list (variable-p list)))))

(define-compiler-macro to-json (&whole form args &key from octets)
  (handler-case
      (let ((variables (collect-variables args)))
        `(progn
           ,@(eval
              `(compile-encoder (:from ,from :octets ,octets :return-form t) ,variables
                 ,args))))
    (error () form)))
