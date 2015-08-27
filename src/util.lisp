(in-package :cl-user)
(defpackage jonathan.util
  (:use :cl)
  (:export :my-plist-p
           :integer-char-p
           :make-keyword
           :comma-p
           :comma-expr
           :*quasiquote*))
(in-package :jonathan.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (when (find-package :sb-impl)
    (push :sb-impl *features*)))

(defun my-plist-p (list)
  (typecase list
    (null t)
    (cons (loop for (key val next) on list by #'cddr
                if (not (keywordp key))
                  return nil
                else
                  unless next return t))))

(declaim (inline integer-char-p))
(defun integer-char-p (char)
  (or (char<= #\0 char #\9)
      (char= char #\-)))

(defun make-keyword (str)
  (intern str #.(find-package :keyword)))

(defun comma-p (comma)
  #+sb-impl
  (sb-impl::comma-p comma)
  #-sb-impl
  (error "Not supported."))

(defun comma-expr (comma)
  #+sb-impl
  (sb-impl::comma-expr comma)
  #-sb-impl
  nil)

(defvar *quasiquote*
  #+sb-impl
  'sb-int:quasiquote
  #-sb-impl
  nil)
