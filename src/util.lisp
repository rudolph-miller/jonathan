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
  #+sbcl
  (sb-impl::comma-p comma)
  #-sbcl
  (error "Not supported."))

(defun comma-expr (comma)
  #+sbcl
  (sb-impl::comma-expr comma)
  #-sbcl
  nil)

(defvar *quasiquote*
  #+sbcl
  'sb-int:quasiquote
  #-sbcl
  nil)
