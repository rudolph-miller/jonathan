(in-package :cl-user)
(defpackage jonathan.util
  (:use :cl)
  (:export :my-plist-p
           :integer-char-p
           :make-keyword))
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
