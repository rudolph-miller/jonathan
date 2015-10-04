(in-package :cl-user)
(defpackage jonathan.util
  (:use :cl
        :jonathan.error)
  (:export :+impl-comma-p+
           :my-plist-p
           :integer-char-p
           :make-keyword
           :comma-p
           :comma-expr
           :*quasiquote*))
(in-package :jonathan.util)

(defparameter +impl-comma-p+ (and (find-package :sb-impl)
                                (find-symbol "COMMA-P" :sb-impl)
                                (find-symbol "COMMA-EXPR" :sb-impl)
                                t))

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
  (if +impl-comma-p+
      (when (uiop:symbol-call :sb-impl "COMMA-P" comma)
        (if (= (uiop:symbol-call :sb-impl "COMMA-KIND" comma) 0)
            ;; (comma-kind comma
            ;; => 0: just only comma
            ;;    1: with dot
            ;;    2: with at
            t
            (error '<jonathan-not-supported-error> :object "Comma with dot or at")))
      (error '<jonathan-not-supported-error> :object " Comma")))

(defun comma-expr (comma)
  (if +impl-comma-p+
      (uiop:symbol-call :sb-impl "COMMA-EXPR" comma)
      nil))

(defvar *quasiquote* (if +impl-comma-p+
                         (find-symbol "QUASIQUOTE" :sb-int)
                         nil))
