(in-package :cl-user)
(defpackage jonathan.config
  (:use :cl)
  (:export :*application-root*
           :*static-directory*
           :*template-directory*))
(in-package :jonathan.config)

(defparameter *application-root*   (asdf:system-source-directory :jonathan))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))
