(in-package :cl-user)
(defpackage jonathan
  (:use :cl)
  (:import-from :jonathan.config
                :*application-root*
                :*static-directory*
                :*template-directory*)
  (:import-from :clack
                :clackup)
  (:import-from :jonathan.util
                :getapi
                :postapi
                :putapi
                :deleteapi)
  (:import-from :jonathan.web
                :*index-tmpl*)
  (:import-from :jonathan.app
                :make-app)
  (:export :*application-root*
           :*static-directory*
           :*template-directory*
           :*index-tmpl*
           :*app*
           :set-app
           :start
           :stop
           :start-routing-rules
           :getapi
           :postapi
           :putapi
           :deleteapi))
(in-package :jonathan)

(defvar *handler* nil)
(defvar *app* (make-app))

(defun set-app (app)
  (if (typep app 'PATHNAME)
    (setf *application-root* app)
    (setf *application-root* (asdf:system-source-directory app)))
  (setf *static-directory*   (merge-pathnames #P"static/" *application-root*))
  (setf *template-directory* (merge-pathnames #P"templates/" *application-root*))
  (setf *app* (make-app))
  t)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
                      :report "Restart the server"
                      (stop))))
  (setf *handler*
        (apply #'clackup *app* args)))

(defun stop ()
  (prog1
    (clack:stop *handler*)
    (setf *handler* nil)))

(defun start-routing-rules ()
  (setf (gethash *package* caveman2.app::*package-app-map*)
        (gethash (find-package :jonathan.web) caveman2.app::*package-app-map*)))
