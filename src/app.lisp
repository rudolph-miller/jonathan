(in-package :cl-user)
(defpackage jonathan.app
  (:use :cl)
  (:import-from :clack
                :call)
  (:import-from :clack.builder
                :builder)
  (:import-from :clack.middleware.static
                :<clack-middleware-static>)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:import-from :clack.middleware.csrf
                :<clack-middleware-csrf>)
  (:import-from :clack.middleware.accesslog
                :<clack-middleware-accesslog>)
  (:import-from :clack.middleware.backtrace
                :<clack-middleware-backtrace>)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :jonathan.web
                :*web*)
  (:import-from :jonathan.config
                :*static-directory*)
  (:export :make-app))
(in-package :jonathan.app)

(defun make-app () ;;TODO: take addon middleware
  (builder
    (<clack-middleware-static>
      :path (lambda (path)
              (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon.ico$)" path)
                path
                nil))
      :root *static-directory*)
    <clack-middleware-session>
    <clack-middleware-csrf>
    *web*))
