(in-package :cl-user)
(defpackage jonathan.web
  (:use :cl
        :caveman2
        :jonathan.config
        :jonathan.util)
  (:export :*index-tmpl*
           :*web*))
(in-package :jonathan.web)

(defparameter *index-tmpl* (merge-pathnames #P"index.tmpl" *template-directory*))

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(Defroute ("^(?!^\/api\/).+" :regexp t) ()
  (let ((emb:*escape-type* :html)
        (emb:*case-sensitivity* nil))
    (emb:execute-emb *index-tmpl*)))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
