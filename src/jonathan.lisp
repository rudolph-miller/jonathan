(in-package :cl-user)
(defpackage jonathan
  (:use :cl)
  (:import-from :jonathan.encode
                :*octet*
                :*from*
                :to-json
                :%to-json
                :%write-char
                :%write-string)
  (:import-from :jonathan.decode
                :parse)
  (:export :*octet*
           :*from*
           :to-json
           :%to-json
           :%write-char
           :%write-string
           :parse))
(in-package :jonathan)
