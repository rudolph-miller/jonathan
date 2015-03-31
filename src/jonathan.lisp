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
                :<jonathan-unexpected-eof-error>
                :parse)
  (:export :*octet*
           :*from*
           :to-json
           :%to-json
           :%write-char
           :%write-string
           :<jonathan-unexpected-eof>
           :parse))
(in-package :jonathan)
