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
  (:import-from :jonathan.helper
                :write-key
                :write-value
                :write-key-value
                :with-object
                :with-array
                :write-item
                :with-output
                :with-output-to-string*)
  (:export :*octet*
           :*from*
           :to-json
           :%to-json
           :%write-char
           :%write-string
           :<jonathan-unexpected-eof>
           :parse
           :write-key
           :write-value
           :write-key-value
           :with-object
           :with-array
           :write-item
           :with-output
           :with-output-to-string*))
(in-package :jonathan)
