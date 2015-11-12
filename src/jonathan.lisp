(in-package :cl-user)
(defpackage jonathan
  (:nicknames :jojo)
  (:use :cl)
  (:import-from :jonathan.error
                :<jonathan-error>
                :<jonathan-unexpected-eof-error>
                :<jonathan-incomplete-json-error>
                :<jonathan-not-supported-error>
                :<jonathan-without-tail-surrogate-error>)
  (:import-from :jonathan.encode
                :*octets*
                :*from*
                :write-key
                :write-value
                :write-key-value
                :with-object
                :with-array
                :write-item
                :with-output
                :to-json
                :%to-json
                :%write-char
                :%write-string)
  (:import-from :jonathan.decode
                :*false-value*
                :*null-value*
                :*empty-object-value*
                :*empty-array-value*
                :parse)
  (:import-from :jonathan.helper
                :with-output-to-string*
                :compile-encoder)
  (:export :*octets*
           :*from*
           :to-json
           :%to-json
           :%write-char
           :%write-string
           :*false-value*
           :*null-value*
           :*empty-object-value*
           :*empty-array-value*
           :<jonathan-error>
           :<jonathan-unexpected-eof-error>
           :<jonathan-incomplete-json-error>
           :<jonathan-not-supported-error>
           :<jonathan-without-tail-surrogate-error>
           :parse
           :write-key
           :write-value
           :write-key-value
           :with-object
           :with-array
           :write-item
           :with-output
           :with-output-to-string*
           :compile-encoder))
(in-package :jonathan)
