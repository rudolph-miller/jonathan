(in-package :cl-user)
(defpackage jonathan-test.helper
  (:use :cl
        :prove
        :jonathan))
(in-package :jonathan-test.helper)

(diag "jonathan-test.helper")

(plan 2)

(subtest "with-output-to-string*"
  (is (with-output-to-string*
        (with-object
          (write-key-value "key" "value")))
      "{\"key\":\"value\"}"
      "can return encoded string."))

(subtest "compile-encoder"
  (let ((encoder1 (compile-encoder () (value)
                    (list :key value)))
        (encoder2 (compile-encoder (:from :alist) (value)
                    `(("key" . ,value))))
        (encoder3 (compile-encoder (:octets t) (value)
                    (list :key value))))

    (is (funcall encoder1 "value")
        "{\"KEY\":\"value\"}"
        "without options.")

    (is (funcall encoder2 "value")
        "{\"key\":\"value\"}"
        "with :from.")

    (is (funcall encoder3 "value")
        #(123 34 75 69 89 34 58 34 118 97 108 117 101 34 125)
        "with :octets T."
        :test #'equalp)

    (is-error (eval '(compile-encoder () (value value)))
              'simple-error
              "can raise the error with duplicate variables.")

    (is-error (eval '(compile-encoder () ("value")))
              'simple-error
              "can raise the error with non-symbol in lambda-list.")

    (is-error (eval '(compile-encoder () (:value)))
              'simple-error
              "can raise the error with keyword in lambda-list.")))

(finalize)
