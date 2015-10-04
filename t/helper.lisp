(in-package :cl-user)
(defpackage jonathan-test.helper
  (:use :cl
        :prove
        :jonathan)
  (:import-from :jonathan.helper
                :normalize-form
                :replace-form-with-placeholders))
(in-package :jonathan-test.helper)

(diag "jonathan-test.helper")

(plan 5)

(subtest "with-output-to-string*"
  (is (with-output-to-string*
        (with-object
          (write-key-value "key" "value")))
      "{\"key\":\"value\"}"
      "can return encoded string."))

(defmacro normalize-form-test (target expected comment &key including-variable)
  `(is ,(if including-variable
            `(normalize-form ',target)
            `(eval (normalize-form ',target)))
       ,(if including-variable
            `',expected
            `(eval ',expected))
       ,comment))

(subtest "normalize-form"
  (normalize-form-test
   :a
   :a
   "with :keyword.")

  (normalize-form-test
   'a
   'a
   "with symbol.")

  (normalize-form-test
   (list :a :b)
   (list :a :b)
   "with list.")

  (normalize-form-test
   (list* :a :b)
   (list* :a :b)
   "with list*.")

  (normalize-form-test
   '(:a . :b)
   (list* :a :b)
   "with dotted pair.")

  (normalize-form-test
   '(:a :b)
   (list :a :b)
   "with QUOTE.")

  (normalize-form-test
   `:a
   :a
   "with QUASIQUOTE.")

  (normalize-form-test
   `(:a :b (:c :d))
   (list :a :b (list :c :d))
   "with QUASIQUOTE and cons.")

  (normalize-form-test
   (list a b)
   (list a b)
   "with variable."
   :including-variable t)

  (normalize-form-test
   `(a . ,"b")
   (list* 'a "b")
   "with unquote.")

  (normalize-form-test
   (list (princ-to-string a))
   (list (princ-to-string a))
   "with function."
   :including-variable t))

(defmacro replace-form-with-placeholders-test (target expected comment)
  `(is (replace-form-with-placeholders ',target)
       ',expected
       ,comment
       :test #'(lambda (a b)
                 (if (consp a)
                     (loop for i in a
                           for j in b
                           always (or (and (eql j :placeholder)
                                           (typep i 'string))
                                      (equal i j)))
                     (or (and (eql b :placeholder)
                              (typep a 'string))
                         (equal a b))))))

(subtest "replace-form-with-placeholders"
  (replace-form-with-placeholders-test
   :a
   :a
   "with :keyword.")

  (replace-form-with-placeholders-test
   'a
   'a
   "with QUOTE.")

  (replace-form-with-placeholders-test
   "a"
   "a"
   "with string.")

  (replace-form-with-placeholders-test
   a
   :placeholder
   "with variable.")

  (replace-form-with-placeholders-test
   (list 'a a)
   (list 'a :placeholder)
   "with list.")

  (replace-form-with-placeholders-test
   (list* 'a a)
   (list* 'a :placeholder)
   "with list*.")

  (replace-form-with-placeholders-test
   (princ-to-string a)
   :placeholder
   "with function."))

(subtest "compile-encoder"
  (let ((encoder1 (compile-encoder () (value)
                    (list :key value)))
        (encoder2 (compile-encoder (:from :alist) (value)
                    `(("key" . ,value))))
        (encoder3 (compile-encoder (:octets t) (value)
                    (list :key value)))
        (encoder4 (compile-encoder () (value)
                    (progn (incf value) (list :key value))))
        (encoder5 (compile-encoder () (value)
                    `(,@(list :key value)))))

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

    (is (funcall encoder4 0)
        "{\"KEY\":1}"
        "with PROGN.")

    ;; https://github.com/Rudolph-Miller/jonathan/issues/29
    (is (funcall encoder5 "value")
        "{\"KEY\":\"value\"}"
        "with unsupported comma.")

    (is-error (eval '(compile-encoder () (value value)))
              'simple-error
              "can raise the error with duplicate variables.")

    (is-error (eval '(compile-encoder () ("value")))
              'simple-error
              "can raise the error with non-symbol in lambda-list.")

    (is-error (eval '(compile-encoder () (:value)))
              'simple-error
              "can raise the error with keyword in lambda-list.")))

(subtest "compiler-macro"
  (flet ((sample (name)
           (to-json (list :name name))))
    (is (sample "Rudolph")
        "{\"NAME\":\"Rudolph\"}"
        "without any keyword arguments."))

  (flet ((sample (name)
           (to-json (list (cons :name name)) :from :alist :octets t)))
    (is (sample "Rudolph")
        #(123 34 78 65 77 69 34 58 34 82 117 100 111 108 112 104 34 125)
        "with :from and :octets."
        :test #'equalp))

  (flet ((sample (name)
           (to-json `((:name . ,name)) :from :alist :octets t)))
    (is (sample "Rudolph")
        #(123 34 78 65 77 69 34 58 34 82 117 100 111 108 112 104 34 125)
        "with special-operations."
        :test #'equalp))

  (flet ((sample (name age)
           (to-json `((:name . ,(princ-to-string name)) (:age . ,(princ-to-string age))) :from :alist)))
    (is (sample "Rudolph" 22)
        "{\"NAME\":\"Rudolph\",\"AGE\":\"22\"}"
        "with unquote function."
        :test #'equalp)))

(finalize)
