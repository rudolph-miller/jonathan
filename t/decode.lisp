(in-package :cl-user)
(defpackage jonathan-test.decode
  (:use :cl
        :prove
        :jonathan.util
        :jonathan)
  (:import-from :alexandria
                :plist-hash-table)
  (:import-from :jonathan.decode
                :foldable-keywords-to-read-p))
(in-package :jonathan-test.decode)

(diag "jonathan-test.decode")

(plan 23)

(defun plist-alist (plist)
  (if (my-plist-p plist)
      (if (null plist)
          *empty-array-value*
          (mapcar #'(lambda (item)
                      (cons (symbol-name (car item))
                            (plist-alist (cdr item))))
                  (alexandria:plist-alist plist)))
      plist))

(defmacro parse-test (target comment)
  `(subtest ,comment
     (let ((*false-value* :false)
           (*null-value* :null)
           (*empty-object-value* :empty)
           (*empty-array-value* :[]))
       (is (parse (to-json ,target))
           (case ,target
             (:false *false-value*)
             (:null *null-value*)
             (t (if (null ,target)
                    *empty-array-value*
                    ,target)))
           ":as :plist.")
       (is (parse (to-json ,target) :as :alist)
           (if (my-plist-p ,target)
               (plist-alist ,target)
               (case ,target
                 (:false *false-value*)
                 (:null *null-value*)
                 (t (if (null ,target)
                        *empty-array-value*
                        ,target))))
           ":as :alist.")
       (is (parse (to-json ,target) :as :jsown)
           (if (my-plist-p ,target)
               (let ((alist (plist-alist ,target)))
                 (if (eq alist *empty-array-value*)
                     *empty-array-value*
                     (cons :obj alist)))
               (case ,target
                 (:false *false-value*)
                 (:null *null-value*)
                 (t (if (null ,target)
                        *empty-array-value*
                        ,target))))
           ":as :jsown.")
       (flet ((convert-to-hash-result (target)
               (case target
                 (:false *false-value*)
                 (:null *null-value*)
                 (:empty (make-hash-table :test #'equal))
                 (t (if (null target)
                        *empty-array-value*
                        target)))))
         (is (parse (to-json ,target) :as :hash-table)
             (if (my-plist-p ,target)
                 (if (null ,target)
                     *empty-array-value*
                     (loop with result = (make-hash-table :test #'equal)
                           for (key value) on ,target by #'cddr
                           do (setf (gethash (symbol-name key) result)
                                    (convert-to-hash-result value))
                           finally (return result)))
                 (convert-to-hash-result ,target))
             ":as :hash-table."
             :test #'equalp)))))

(parse-test t
            "with T.")

(parse-test nil
            "with NIL.")

(parse-test :false
            "with :false.")

(parse-test :null
            "with :null.")

(parse-test "Rudolph"
            "with string.")

(parse-test "Rudolph\\b"
            "with #\Backspace.")

(parse-test "Rudolph\\f"
            "with #\Linefeed.")

(parse-test "Rudolph\\n"
            "with #\Newline.")

(parse-test "Rudolph\\r"
            "with #\Return.")

(parse-test "Rudolph\\t"
            "with #\Tab.")

(parse-test '("Rudolph" "Miller")
            "with list.")

(parse-test 1
            "with integer.")

(parse-test -1
            "with negative.")

(parse-test (/ 1 10)
            "with float.")

(parse-test '(:Rudolph "Miller")
            "with object.")

(parse-test :empty
            "with empty object.")

(parse-test '(:Rudolph :empty)
            "with object which have empty object in its value part.")

(subtest "<jonathan-unexpected-eof>"
  (is-error (parse "{\key\":\"value\"")
            '<jonathan-unexpected-eof-error>
            "without }.")

  (is-error (parse "{\key\":\"value")
            '<jonathan-unexpected-eof-error>
            "without \".")

  (is-error (parse "{\key\":\"")
            '<jonathan-unexpected-eof-error>
            "without any characters after \".")

  (is-error (parse "{\key\":")
            '<jonathan-unexpected-eof-error>
            "without any value after :.")

  (is-error (parse "{\key\"")
            '<jonathan-unexpected-eof-error>
            "without :.")

  (is-error (parse "{")
            '<jonathan-unexpected-eof-error>
            "without any keys after {."))

(subtest "<jonathan-incomplete-json-error>"
  (is-error (parse "anything")
            '<jonathan-incomplete-json-error>
            "with any other characters."))

(subtest ":junk-allowed t"
  (ok (parse "{\"key\":\"value\"" :junk-allowed t)
      "can allow incomplete JSON foramt string."))

(subtest ":keywords-to-read"
  (is (parse "{\"key1\":{\"key2\":\"value2\"},\"key3\":\"value3\",\"key4\":1.1,\"key5\":[1,2]}"
             :keywords-to-read '("key1"))
      '(:|key1| (:|key2| "value2"))
      "can restrict keywords to read.")

  (is (parse "{\"key1\":{\"key2\":\"value2\"},\"key3\":\"value3\",\"key4\":1.1,\"key5\":[1,2]}"
             :keywords-to-read '(x))
      nil
      "with symbols in :keywords-to-read."))

(subtest ":keyword-normalizer"
  (flet ((normalizer (key)
           (when (equal key "key1")
             "other-key")))
    (is (parse "{\"key1\":{\"key2\":\"value2\"},\"key3\":\"value3\",\"key4\":1.1,\"key5\":[1,2]}"
               :keyword-normalizer #'normalizer)
        '(:|other-key| (:|key2| "value2"))
        "can normalize keywords.")))

(subtest "foldable-keywords-to-read-p"
  (ok (foldable-keywords-to-read-p ''("key"))
      "with QUOTE.")

  (ok (foldable-keywords-to-read-p '(list "key"))
      "with LIST.")

  (ok (not (foldable-keywords-to-read-p 'x))
      "with variable."))

(finalize)
