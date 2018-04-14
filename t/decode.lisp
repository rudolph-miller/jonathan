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

(plan 32)

(defvar *upper-exponent* (gensym "upper"))
(defvar *lower-exponent* (gensym "lower"))
(defvar *exponent-with-plus-sign* (gensym "plus-sign"))
(defvar *exponent-with-minus-sign* (gensym "minus-sign"))

(defun exponent-p (obj)
  (when (member obj (list *upper-exponent* *lower-exponent* *exponent-with-plus-sign* *exponent-with-minus-sign*))
    t))

(defun exponent-value (obj)
  (cond
    ((eql obj *exponent-with-minus-sign*) 123e-4)
    (t 123e4)))

(defmethod %to-json ((obj (eql *upper-exponent*)))
  (%write-string "123E4"))

(defmethod %to-json ((obj (eql *lower-exponent*)))
  (%write-string "123e4"))

(defmethod %to-json ((obj (eql *exponent-with-plus-sign*)))
  (%write-string "123e+4"))

(defmethod %to-json ((obj (eql *exponent-with-minus-sign*)))
  (%write-string "123e-4"))

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
           (if (exponent-p ,target)
               (exponent-value ,target)
               (case ,target
                 (:false *false-value*)
                 (:null *null-value*)
                 (t (if (null ,target)
                        *empty-array-value*
                        ,target))))
           ":as :plist."
           :test #'equalp)
       (is (parse (to-json ,target) :as :alist)
           (if (my-plist-p ,target)
               (plist-alist ,target)
               (if (exponent-p ,target)
                   (exponent-value ,target)
                   (case ,target
                     (:false *false-value*)
                     (:null *null-value*)
                     (t (if (null ,target)
                            *empty-array-value*
                            ,target)))))
           ":as :alist."
           :test #'equalp)
       (is (parse (to-json ,target) :as :jsown)
           (if (my-plist-p ,target)
               (let ((alist (plist-alist ,target)))
                 (if (eq alist *empty-array-value*)
                     *empty-array-value*
                     (cons :obj alist)))
               (if (exponent-p ,target)
                   (exponent-value ,target)
                   (case ,target
                     (:false *false-value*)
                     (:null *null-value*)
                     (t (if (null ,target)
                            *empty-array-value*
                            ,target)))))
           ":as :jsown."
           :test #'equalp)
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
                 (if (exponent-p ,target)
                     (exponent-value ,target)
                     (convert-to-hash-result ,target)))
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

(parse-test 0.1
            "with float.")

(parse-test -0.0
            "with negative float zero.")

(parse-test -0.1
            "with negative float.")

(parse-test -9.9
            "with negative float.")

(parse-test *upper-exponent*
            "with E.")

(parse-test *lower-exponent*
            "with e.")

(parse-test *exponent-with-plus-sign*
            "with e+.")

(parse-test *exponent-with-minus-sign*
            "with e-.")

(parse-test '(:Rudolph "Miller")
            "with object.")

(parse-test :empty
            "with empty objectu.")

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

(subtest ":normalize-all"
  (flet ((normalizer (key)
           (when (equal key "key1")
             "other-key")))
    (is (parse "{\"key1\":{\"key2\":\"value2\"}}"
               :keyword-normalizer #'normalizer
               :normalize-all nil)
        '(:|other-key| (:|key2| "value2"))
        "NIL.")

    (is (parse "{\"key1\":{\"key2\":\"value2\"}}"
               :keyword-normalizer #'normalizer
               :normalize-all t)
        '(:|other-key| nil)
        "T.")))

(subtest ":exclude-normalize-keys"
  (flet ((normalizer (key)
           (when (equal key "key1")
             "other-key")))
    (is (parse "{\"key1\":{\"key2\":\"value2\"}}"
               :keyword-normalizer #'normalizer
               :normalize-all t
               :exclude-normalize-keys nil)
        '(:|other-key| nil)
        "NIl.")

    (is (parse "{\"key1\":{\"key2\":\"value2\"}}"
               :keyword-normalizer #'normalizer
               :normalize-all t
               :exclude-normalize-keys '("other-key"))
        '(:|other-key| (:|key2| "value2"))
        "specified.")))

(subtest ":unescape-unicode-escape-sequence"
  (subtest "T"
    (is (parse "\"\\u30b8\\u30e7\\u30ca\\u30b5\\u30f3\"")
        "ジョナサン"
        "without surrogate pair.")
    #+(or :sbcl :clisp :ccl)
    (is (parse "\"\\uD840\\uDC0B\"")
        "𠀋"
        "with surrogate pair.")
    (is (parse "\"\\uD83D\\uDE3E\\uD83D\\uDD2A\"")
        (concatenate 'string
                     (parse "\"\\uD83D\\uDE3E\"")
                     (parse "\"\\uD83D\\uDD2A\""))
        "surrogate pairs should be parsed equally when they follow together and separate from each other."))

  (subtest "NIL"
    (is (parse "\"\\u30b8\\u30e7\\u30ca\\u30b5\\u30f3\""
               :unescape-unicode-escape-sequence nil)
        "\u30b8\u30e7\u30ca\u30b5\u30f3"
        "pass.")))

(subtest "<jonathan-without-tail-surrogate-error>"
  (is-error (parse "\"\\uD840\"")
            '<jonathan-without-tail-surrogate-error>
            "with no tail surrogate.")

  (is-error (parse "\"\\uD840\\uD840\"")
            '<jonathan-without-tail-surrogate-error>
            "with not valid tail surrogate.")) 

(subtest "foldable-keywords-to-read-p"
  (ok (foldable-keywords-to-read-p ''("key"))
      "with QUOTE.")

  (ok (foldable-keywords-to-read-p '(list "key"))
      "with LIST.")

  (ok (not (foldable-keywords-to-read-p 'x))
      "with variable."))

(subtest "double-float"
  (is (parse "35.65910807942215")
      35.65910807942215d0
      "Can parse double-float")
  (is (parse "139.70372892916203")
      139.70372892916203d0
      "Can parse double-float")
  (is (parse "35.659108")
      35.659108
      "Can parse single-float")
  (is (parse "-35.65910807942215")
      -35.65910807942215d0
      "Can parse negative double-float")
  (is (parse "-139.70372892916203")
      -139.70372892916203d0
      "Can parse negative double-float")
  (is (parse "-35.659108")
      -35.659107
      "Can parse negative single-float"))

(finalize)
