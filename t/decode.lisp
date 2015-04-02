(in-package :cl-user)
(defpackage jonathan-test.decode
  (:use :cl
        :prove
        :jonathan.util
        :jonathan)
  (:import-from :alexandria
                :plist-hash-table))
(in-package :jonathan-test.decode)

(diag "jonathan-test.decode")

(plan 17)

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
       (is (parse (to-json ,target) :as :hash-table)
           (if (my-plist-p ,target)
               (if (null ,target)
                   *empty-array-value*
                   (loop with result = (make-hash-table :test #'equal)
                         for (key value) on ,target by #'cddr
                         do (setf (gethash (symbol-name key) result) value)
                         finally (return result)))
               (case ,target
                 (:false *false-value*)
                 (:null *null-value*)
                 (t (if (null ,target)
                        *empty-array-value*
                        ,target))))
           ":as :hash-table."
           :test #'equalp))))

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

(subtest ":junk-allowed t"
  (ok (parse "{\key\":\"value\"" :junk-allowed t)
      "can allow incomplete JSON foramt string."))

(finalize)
