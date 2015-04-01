(in-package :cl-user)
(defpackage jonathan-test.decode
  (:use :cl
        :prove
        :jonathan.util
        :jonathan))
(in-package :jonathan-test.decode)

(diag "jonathan-test.decode")

(plan 13)

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
           ":as :jsown."))))

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
            "Parse can raise <jonathan-unexpected-eof> with incomplete JSON foramt string."))

(subtest ":junk-allowed t"
  (ok (parse "{\key\":\"value\"" :junk-allowed t)
      "can allow incomplete JSON foramt string."))

(finalize)
