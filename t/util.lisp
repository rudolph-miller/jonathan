(in-package :cl-user)
(defpackage jonathan-test.util
  (:use :cl
        :prove
        :jonathan.util))
(in-package :jonathan-test.util)

(plan 3)

(subtest "my-plist-p"
  (ok (my-plist-p '(:key1 :value1)))
  (ok (my-plist-p '(:key1 :value1 :key2)))
  (ok (my-plist-p '(:key1 :value1 :key2 :value2)))
  (ok (my-plist-p '(:key1 (:key2 :value3))))
  (ok (not (my-plist-p '("item1" "item2")))))

(subtest "integer-char-p"
  (ok (integer-char-p #\1))
  (ok (integer-char-p #\-))
  (ok (not (integer-char-p #\a))))

(subtest "make-keyword"
  (is (make-keyword "RUDOLPH")
      :rudolph))

(finalize)
