(compile-encoder () (name)
  (list :name name))
;; => #<FUNCTION (LAMBDA (name))>

(funcall * "Rudolph")
;; => "{\"NAME\":\"Rudolph\"}"

(compile-encoder (:from :alist) (name)
  `(("name" . ,name)))
;; => #<FUNCTION (LAMBDA (name))>

(funcall * "Rudolph")
;; => "{\"name\":\"Rudolph\"}"

(compile-encoder (:octets t) (name)
  (list :name name))
;; => #<FUNCTION (LAMBDA (name))>

(funcall * "Rudolph")
;; => #(123 34 75 69 89 49 ...)
