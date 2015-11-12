(parse "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}")
;; => (:NAME "Common Lisp" :BORN 1984 :IMPLS ("SBCL" "CCL" "KCL"))

(parse "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}"
       :as :alist)
;; => (("NAME" . "Common Lisp") ("BORN" . 1984) ("IMPLS" "SBCL" "CCL" "KCL"))

(parse "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}"
       :as :jsown)
;; => (:obj ("NAME" . "Common Lisp") ("BORN" . 1984) ("IMPLS" "SBCL" "CCL" "KCL"))

(parse "{\"NAME\":\"Common Lisp\",\"BORN\":1984,\"IMPLS\":[\"SBCL\",\"KCL\"]}"
       :as :hash-table)
;; => #<HASH-TABLE :TEST EQUAL :COUNT 3>

(parse "{\"key\":\"value\"")
;; => raise <jonathan-unexpected-eof>.

(parse "{\"key\":\"value\"" :junk-allowed t)
;; => (:|key| "value")

(let ((*null-value* :null)
      (*false-value* :false)
      (*empty-array-value* :[]))
  (parse "{\"null\":null,\"false\":false,\"empty\":[]}"))
;; => (:|null| :NULL :|false| :FALSE :|empty| :[])

(parse "{\"key1\":\"value1\",\"key2\":\"value2\"}" :keywords-to-read '("key1"))
;; => (:|key1| "value1")

(flet ((normalizer (key)
         (with-vector-parsing (key)
           (match-i-case
             ("key1" (return-from normalizer "other-key"))
             (otherwise (return-from normalizer nil))))))
  (parse "{\"KEY1\":{\"key2\":\"value2\"},\"key3\":\"value3\"}"
         :keyword-normalizer #'normalizer))
;; => (:|other-key| (:|key2| "value2"))

(flet ((normalizer (key)
         (with-vector-parsing (key)
           (match-i-case
             ("key1" (return-from normalizer "other-key"))
             (otherwise (return-from normalizer nil))))))
  (parse "{\"KEY1\":{\"key2\":\"value2\"},\"key3\":\"value3\"}"
         :keyword-normalizer #'normalizer
         :normalize-all nil)
  ;; => (:|other-key| (:|key2| "value2"))

  (parse "{\"KEY1\":{\"key2\":\"value2\"},\"key3\":\"value3\"}"
         :keyword-normalizer #'normalizer
         :normalize-all t)
  ;; => (:|other-key| nil))

(parse "\"\\u30b8\\u30e7\\u30ca\\u30b5\\u30f3\"")
;; => "ジョナサン"

(parse "\"\\uD840\\uDC0B\"")
;; => "𠀋"

(parse "\"\\u30b8\\u30e7\\u30ca\\u30b5\\u30f3\""
       :unescape-unicode-escape-sequence nil)
;; => "\u30b8\u30e7\u30ca\u30b5\u30f3"
