(let ((s "{\"key1\":\"value\",\"key2\":1.1,\"key3\":[\"Hello\",1.2]}"))
  (time
   (dotimes (_ 100000)
     (jonathan:parse s :as :alist :keywords-to-read '("key1")))))
;; => 0.065

(let ((s "{\"key1\":\"value\",\"key2\":1.1,\"key3\":[\"Hello\",1.2]}"))
  (time
   (dotimes (_ 100000)
     (jonathan:parse s :as :jsown :keywords-to-read '("key1")))))
;; => 0.069

(let ((s "{\"key1\":\"value\",\"key2\":1.1,\"key3\":[\"Hello\",1.2]}"))
  (time
   (dotimes (_ 100000)
     (jsown:parse s "key1"))))
;; => 0.085
