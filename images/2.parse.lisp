(let ((s "{\"key1\":\"value\",\"key2\":1.1,\"key3\":[\"Hello\",1.2]}"))
  (time
   (dotimes (_ 100000)
     (jonathan:parse s :as :alist))))
;; => 0.174

(let ((s "{\"key1\":\"value\",\"key2\":1.1,\"key3\":[\"Hello\",1.2]}"))
  (time
   (dotimes (_ 100000)
     (jonathan:parse s :as :jsown))))
;; => 0.181

(let ((s "{\"key1\":\"value\",\"key2\":1.1,\"key3\":[\"Hello\",1.2]}"))
  (time
   (dotimes (_ 100000)
     (jsown:parse s))))
;; => 0.204
