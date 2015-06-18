(let ((post (compile-encoder () (text)
               (list :|channel| "lisp-alien"
                     :|username| "alien-bot"
                     :|text| text
                     :|icon_url| "http://www.lisperati.com/lisplogo_warning2_256.png"))))
  (time
   (dotimes (_ 100000)
     (funcall post "Post from Alien!"))))
;; => 0.095

(flet ((post (text)
         (jonathan:to-json
               (list :|channel| "lisp-alien"
                     :|username| "alien-bot"
                     :|text| text
                     :|icon_url| "http://www.lisperati.com/lisplogo_warning2_256.png"))))
  (time
   (dotimes (_ 100000)
     (post "Post from Alien!"))))
;; => 0.095

(flet ((post (text)
         (format nil "{\"channel\":\"lisp-alien\",\"username\":\"alien-bot\",\"text\":~s,\"icon_url\":\"http://www.lisperati.com/lisplogo_warning2_256.png\"}" text)))
  (time
   (dotimes (_ 100000)
     (post "Post from Alien!"))))"\"}"))))
;; => 0.146

(flet ((post (text)
         (jonathan:to-json
               (list :|channel| "lisp-alien"
                     :|username| "alien-bot"
                     :|text| text
                     :|icon_url| "http://www.lisperati.com/lisplogo_warning2_256.png"))))
  (time
   (dotimes (_ 100000)
     (post "Post from Alien!"))))
;; => 0.604 - without compiler-macro.

(flet ((post (text)
         (jsown:to-json
          `(:obj (:|channel| . "lisp-alien")
                 (:|username| . "alien-bot")
                 (:|text| . ,text)
                 (:|icon_url| . "http://www.lisperati.com/lisplogo_warning2_256.png")))))
  (time
   (dotimes (_ 100000)
     (post "Post from Alien!"))))
;; => 1.117
