(in-package :cl-user)
(defpackage jonathan.decode
  (:use :cl
        :jonathan.error
        :jonathan.util
        :proc-parse)
  (:export :*false-value*
           :*null-value*
           :*empty-array-value*
           :parse))
(in-package :jonathan.decode)

(defvar *false-value* nil)
(defvar *null-value* nil)
(defvar *empty-array-value* nil)

(defmacro make-normalizer (keywords)
  (let ((normalizer-block (gensym)))
    `(lambda (key)
       (block ,normalizer-block
         (with-vector-parsing (key)
           (match-case
            ,@(mapcar #'(lambda (key)
                          `(,key (return-from ,normalizer-block ,key)))
                      keywords)
            (otherwise (return-from ,normalizer-block))))))))

(defun parse (string &key (as :plist) junk-allowed keywords-to-read keyword-normalizer dont-compile)
  (declare (ignore dont-compile)
           (type simple-string string)
           (type (or null function) keyword-normalizer)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((as-alist (eq as :alist))
        (as-jsown (eq as :jsown))
        (as-hash-table (eq as :hash-table)))
    (with-vector-parsing (string)
      (macrolet ((with-allowed-last-character ((&optional char) &body body)
                   (let ((junk-allowed-block (gensym "junk-allowed-block")))
                     `(block ,junk-allowed-block
                        (tagbody
                           (return-from ,junk-allowed-block
                             (progn ,@body))
                         :eof
                           (return-from ,junk-allowed-block
                             (or junk-allowed
                                 (if (eq ,char (current))
                                     t
                                     (error '<jonathan-unexpected-eof-error>
                                            :object string))))))))
                 (skip-spaces ()
                   `(skip* #\Space #\Newline #\Tab))
                 (with-skip-spaces (&body body)
                   `(progn
                      (skip-spaces)
                      ,@body
                      (skip-spaces)))
                 (skip?-or-eof (char)
                   `(wIth-allowed-last-character (,char)
                      (or (skip? ,char)
                          (when (eofp) (go :eof)))))
                 (match-and-return (string block value)
                   `(match-case
                     (,string (return-from ,block ,value))
                     (otherwise (or (and junk-allowed (return-from ,block ,value))
                                    (error '<jonathan-incomplete-json-error> :object string))))))
        (labels ((dispatch (&optional skip-p force-read-p)
                   (skip-spaces)
                   (match-case
                    ("{" (return-from dispatch (read-object skip-p force-read-p)))
                    ("\"" (return-from dispatch (read-string skip-p)))
                    ("[" (return-from dispatch (read-array skip-p)))
                    ("t" (match-and-return "rue" dispatch t))
                    ("f" (match-and-return "alse" dispatch *false-value*))
                    ("n" (match-and-return "ull" dispatch *null-value*))
                    (otherwise (or (and (eofp) (and (or junk-allowed (error '<jonathan-unexpected-eof-error> :object string))
                                                    (return-from dispatch nil)))
                                   (or (and (integer-char-p (current)) (return-from dispatch (read-number skip-p)))
                                       (error '<jonathan-incomplete-json-error> :object string))))))
                 (read-object (&optional skip-p force-read-p)
                   (skip-spaces)
                   (loop until (skip?-or-eof #\})
                         with result = (when as-hash-table (make-hash-table :test #'equal))
                         as key = (progn (advance*)
                                         (let ((string (read-string skip-p)))
                                           (cond
                                             ((or (not (or keyword-normalizer keywords-to-read)) force-read-p skip-p) string)
                                             (keyword-normalizer (funcall keyword-normalizer string))
                                             (t (when (member string keywords-to-read :test #'string=) string)))))
                         as value = (progn (with-skip-spaces (advance*))
                                           (dispatch (not key) key))
                         do (with-skip-spaces (skip? #\,))
                         when key
                           do (cond
                                ((or as-alist as-jsown) (push (cons key value) result))
                                (as-hash-table (setf (gethash key result) value))
                                (t (setq result (nconc (list (make-keyword key) value) result))))
                         finally (return-from read-object
                                   (if as-jsown
                                       (cons :obj result)
                                       result))))
                 (read-string (&optional skip-p)
                   (if (eofp)
                       ""
                       (let ((start (pos))
                             (escaped-count 0))
                         (declare (type fixnum start escaped-count))
                         (with-allowed-last-character (#\")
                           (skip-while (lambda (c)
                                         (or (and (char= c #\\) (incf escaped-count) (advance*))
                                             (char/= c #\")))))
                         (prog1
                             (unless skip-p
                               (if (= escaped-count 0)
                                   (subseq string start (pos))
                                   (parse-string-with-escaping start escaped-count)))
                           (skip?-or-eof #\")))))
                 (parse-string-with-escaping (start escaped-count)
                   (declare (type fixnum start escaped-count))
                   (loop with result = (make-array (- (pos) start escaped-count)
                                                   :element-type 'character
                                                   :adjustable nil)
                         with result-index = 0
                         with escaped-p
                         for index from start below (pos)
                         for char = (char string index)
                         if escaped-p
                           do (setf escaped-p nil)
                              (setf (char result result-index)
                                    (case char
                                      (#\b #\Backspace)
                                      (#\f #\Linefeed)
                                      (#\n #\Linefeed)
                                      (#\r #\Return)
                                      (#\t #\Tab)
                                      (t char)))
                              (incf result-index)
                              (when (zerop (decf escaped-count))
                                (return-from parse-string-with-escaping
                                  (replace result (the (simple-array character (*)) (subseq string (1+ index)))
                                           :start1 result-index)))
                         else
                           if (char= char #\\)
                             do (setf escaped-p t)
                         else
                           do (setf (char result result-index) char)
                              (incf result-index)
                         finally (return result)))
                 (read-array (&optional skip-p)
                   (skip-spaces)
                   (or (loop until (skip?-or-eof #\])
                             collect (dispatch skip-p)
                             do (with-skip-spaces (skip? #\,)))
                       *empty-array-value*))
                 (read-number (&optional skip-p)
                   (if skip-p
                       (tagbody
                          (skip-while integer-char-p)
                          (when (skip? #\.)
                            (skip-while integer-char-p))
                        :eof
                          (return-from read-number))
                       (bind (num-str (skip-while integer-char-p))
                         (let ((num (the fixnum (parse-integer num-str))))
                           (return-from read-number
                             (if (with-allowed-last-character ()
                                   (skip? #\.))
                                 (+ num
                                    (block nil
                                      (let ((rest-start (the fixnum (pos))))
                                        (bind (rest-num-str (skip-while integer-char-p))
                                          (let ((rest-num (the fixnum (parse-integer rest-num-str))))
                                            (return (the rational (/ rest-num (the fixnum (expt 10 (- (pos) rest-start)))))))))))
                                 (the fixnum num))))))))
          (declare (inline read-object read-string parse-string-with-escaping read-array read-number))
          (skip-spaces)
          (return-from parse (dispatch)))))))


(define-compiler-macro parse (&whole form string &key (as :plist) junk-allowed keywords-to-read keyword-normalizer dont-compile)
  (handler-case
      (if (and keywords-to-read (not keyword-normalizer) (not dont-compile))
          (let ((keywords (eval keywords-to-read)))
            (unless (every #'stringp keywords)
              (error 'simple-error))
            `(parse ,string :as ,as
                            :junk-allowed ,junk-allowed
                            :keywords-to-read ,keywords-to-read
                            :keyword-normalizer (make-normalizer ,keywords)))
          form)
    (error () form)))
