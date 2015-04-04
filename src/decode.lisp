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

(defmacro make-matcher-macro (keywords)
  (let ((matcher-block (gensym)))
    `(lambda (key)
       (block ,matcher-block
         (with-vector-parsing (key)
           (match-case
            ,@(mapcar #'(lambda (key)
                          `(,key (return-from ,matcher-block t)))
                      keywords)
            (otherwise (return-from ,matcher-block))))))))

(defun parse (string &key (as :plist) junk-allowed keywords-to-read matcher-for-keywords)
  (declare (type simple-string string))
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
                 (skip?-with-spaces (char)
                   `(progn
                      (skip-spaces)
                      (skip? ,char)
                      (skip-spaces)))
                 (skip?-or-eof (char)
                   `(with-allowed-last-character (,char)
                      (or (skip? ,char)
                          (when (eofp) (go :eof))))))
        (labels ((dispatch (&optional skip-p force-read-p)
                   (skip-spaces)
                   (match-case
                    ("{" (return-from dispatch (read-object skip-p force-read-p)))
                    ("\"" (return-from dispatch (read-string skip-p)))
                    ("[" (return-from dispatch (read-array skip-p)))
                    ("true" (return-from dispatch t))
                    ("false" (return-from dispatch *false-value*))
                    ("null" (return-from dispatch *null-value*))
                    (otherwise (return-from dispatch
                                 (if (eofp)
                                     nil
                                     (read-number skip-p))))))
                 (read-object (&optional skip-p force-read-p)
                   (skip-spaces)
                   (loop until (and (skip?-or-eof #\})
                                    (or (when (eq as :hash-table)
                                          (return-from read-object hash-table))
                                        t))
                         with first = t
                         with hash-table = nil
                         for key = (progn (advance*)
                                          (let ((string (read-string skip-p)))
                                            (cond
                                              (matcher-for-keywords (when (funcall matcher-for-keywords string) string))
                                              (force-read-p string)
                                              (t (when (member string keywords-to-read :test #'string=) string)))))
                         for value = (progn (skip-spaces)
                                            (advance*)
                                            (skip-spaces)
                                            (dispatch (not key) key))
                         when (and first as-hash-table)
                           do (setq first nil)
                              (setq hash-table (make-hash-table :test #'equal))
                         do (skip?-with-spaces #\,)
                         when (and first as-jsown)
                           collecting (progn (setq first nil) :obj)
                         if (not key)
                           nconc nil
                         else
                           if (or as-alist as-jsown)
                             collecting (cons key value)
                         else
                           if as-hash-table
                             do (setf (gethash key hash-table) value)
                         else
                           nconc (list (make-keyword key) value)))
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
                             (if skip-p
                                 nil
                                 (if (= escaped-count 0)
                                     (subseq string start (pos))
                                     (parse-string-with-escaping start escaped-count)))
                           (skip?-or-eof #\")))))
                 (parse-string-with-escaping (start escaped-count)
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
                         else
                           if (char= char #\\)
                             do (setf escaped-p t)
                         else
                           do (setf (char result result-index) char)
                              (incf result-index)
                         finally (return result)))
                 (read-array (&optional skip-p)
                   (skip-spaces)
                   (when (skip?-or-eof #\])
                     (return-from read-array *empty-array-value*))
                   (loop collect (prog1 (dispatch skip-p)
                                   (skip?-with-spaces #\,))
                         until (skip?-or-eof #\])))
                 (read-number (&optional skip-p)
                   (if skip-p
                       (tagbody
                          (skip-while integer-char-p)
                          (when (skip? #\.)
                            (skip-while integer-char-p))
                          (return-from read-number)
                        :eof
                          (return-from read-number))
                       (bind (num-str (skip-while integer-char-p))
                         (let ((num (the fixnum (or (parse-integer num-str :junk-allowed t) 0))))
                           (return-from read-number
                             (if (with-allowed-last-character ()
                                   (skip? #\.))
                                 (+ num
                                    (block nil
                                      (let ((rest-start (the fixnum (pos))))
                                        (bind (rest-num-str (skip-while integer-char-p))
                                          (let ((rest-num (the fixnum (or (parse-integer rest-num-str :junk-allowed t) 0))))
                                            (return (the rational (/ rest-num (the fixnum (expt 10 (- (pos) rest-start)))))))))))
                                 (the fixnum num))))))))
          (declare (inline read-object read-string parse-string-with-escaping read-array read-number))
          (skip-spaces)
          (return-from parse (dispatch)))))))


(define-compiler-macro parse (&whole form string &key (as :plist) junk-allowed keywords-to-read matcher-for-keywords)
  (handler-case
      (if (and keywords-to-read (not matcher-for-keywords))
          `(parse ,string :as ,as
                          :junk-allowed ,junk-allowed
                          :keywords-to-read ,keywords-to-read
                          :matcher-for-keywords (make-matcher-macro ,(eval keywords-to-read)))
          form)
    (error () form)))
