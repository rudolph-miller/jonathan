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

(defun parse (string &key (as :plist) junk-allowed)
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
                      (when (eofp)
                        (go :eof))))))
        (labels ((dispatch ()
                   (skip-spaces)
                   (match-case
                    ("{" (return-from dispatch (read-object)))
                    ("\"" (return-from dispatch (read-string)))
                    ("[" (return-from dispatch (read-array)))
                    ("true" (return-from dispatch t))
                    ("false" (return-from dispatch *false-value*))
                    ("null" (return-from dispatch *null-value*))
                    (otherwise (return-from dispatch
                                 (if (eofp)
                                     nil
                                     (read-number))))))
                 (read-object ()
                   (skip-spaces)
                   (loop until (and (skip?-or-eof #\})
                                    (or (when (eq as :hash-table)
                                          (return-from read-object hash-table))
                                        t))
                         with first = t
                         with hash-table = nil
                         for key = (progn (advance*) (read-string))
                         for value = (progn (skip-spaces) (advance*) (skip-spaces) (dispatch))
                         when (and first as-hash-table)
                           do (setq first nil)
                              (setq hash-table (make-hash-table :test #'equal))
                         do (skip?-with-spaces #\,)
                         when (and first as-jsown)
                           collecting (progn (setq first nil) :obj)
                         if (or as-alist as-jsown)
                           collecting (cons key value)
                         else
                           if as-hash-table
                             do (setf (gethash key hash-table) value)
                         else
                           nconc (list (make-keyword key) value)))
                 (read-string ()
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
                             (if (= escaped-count 0)
                                 (subseq string start (pos))
                                 (parse-string-with-escaping start escaped-count))
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
                 (read-array ()
                   (skip-spaces)
                   (when (skip?-or-eof #\])
                     (return-from read-array *empty-array-value*))
                   (loop collect (prog1 (dispatch)
                                   (skip?-with-spaces #\,))
                         until (skip?-or-eof #\])))
                 (read-number ()
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
                             (the fixnum num)))))))
          (declare (inline read-object read-string parse-string-with-escaping read-array read-number))
          (skip-spaces)
          (return-from parse (dispatch)))))))
