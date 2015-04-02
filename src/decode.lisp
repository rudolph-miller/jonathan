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
  (with-vector-parsing (string)
    (macrolet ((skip-spaces ()
                 `(skip* #\Space))
               (skip?-with-spaces (char)
                 `(progn
                    (skip-spaces)
                    (skip? ,char)
                    (skip-spaces)))
               (skip?-or-eof (char)
                 `(or (skip? ,char) (and (eofp)
                                         (or junk-allowed
                                             (error '<jonathan-unexpected-eof-error>
                                                    :object string))))))
      (labels ((dispatch ()
                 (skip-spaces)
                 (match-case
                  ("{" (read-object))
                  ("\"" (read-string))
                  ("[" (read-array))
                  ("true" t)
                  ("false" *false-value*)
                  ("null" *null-value*)
                  (otherwise (if (eofp)
                                 (return-from dispatch)
                                 (read-number)))))
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
                       when (and first (eq as :hash-table))
                         do (setq first nil)
                            (setq hash-table (make-hash-table :test #'equal))
                       do (skip?-with-spaces #\,)
                       when (and first (eq as :jsown))
                         collecting (progn (setq first nil) :obj)
                       if (or (eq as :alist) (eq as :jsown))
                         collecting (cons key value)
                       else
                         if (eq as :hash-table)
                           do (setf (gethash key hash-table) value)
                       else
                           nconc (list (make-keyword key) value)))
               (read-string ()
                 (if (eofp)
                     ""
                     (let ((start (pos))
                           (escaped-count 0))
                       (declare (type fixnum start escaped-count))
                       (skip-while (lambda (c)
                                     (or (and (char= c #\\) (incf escaped-count) (advance*))
                                         (char/= c #\"))))
                       (prog1
                           (if (= escaped-count 0)
                               (subseq string start (pos))
                               (parse-string-with-escaping start escaped-count))
                         (skip?-or-eof #\")))))
               (parse-string-with-escaping (start escaped-count)
                 (declare (optimize (speed 3) (debug 0) (safety 0)))
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
               (read-number (&optional rest-p)
                 (let ((start (the fixnum (pos))))
                   (bind (num-str (skip-while integer-char-p))
                     (let ((num (the fixnum (or (parse-integer num-str :junk-allowed t) 0))))
                       (return-from read-number
                         (cond
                           (rest-p
                            (the rational (/ num (the fixnum (expt 10 (- (pos) start))))))
                           ((skip? #\.)
                            (the rational (+ num (the rational (read-number t)))))
                           (t (the fixnum num)))))))))
        (declare (inline parse-string-with-escaping))
        (skip-spaces)
        (return-from parse (dispatch))))))
