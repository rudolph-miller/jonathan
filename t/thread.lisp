(in-package :cl-user)
(defpackage jonathan-test.thread
  (:use :cl
        :prove
        :jonathan
        :legion))
(in-package :jonathan-test.thread)

(diag "jonathan-test.thread")

(plan 1)

(defparameter *failed-count* 0)

(defparameter *cluster*
  (legion:make-cluster 30
                       (lambda (val)
                         (handler-case
                             (assert (string= "{\"retry_count\":0,\"failed_at\":1481507440,\"queue\":\"default\",\"error_class\":\"COMMON-LISP::SIMPLE-ERROR\",\"error_message\":\"Some shit happened\",\"class\":\"WORKER::MY-WORKER\",\"args\":[140],\"jid\":\"xeeuh54pm4id\",\"created_at\":1481507440,\"enqueued_at\":1481507704}"
                                              (jojo:to-json val :from :alist)))
                           (error (e)
                             (declare (ignore e))
                             (incf *failed-count*))))))

(legion:start *cluster*)

(dotimes (i 300)
  (legion:add-job *cluster* '(("retry_count" . 0)
                              ("failed_at" . 1481507440)
                              ("queue" . "default")
                              ("error_class" . "COMMON-LISP::SIMPLE-ERROR")
                              ("error_message" . "Some shit happened")
                              ("class" . "WORKER::MY-WORKER")
                              ("args" 140)
                              ("jid" . "xeeuh54pm4id")
                              ("created_at" . 1481507440)
                              ("enqueued_at" . 1481507704))))

(is *failed-count* 0)

(finalize)
