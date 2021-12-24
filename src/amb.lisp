;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AMB - an implementation of the ambiguous operator.
;;;; Author: Michał "phoe" Herda, 2021.
;;;; License: MIT.

(uiop:define-package #:amb
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:amb #:constrain #:amb-failure #:amb-failure-stack))

(in-package #:amb)

(defparameter *started-ambs* '())

(defun report-amb-failure (condition stream)
  (let ((stack (amb-failure-stack condition)))
    (format stream "AMB for ~:[stack ~S~;the default stack~] failed to match."
            (eq stack 'amb-stack) stack)))

(define-condition amb-failure (simple-warning)
  ((stack :initarg :stack :reader amb-failure-stack))
  (:report report-amb-failure)
  (:documentation))

(defmacro constrain (constraint &optional (stack ''amb-stack))
  `(or ,constraint (throw ',stack nil)))

(defun generate-binding (binding body stack signalp)
  (destructuring-bind (var value &key shufflep) binding
    (a:with-gensyms (result)
      `(let (,result)
         (let ((*started-ambs* (adjoin ',stack *started-ambs*)))
           (dolist (,var ,(if shufflep `(a:shuffle (copy-seq ,value)) value))
             (catch ',stack
               (setf ,result ,body)
               (when ,result (return)))))
         (cond (,result)
               ((not (member ',stack *started-ambs*))
                ,(when signalp
                   `(,signalp 'amb-failure :stack ',stack)))
               (t (throw ',stack nil)))))))

(defun generate-body (bindings body stack signalp)
  (cond (bindings
         (destructuring-bind (binding . rest) bindings
           (let ((new-body (generate-body rest body stack signalp)))
             (generate-binding binding new-body stack signalp))))
        (body `(locally ,@body))
        (signalp `(if (member ',stack *started-ambs*)
                      (throw ',stack nil)
                      (,signalp 'amb-failure :stack ',stack)))
        (t `(progn))))

(defun parse-amb (bindings-and-options body)
  (flet ((optionp (x) (member x '(:stack :signalp))))
    (let* ((bindings (remove-if #'optionp bindings-and-options :key #'first))
           (signalp-option (assoc :signalp bindings-and-options))
           (signalp (if signalp-option (second signalp-option) 'warn))
           (stack-option (assoc :stack bindings-and-options))
           (stack (if stack-option (second stack-option) ''amb-stack)))
      (check-type signalp (member nil signal warn error))
      (generate-body bindings body stack signalp))))

(defmacro amb (bindings-and-options &body body)
  (parse-amb bindings-and-options body))
