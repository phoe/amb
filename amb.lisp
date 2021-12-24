;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AMB - an implementation of the ambiguous operator.
;;;; Author: Michał "phoe" Herda, 2021.
;;;; License: MIT.

(uiop:define-package #:amb
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:amb #:constrain #:amb-failure #:amb-failure-stack))

(in-package #:amb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defparameter *started-ambs* '()
  "The internal dynamic variable that controls signalling AMB-FAILURE.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(defun report-amb-failure (condition stream)
  (let ((stack (amb-failure-stack condition)))
    (format stream "AMB for ~:[stack ~S~;the default stack~] failed to match."
            (eq stack 'amb-stack) stack)))

(define-condition amb-failure (simple-warning)
  ((stack :initarg :stack :reader amb-failure-stack))
  (:report report-amb-failure)
  (:documentation
   "The warning signaled whenever the outermost AMB form fails to find a match
for its contents."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

(defmacro constrain (constraint &optional (stack ''amb-stack))
  "A macro for adding a constraint to an ambiguous variable. If the constraint
is satisfied, its value is returned (for convenience inside AMB); otherwise,
backtracking occurs (via CL:THROW)."
  `(or ,constraint (throw ,stack nil)))

(defun generate-binding (binding body stack signalp)
  (destructuring-bind (var value &key shufflep) binding
    (a:with-gensyms (result)
      `(let (,result)
         (let ((*started-ambs* (adjoin ,stack *started-ambs*)))
           (dolist (,var ,(if shufflep `(a:shuffle (copy-seq ,value)) value))
             (catch ,stack
               (setf ,result ,body)
               (when ,result (return)))))
         (cond (,result)
               ,@(when signalp
                   `(((not (member ,stack *started-ambs*))
                      (,signalp 'amb-failure :stack ,stack)))))))))

(defun generate-body (bindings body stack signalp)
  (cond (bindings
         (destructuring-bind (binding . rest) bindings
           (let ((new-body (generate-body rest body stack signalp)))
             (generate-binding binding new-body stack signalp))))
        (body `(locally ,@body))
        (signalp `(unless (member ,stack *started-ambs*)
                    (,signalp 'amb-failure :stack ,stack)))
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
  "A macro implementation of the ambiguous operator.
It establishes ambiguous variables and a dynamic environment in which it is
possible to use AMB:CONSTRAIN in order to constrain the ambiguous variables.

BINDING is a list of bindings, where each binding must consist of a symbol
naming a variable, a list of possible values for that variable, and optionally
a :SHUFFLEP keyword argument if the order of values for that variable is meant
to be randomized every time control enters the AMB form.

The :SIGNALP keyword argument sets the signaling behavior for AMB-FAILURE.
NIL does not signal anything, whereas SIGNAL, WARN, and ERROR use the respective
CL functions for signaling the AMB-FAILURE condition. (Note that :SIGNALP set on
any but the outermost AMB has no effect, as only the outermost AMB signals
the condition.)

If BODY returns true, then that value becomes the return value of AMB.
Otherwise, backtracking occurs until all possibilities are exhausted. In that
case, AMB-FAILURE is signaled (via WARN) and NIL is returned."
  (parse-amb bindings-and-options body))
