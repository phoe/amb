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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:amb/test
  (:use #:cl #:parachute)
  (:local-nicknames (#:a #:alexandria))
  (:export #:amb))

(in-package #:amb/test)

(define-test amb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic tests

(define-test amb.empty-sequence :parent amb
  (fail (amb:amb ()) amb:amb-failure)
  (fail (amb:amb ((:stack 'stack))) amb:amb-failure)
  (handler-case (progn (amb:amb ((:signalp nil)))
                       (amb:amb ((:signalp nil) (:stack 'stack))))
    (amb:amb-failure (c) (error "Test failure: ~S" c))))

(define-test amb.only-one-failure :parent amb
  (let ((failures '()))
    (flet ((collect (c) (push c failures)))
      (handler-bind ((amb:amb-failure #'collect))
        (amb:amb ((x '(1 2 3 4 5)) (:signalp signal))
          (amb:amb ()
            (amb:amb ((y '(6 7 8))))))
        (is = 1 (length failures))))))

(define-test amb.one-var :parent amb:amb
  (let ((result (amb:amb ((x '(1 2 3 4 5)))
                  (amb:constrain (= x 2))
                  (is = 2 x)
                  x)))
    (is equal 2 result)))

(define-test amb.two-vars :parent amb
  (let ((result (amb:amb ((x '(1 2 3 4 5)))
                  (amb:constrain (= x 2))
                  (amb:amb ((y '(1 2 3 4 5)))
                    (amb:constrain (= y x))
                    (is equal '(2 2) (list x y))
                    (list x y)))))
    (is equal '(2 2) result))
  (let ((result (amb:amb ((x '(1 2 3 4 5)))
                  (amb:amb ((y '(1 2 3 4 5)))
                    (amb:constrain (= x 2))
                    (amb:constrain (= y x))
                    (is equal '(2 2) (list x y))
                    (list x y)))))
    (is equal '(2 2) result)))

(define-test amb.shuffle :parent amb
  (let ((*random-state* (make-random-state t)))
    (amb:amb ((x '(1 2 3 4 5) :shufflep t))
      (amb:constrain (= x 2))
      (amb:amb ((y '(1 2 3 4 5) :shufflep t))
        (amb:constrain (= y x))
        (is equal '(2 2) (list x y))))
    (amb:amb ((x '(1 2 3 4 5) :shufflep t))
      (amb:amb ((y '(1 2 3 4 5) :shufflep t))
        (amb:constrain (= x 2))
        (amb:constrain (= y x))
        (is equal '(2 2) (list x y))))))

(define-test amb.dynamic-scope :parent amb
  (flet ((nested (x)
           (amb:amb ((y '(2 4 6)))
             (amb:constrain (= x y))
             (is equal '(2 2) (list x y)))))
    (amb:amb ((x '(1 2 3))) (nested x))))

(define-test amb.shuffle.stress :parent amb.shuffle
  (let ((*random-state* (make-random-state t)))
    (labels ((shuffle-random-test ()
               (loop repeat 10000
                     with list1 = '(q w e r t y)
                     with list2 = '(q a z x s d)
                     with list3 = '(q :q "q" #:q |q|)
                     with list4 = '(q qq qqq qqqq qqqqq)
                     with expected = 'q
                     with actual = (amb:amb ((var1 list1 :shufflep t)
                                             (var2 list2 :shufflep t))
                                     (amb:constrain (eq var1 var2))
                                     (amb:amb ((var3 list3 :shufflep t))
                                       (amb:constrain (eq var1 var3))
                                       (amb:amb ((var4 list4 :shufflep t))
                                         (amb:constrain (eq var1 var4))
                                         var1)))
                     always (equal expected actual))))
      (true (shuffle-random-test)))))

(define-test amb.stack :parent amb
  (let (result-x result-y)
    (handler-case
        (amb:amb ((x1 '(1 2 3)) (:stack 'stack-x))
          (amb:amb ((y1 '(1 2 3)) (:stack 'stack-y))
            (amb:amb ((y2 '(2 4 6)) (:stack 'stack-y))
              (amb:constrain (= y1 y2) 'stack-y)
              (setf result-y (list y1 y2))))
          (amb:amb ((x2 '(-2 -4 -6)) (:stack 'stack-x))
            (amb:constrain (= x1 (- x2)) 'stack-x)
            (setf result-x (list x1 x2))))
      (amb:amb-failure (c) (error "test failure: ~A" c)))
    (is equal '(2 -2) result-x)
    (is equal '(2 2) result-y)))

(define-test amb.stack-fail :parent amb
  (let ((stacks '()))
    (flet ((collect (c) (push (amb:amb-failure-stack c) stacks)))
      (handler-bind ((amb:amb-failure #'collect))
        ;; The behavior here is peculiar: for each element of the first sequence
        ;; in AMB for STACK-X, the AMB for STACK-Y fails to match, which pushes
        ;; STACK-Y into the result three times. Only then the AMB for STACK-X
        ;; fails to match and STACK-X is pushed into the result.
        (amb:amb ((x1 '(1 2 3)) (:stack 'stack-x) (:signalp signal))
          (amb:amb ((y1 '(10 20 30)) (:stack 'stack-y) (:signalp signal))
            (amb:amb ((x2 '(4 5 6)) (:stack 'stack-x))
              (amb:amb ((y2 '(40 50 60)) (:stack 'stack-y)))))))
      (let ((expected '(stack-x stack-y stack-y stack-y)))
        (is equal expected stacks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests from SICP 4.3 (Nondeterministic Computing)

(define-test amb.sicp :parent amb)

(define-test amb.pythagorean-triples :parent amb.sicp
  ;; SICP Exercise 4.35
  (let ((integers (a:iota 100 :start 1))
        (expected '((3 4 5)    (5 12 13)  (8 15 17)  (7 24 25)
                    (20 21 29) (12 35 37) (9 40 41)  (28 45 53)
                    (11 60 61) (16 63 65) (33 56 65) (48 55 73)
                    (13 84 85) (36 77 85) (39 80 89) (65 72 97)))
        (actual '()))
    (amb:amb ((x integers)
              (y integers)
              (z integers)
              (:signalp nil))
      (amb:constrain (< x y z))
      (amb:constrain (= 1 (gcd x y z)))
      (amb:constrain (= (+ (* x x) (* y y)) (* z z)))
      (push (list x y z) actual)
      nil)
    (true (a:set-equal expected actual :test #'equal))))

(define-test amb.multiple-dwellings.simple :parent amb.sicp
  ;; SICP Section 4.3.2
  (let ((flats '(1 2 3 4 5)))
    (amb:amb ((baker flats)
              (cooper flats)
              (fletcher flats)
              (miller flats)
              (smith flats))
      (amb:constrain (/= baker cooper fletcher miller smith))
      (amb:constrain (/= baker 5))
      (amb:constrain (/= cooper 1))
      (amb:constrain (/= fletcher 1 5))
      (amb:constrain (> miller cooper))
      (amb:constrain (/= (abs (- fletcher cooper)) 1))
      (amb:constrain (/= (abs (- smith fletcher)) 1))
      (let ((result (list baker cooper fletcher miller smith)))
        (is equal '(3 2 4 5 1) result)))))

(define-test amb.multiple-dwellings.multiple :parent amb.sicp
  ;; SICP Exercise 4.38
  (let ((flats '(1 2 3 4 5))
        (solutions '((1 2 4 3 5) (1 2 4 5 3) (1 4 2 5 3)
                     (3 2 4 5 1) (3 4 2 5 1)))
        (count 0))
    (amb:amb ((baker flats)
              (cooper flats)
              (fletcher flats)
              (miller flats)
              (smith flats)
              (:signalp nil))
      (amb:constrain (/= baker cooper fletcher miller smith))
      (amb:constrain (/= baker 5))
      (amb:constrain (/= cooper 1))
      (amb:constrain (/= fletcher 1 5))
      (amb:constrain (> miller cooper))
      (amb:constrain (/= (abs (- fletcher cooper)) 1))
      ;; Commented out on purpose - see SICP 4.38
      ;; (amb:constrain (/= (abs (- smith fletcher)) 1))
      (let ((result (list baker cooper fletcher miller smith)))
        (true (member result solutions :test #'equal)))
      (incf count)
      nil)
    (is = 5 count)))

(define-test amb.multiple-dwellings.optimized :parent amb.sicp
  ;; SICP Exercise 4.40
  (let ((flats '(1 2 3 4 5)))
    (amb:amb ((baker flats))
      (amb:constrain (/= baker 5))
      (amb:amb ((cooper flats))
        (amb:constrain (/= cooper 1))
        (amb:amb ((fletcher flats))
          (amb:constrain (/= fletcher 1 5))
          (amb:constrain (/= (abs (- fletcher cooper)) 1))
          (amb:amb ((miller flats))
            (amb:constrain (> miller cooper))
            (amb:amb ((smith flats))
              (amb:constrain (/= baker cooper fletcher miller smith))
              (amb:constrain (/= (abs (- smith fletcher)) 1))
              (let ((result (list baker cooper fletcher miller smith)))
                (is equal '(3 2 4 5 1) result)))))))))

(define-test amb.8-queens :parent amb.sicp
  ;; SICP Exercise 4.44
  (flet ((check-conflict (queen-1 &rest queens)
           (let ((x1 (first queen-1))
                 (y1 (second queen-1)))
             (dolist (queen-2 queens)
               (let ((x2 (first queen-2))
                     (y2 (second queen-2)))
                 (amb:constrain (not (= x1 x2)))
                 (amb:constrain (not (= y1 y2)))
                 (amb:constrain (not (= (- x1 y1) (- x2 y2))))
                 (amb:constrain (not (= (+ x1 y1) (+ x2 y2)))))))))
    (let ((result '()))
      (macrolet ((%generate (remaining &optional done)
                   (if remaining
                       (destructuring-bind (queen . rest) remaining
                         (flet ((make-coords (column)
                                  (a:map-product #'list
                                                 (list column)
                                                 (a:iota 8 :start 1))))
                           (let* ((column (1+ (length done)))
                                  (coords (make-coords column)))
                             `(amb:amb ((,queen ',coords)
                                        (:signalp nil))
                                (check-conflict ,queen ,@done)
                                (%generate ,rest (,queen ,@done))))))
                       `(progn
                          (push (list ,@(reverse done)) result)
                          nil)))
                 (generate (&rest remaining)
                   `(%generate ,remaining)))
        (generate queen-1 queen-2 queen-3 queen-4
                  queen-5 queen-6 queen-7 queen-8)
        (is = 92 (length result))))))

;; ;; TODO more tests from SICP exercises, see
;; ;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.2
