;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AMB - an implementation of the ambiguous operator.
;;;; Author: Michał "phoe" Herda, 2021.
;;;; License: MIT.

(defpackage #:amb/test
  (:use #:cl #:parachute)
  (:local-nicknames (#:a #:alexandria))
  (:export #:amb))

(in-package #:amb/test)

(defun test-amb () (parachute:test 'amb :report 'parachute:interactive))

(define-test amb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic tests

(define-test amb.empty-sequence :parent amb
  (fail (amb:amb ()) amb:amb-failure)
  (fail (amb:amb ((:stack stack))) amb:amb-failure)
  (handler-case (progn (amb:amb ((:signalp nil)))
                       (amb:amb ((:signalp nil) (:stack stack))))
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
        (amb:amb ((x1 '(1 2 3)) (:stack stack-x))
          (amb:amb ((y1 '(1 2 3)) (:stack stack-y))
            (amb:amb ((y2 '(2 4 6)) (:stack stack-y))
              (amb:constrain (= y1 y2) stack-y)
              (setf result-y (list y1 y2))))
          (amb:amb ((x2 '(-2 -4 -6)) (:stack stack-x))
            (amb:constrain (= x1 (- x2)) stack-x)
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
        (amb:amb ((x1 '(1 2 3)) (:stack stack-x) (:signalp signal))
          (amb:amb ((y1 '(10 20 30)) (:stack stack-y) (:signalp signal))
            (amb:amb ((x2 '(4 5 6)) (:stack stack-x))
              (amb:amb ((y2 '(40 50 60)) (:stack stack-y)))))))
      (let ((expected '(stack-x stack-y stack-y stack-y)))
        (is equal expected stacks)))))

(define-test amb.no-fallthrough-on-failed-constraint :parent amb
  (let ((fallthroughp nil))
    (amb:amb ((:signalp nil)
              (x '(1 2 3 4 5)))
      (amb:constrain (= x 0))
      (setf fallthroughp t))
    (false fallthroughp)))

