;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AMB - an implementation of the ambiguous operator.
;;;; Author: Michał "phoe" Herda, 2021.
;;;; License: MIT.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests from SICP 4.3 (Nondeterministic Computing)

(in-package #:amb/test)

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

;; TODO more tests from SICP exercises, see
;; https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-28.html#%_sec_4.3.2
