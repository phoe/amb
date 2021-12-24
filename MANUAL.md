# AMB Manual

The basic use case for `amb` is to find a set of values for which the
constraints found inside the `amb`'s body can be satisfied. Here, the returned
values of `x` and `y` are found by `amb` based on the constraints placed on it
(`x` greater than 3, `y` must be equal to `x` times 20).

```lisp
AMB> (amb ((x '(1 2 3 4 5))
           (y '(20 40 60 80 100)))
       (constrain (> x 3))
       (constrain (= y (* x 20)))
       (list x y))

(4 80)
```

This implementation of `amb` is dynamically scoped, which means that it is
possible to write programs whose definitions of ambiguous variables and
constraints can cross the function boundary.

```lisp
AMB> (flet ((foo (x) (constrain (evenp x)))
            (bar (x) (constrain (oddp x))))
       (list (amb ((x '(1 3 5 8 11 19))) (foo x) x)
             (amb ((x '(1 3 5 8 11 19))) (bar x) x)))
(8 1)
```

It is obviously possible that `amb` does not find a successful match if
the constraints it is given are impossible to meet with the given list of
values. In such a case, this implementation of `amb` by default signals a
warning and returns `nil`.

```lisp
AMB> (flet ((foo (x) (constrain (evenp x)))
            (bar (x) (constrain (oddp x))))
       (amb ((x '(1 3 5 8 11 19)))
         (foo x)
         (bar x)
         x))
;; WARNING: AMB for the default stack failed to match.
NIL
```

Each set of values can optionally be shuffled in order, e.g. to produce a
"random" solution for a given problem. (Note that "random" is quoted because it
does not mean that all possbible solutions are going to have equal probabilities
of appearing; it merely randomizes the order in which the values are tried.)

```lisp
AMB> (amb ((x (alexandria:iota 100))
           (y (alexandria:iota 100))
           (z (alexandria:iota 100)))
       (constrain (< x y z))
       (list x y z))
(0 1 2)

AMB> (amb ((x (iota 100) :shufflep t)
           (y (iota 100) :shufflep t)
           (z (iota 100) :shufflep t))
       (constrain (< x y z))
       (list x y z))
(13 87 98)
```

The `amb` operator can be programmed to modify its behavior for signaling
`amb-failure` in case of a match failure.

```lisp
AMB> (handler-bind ((amb-failure #'princ))
       (amb ((:signalp nil))))
NIL

AMB> (handler-bind ((amb-failure #'princ))
       (amb ((:signalp signal))))
AMB for the default stack failed to match.
NIL

AMB> (handler-bind ((amb-failure #'princ))
       (amb ((:signalp warn))))
AMB for the default stack failed to match.
;; WARNING: AMB for the default stack failed to match.
NIL

AMB> (handler-bind ((amb-failure #'princ))
       (amb ((:signalp error))))
AMB for the default stack failed to match.
;; Error: AMB for the default stack failed to match.
;;   [Condition of type AMB-FAILURE]
```

In case of multiple nested `amb` forms, only the outermost one signals in case
of a failed match.

```lisp
AMB> (amb ((x '(1 2 3)))
       (amb ((y '(10 20 30)))
         (amb ((z '(100 200 300)))
           (constrain (= x y z)))))
;; WARNING: AMB for stack 'AMB-STACK failed to match.
NIL
```

The `amb` operator can be programmed to use multiple stacks. Each new stack
effectively creates a new "instance" of `amb`, therefore isolating multiple
`amb` invocations from affecting one another.

This is especially important when handling `amb-failure` conditions across
multiple functions, since `amb` has dynamic scope and not isolating multiple
`amb` stacks from one another can introduce unintended backtracking and
therefore fail to find a solution.

```lisp
(defun inner (x)
  (handler-case (amb ((y (mapcar #'- x)))
                  (constrain (< 0 y))
                  y)
    (amb-failure () 10)))

(defun outer (args)
  (amb ((x args))
    (let ((y (inner args)))
      (list x y))))

AMB> (outer '(1 2 3))
;; WARNING: AMB for the default stack failed to match.
NIL
```

In the above example, the `amb` in `inner` fails to meet its constraint and
backtracks immediately to the `amb` in `outer`, which skips the `handler-case`
that would be triggered by an `amb-failure` being signaled from the inner `amb`.
A solution is to isolate the `amb` invocations from one another by using
multiple stacks for them:

```lisp
(defun inner2 (x)
  (handler-case (amb ((:stack foo) ; uses stack FOO
                      (y (mapcar #'- x)))
                  (constrain (< 0 y) foo) ; uses stack FOO
                  y)
    (amb-failure () 10)))

(defun outer2 (args)
  (amb ((:stack bar) ; uses stack BAR
        (x args))
    (let ((y (inner2 args)))
      (list x y))))

AMB> (outer2 '(1 2 3))
(1 10)
```
