(in-package #:amb)

(setf
 (documentation 'amb-failure 'type)
 "The warning signaled whenever the outermost AMB form fails to find a match
for its contents."
 (documentation 'constrain 'function)
 "A macro for adding a constraint to an ambiguous variable. If the constraint
is satisfied, its value is returned (for convenience inside AMB); otherwise,
backtracking occurs (via CL:THROW)."
 (documentation 'amb 'function)
 "A macro implementation of John McCarthy's ambiguous operator.
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
case, AMB-FAILURE is signaled (via WARN) and NIL is returned.")
