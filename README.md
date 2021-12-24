# AMB

This is an implementation of John McCarthy's ambiguous operator written in
portable Common Lisp.

The `amb` operator implements a system which is capable of automatically
searching for a set of values for variables (henceforth called *ambiguous
variables*) for which a set of constraints is satisfied. The operator and its
example use are described in detail in
[Structure and Interpretation of Computer Programs Chapter 4.3 (Nondeterministic
Computing)](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-28.html).

See the [manual](doc/MANUAL.md) and the [SICP test cases](t/test-sicp.lisp) for
examples.

## API

The direct API consists of two macros, `amb` and `constrain`, which,
respectively, bind ambiguous variables and place constraints which, if not met,
cause the code to backtrack and select the next combination of values to try.

If no match is found, a warning `amb-failure` might be signaled. The `amb` stack
for which no match was found can be retrieved from that condition via the
`amb-failure-stack` reader function.

* Macro **`AMB`**
  * Binds one or more ambiguous variables and establishes a dynamic
    environment in which it is possible to place constraints via the
    `constrain` macro. If all of the constraints are satisfied and the body
    evaluates to a true value, it is returned; otherwise, a next set of values
    is tried for a match. If `amb` runs out of combinations to try,
    `amb-failure` might be signaled depending on the value of `:signalp` option.
  * Syntax: `(amb bindings-and-options &body body)`
    * `bindings-and-options`: a list of bindings and options.
      * Binding syntax: `(variable values &key shufflep)`
        * `variable` must be a symbol naming a variable,
        * `values` is an expression that is evaluated to produce a list of
          values that will be tried in order to find a match.
        * `shufflep`, if true, randomizes the order in which the values will be
          tried. Default behavior is no randomization.
      * Option **`:STACK`**
        * Sets the stack for the given `amb` invocation. The default is a
          default stack.
        * Syntax: `(:stack STACK-NAME)`, where `STACK-NAME` is a
          symbol.
      * Option **`:SIGNALP`**
        * Sets the warning mode for the given `amb` invocation: no signaling in
          case of `nil`, or using, respectively, `signal`, `warn`, or `error` to
          signal the `amb-failure` warning.
        * Syntax: `(:signalp MODE)`, where `MODE` is one of `nil`, `signal`,
          `warn`, or `error`.
* Macro **`CONSTRAIN`**
  * Syntax: `(constrain constraint &optional stack)`
    * `constraint` is an expression that is evaluated to produce a boolean
      value. If it is true, it is returned; otherwise, it causes the program to
      backtrack in order to select the next set of values to try.
    * `stack` selects the stack for the given `amb` invocation. The default is a
    default stack.
* Condition Type **`AMB-FAILURE`**
  * The warning signaled whenever the outermost `amb` form fails to find a match
    for its contents.
  * Reader: **`AMB-FAILURE-STACK`** - returns the `amb` stack for which no match
  was found.

## Differences from Screamer

[Screamer](https://github.com/nikodemus/screamer/) offers a much more complete
and optimized environment for writing nondeterministic Common Lisp, at a cost
of being big in terms of lines of code and complexity, as well as needing to
shadow many symbols of standard Common Lisp.

This implementation of `amb` is meant to achieve the converse: it should be easy
to understand and its code is meant to fit on a single sheet of paper.

Therefore: for complex and/or production uses, please consider using Screamer.
For simple tasks and working with SICP, this `amb` implementation should be a
decent fit.

## License

MIT.
