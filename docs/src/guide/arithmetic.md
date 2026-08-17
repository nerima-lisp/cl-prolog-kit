# Arithmetic and Comparison

cl-prolog-kit evaluates arithmetic through the `is/2` goal and a family of
comparison goals. Expressions are written as **prefix, Lisp-shaped terms** —
`(+ 20 (* 2 11))`, not the infix `20 + 2 * 11` of textbook Prolog.

## Evaluating expressions with `is/2`

`(is ?x expr)` evaluates `expr` and unifies the numeric result with `?x`:

```lisp
(query-prolog (make-rulebase) '(is ?total (+ 20 (* 2 11))))
;; => (((?TOTAL . 42)))
```

The engine never calls `eval` on user data; arithmetic is a dedicated evaluator
over the operators below.

## Binary operators

Core arithmetic:

| Operator | Meaning                                                       |
| -------- | ------------------------------------------------------------- |
| `+`      | addition                                                      |
| `-`      | subtraction                                                   |
| `*`      | multiplication                                                |
| `/`      | division — always evaluates to a float, and rejects a zero divisor |
| `//`     | integer division, truncating toward zero (integers only)      |
| `div`    | floored integer division (integers only)                      |
| `rem`    | remainder, matching `//` (integers only)                      |
| `mod`    | modulo, matching `div` (integers only)                        |
| `min`    | smaller of two values                                         |
| `max`    | larger of two values                                          |
| `**`     | exponentiation (bounded against runaway growth)               |
| `^`      | exponentiation (bounded against runaway growth)               |
| `atan`   | two-argument arctangent `atan(y, x)`                          |
| `atan2`  | two-argument arctangent (alias for `atan/2`)                  |
| `gcd`    | greatest common divisor (integers only)                      |

Bitwise operators (integers only):

| Operator | Meaning                          |
| -------- | -------------------------------- |
| `/\`     | bitwise AND                      |
| `\/`     | bitwise OR                       |
| `xor`    | bitwise exclusive OR             |
| `<<`     | arithmetic left shift (result bounded against runaway growth, like `**`/`^`) |
| `>>`     | arithmetic right shift (cannot grow the result)                             |

## Unary operators

Sign and rounding:

| Operator   | Meaning                                   |
| ---------- | ----------------------------------------- |
| `+`        | identity                                  |
| `-`        | negation                                  |
| `abs`      | absolute value                            |
| `sign`     | sign (-1, 0, or 1); `signum` is an alias  |
| `truncate` | truncate toward zero                      |
| `round`    | round to nearest                          |
| `ceiling`  | round up                                  |
| `floor`    | round down                                |

Float conversion (real arguments only):

| Operator                | Meaning                                    |
| ----------------------- | ------------------------------------------ |
| `float`                 | convert to a double float                  |
| `float_integer_part`    | the integer part, as a float               |
| `float_fractional_part` | the fractional part                        |

Transcendental (real arguments; `sqrt`/`log` reject out-of-domain inputs):

| Operator | Meaning                          |
| -------- | -------------------------------- |
| `sqrt`   | square root                      |
| `exp`    | exponential, `e^x`               |
| `log`    | natural logarithm                |
| `sin`    | sine                             |
| `cos`    | cosine                           |
| `tan`    | tangent                          |
| `asin`   | arcsine                          |
| `acos`   | arccosine                        |
| `atan`   | arctangent (one argument)        |

Integer bit inspection (integers only):

| Operator   | Meaning                                                            |
| ---------- | ----------------------------------------------------------------- |
| `\`        | bitwise complement                                                |
| `msb`      | zero-based index of the most significant set bit (positive only)  |
| `lsb`      | zero-based index of the least significant set bit (positive only) |
| `popcount` | count of set bits (non-negative only)                             |

## Constants

Zero-argument constants evaluate directly:

| Constant | Value           |
| -------- | --------------- |
| `pi`     | π               |
| `e`      | Euler's number  |

```lisp
(query-prolog (make-rulebase) '(is ?x pi))
;; ?X is bound to pi as a float
```

## Numeric comparison

These goals evaluate both sides as arithmetic expressions and compare the
numeric values:

| Goal        | True when                        |
| ----------- | -------------------------------- |
| `(=:= a b)` | `a` equals `b`                   |
| `(=\= a b)` | `a` differs from `b`             |
| `(< a b)`   | `a` is less than `b`             |
| `(=< a b)`  | `a` is less than or equal to `b` |
| `(> a b)`   | `a` is greater than `b`          |
| `(>= a b)`  | `a` is greater than or equal to `b` |

```lisp
(query-prolog (make-rulebase) '(=:= (+ 1 2) 3))     ; => (nil)   -- succeeds
(query-prolog (make-rulebase) '(< 2 (* 2 2)))       ; => (nil)   -- succeeds
```

!!! note "Arithmetic comparison vs. term comparison"
    `=:=` compares *numeric values* after evaluation. To compare terms by the
    standard order of terms instead (`==`, `@<`, `compare`, …), see the term
    comparison goals in [Builtin Goals](builtin-goals.md) and the symbol list
    in the [API Reference](../reference/api.md#builtin-goal-symbols).

## Relational arithmetic

Three goals relate integers rather than evaluating a one-shot expression. They
are callable in queries and parsed Prolog source, but are not exported as Common
Lisp symbols (see the
[Prolog-source goals](../reference/api.md#prolog-source-and-query-goals) catalogue).

- **`(between low high value)`** — `low` and `high` must be instantiated
  integers. When `value` is unbound it enumerates every integer from `low` to
  `high` inclusive; when `value` is bound it succeeds once iff
  `low <= value <= high`.
- **`(succ predecessor successor)`** — the successor relation over non-negative
  integers, usable in either direction. At least one argument must be bound: if
  `predecessor` is bound, `successor` is `predecessor + 1`; if `successor` is
  bound and positive, `predecessor` is `successor - 1`. A negative argument
  raises `domain_error(not_less_than_zero, _)`.
- **`(plus a b c)`** — the integer relation `A + B =:= C`, usable with any one
  argument unbound (it is solved from the other two). At least two arguments
  must be instantiated integers, else `instantiation_error`; a non-integer
  argument raises `type_error(integer, _)`. With all three bound it acts as a
  check.

```lisp
(query-prolog (make-rulebase) '(between 1 3 ?x))
;; => (((?X . 1)) ((?X . 2)) ((?X . 3)))

(query-prolog (make-rulebase) '(succ 4 ?y))
;; => (((?Y . 5)))
```

## Which numbers count

Prolog numbers are **integers** and **floating-point** values. Common Lisp
ratios and complex values are not accepted by numeric or atomic term predicates
and do not receive numeric term ordering. Passing them raises the corresponding
type error rather than silently coercing.

## Using arithmetic in rules

Guards let a rule fire only when an arithmetic condition holds. In the DSL you
write the condition as an expression and the macro compiles it to a closure:

```lisp
(define-rulebase *scores*
  ((score bob 42))
  ((score amy 8))
  ((rich ?x) (score ?x ?n) (:when (> ?n 10))))

(query-prolog *scores* '(rich ?who))
;; => (((?WHO . BOB)))
```

See [Rule DSL](rule-dsl.md) for `:when` guards and [Semantics](../reference/semantics.md)
for how guards are evaluated at proof time.

## Finite-domain constraints

For constraint-style integer reasoning — as opposed to one-shot evaluation —
cl-prolog-kit also ships finite-domain (FD) goals. These constrain variables and
enumerate solutions rather than evaluating a ground expression.

### Domain assignment

`(in x domain)` constrains a single variable; `(ins vars domain)` constrains a
list of variables. A **domain** is either an integer interval written with the
`..` range operator or an explicit list of integers:

| Domain form         | Meaning                                  |
| ------------------- | ---------------------------------------- |
| `(Lower .. Upper)`  | every integer from `Lower` to `Upper`    |
| `(1 3 5 7)`         | exactly the listed integers              |

```lisp
(query-prolog (make-rulebase)
  '(and (in ?x (1 .. 3)) (indomain ?x)))
;; => (((?X . 1)) ((?X . 2)) ((?X . 3)))
```

!!! note "`..` is an operator, not a goal"
    `..` is the exported infix FD range operator (priority 450, `xfx`). It only
    has meaning inside a domain specification; it is not a callable predicate.

### Constraint and search goals

| Goal                                       | Role                                               |
| ------------------------------------------ | -------------------------------------------------- |
| `#=` `#\=` `#<` `#=<` `#>` `#>=`           | arithmetic constraints between FD terms            |
| `in` / `ins`                               | assign a domain to one variable / a list           |
| `all_different`                            | require a list of FD variables to be pairwise distinct |
| `indomain`                                 | enumerate a variable's remaining domain values     |
| `labeling`                                 | search a list of variables (`labeling(Options, Vars)`); `down` reverses the value order |

`#=/2` binds an unconstrained variable when the other operand is an integer, and
the binding is visible to ordinary unification and later goals. See
[Builtin Goals](builtin-goals.md#finite-domain-equality) for the reflexive-case
behavior of each comparison.
