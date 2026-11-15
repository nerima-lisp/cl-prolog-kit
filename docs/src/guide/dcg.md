# DCG Grammars

Definite Clause Grammars (DCGs) are a front end for describing token grammars.
`def-dcg-rule` compiles a grammar body into an ordinary rule with two stream
arguments (the input and the remainder), so grammars run on the same engine as
everything else. See [Architecture](../reference/architecture.md) for how the front ends
converge on one runtime.

## A worked grammar

```lisp
(defparameter *grammar*
  (make-rulebase
   :clauses
   (list (def-dcg-rule noun (terminal :noun))
         (def-dcg-rule verb (terminal :verb))
         (def-dcg-rule sentence
           (dcg-star noun)
           (verb)
           (brace (= 1 1)))))) ; Lisp guard, like (:when ...)

(phrase *grammar* 'sentence '(:noun :noun :verb))
;; => NIL, T   (remainder, matched-p)

(phrase-all *grammar* 'sentence '(:noun :noun :verb))
;; => (NIL)
```

`sentence` matches zero or more nouns (`dcg-star noun`), then a verb, then a
`brace` guard. `phrase` returns the first parse; `phrase-all` returns the
remainder of every parse.

## Tokens

Tokens are either a bare **kind** symbol (`:noun`) or a `(kind . value)` cons.
Two matchers consume them:

- `(dcg-token-match kind input rest)` — matches the kind, returning the
  remaining input in `rest`.
- `(dcg-token-match-value kind value input rest)` — matches both kind and
  value.

## Grammar body elements

Inside `def-dcg-rule`, a body may contain:

- `(terminal KIND...)` — match one or more terminal token kinds.
- `(brace EXPR)` — a Lisp guard, exactly like `(:when ...)` in the
  [Rule DSL](rule-dsl.md#guards-with-when).
- **non-terminal calls** — invoke another DCG rule by name.
- **combinator forms** — see below.

## Combinators

Usable as goals inside a grammar body:

| Combinator            | Meaning                                             |
| --------------------- | --------------------------------------------------- |
| `dcg-alt`             | ordered alternation (first matching branch)         |
| `dcg-opt`             | optional — match zero or one                        |
| `dcg-star`            | repetition — zero or more                           |
| `dcg-plus`            | repetition — one or more                            |
| `dcg-error-recovery`  | skip ahead to a synchronizing token                 |

!!! note "Nullable rules terminate"
    `dcg-star` refuses to repeat a rule that consumed no input, so nullable
    rules cannot loop forever.

`dcg-error-recovery` skips ahead to the next token whose kind is in the internal
`*dcg-sync-tokens*` parameter (`:t-rparen`, `:t-semi`, `:t-eof`).

## Reference

| Symbol                   | Role                                                       |
| ------------------------ | ---------------------------------------------------------- |
| `def-dcg-rule`           | Compile a grammar body into a two-stream rule.             |
| `phrase`                 | `(phrase rulebase rule-name input)` → `(values remainder matched-p)` for the first parse. |
| `phrase-all`             | `(phrase-all rulebase rule-name input)` → the remainders of every parse. |
| `phrase-next-terminals`  | The terminals that may follow a partial input, for completion. See [Next terminals](../reference/api.md#next-terminals-after-a-prefix). |
| `phrase-token-valid-p`   | Whether a typed token can continue a partial input. See [Next terminals](../reference/api.md#next-terminals-after-a-prefix). |
| `dcg-alt` … `dcg-error-recovery` | Combinators usable as goals.                       |
| `dcg-token-match`        | Match a token kind.                                        |
| `dcg-token-match-value`  | Match a token kind and value.                              |

The exported DCG symbols are also listed in the
[API Reference](../reference/api.md#dcg).

## Next steps

- [Rule DSL](rule-dsl.md) — the clause forms DCG rules compile into.
- [Builtin Goals](builtin-goals.md) — goals you can call from a grammar body.
