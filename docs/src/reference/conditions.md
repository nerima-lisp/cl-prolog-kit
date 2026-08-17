# Conditions and Errors

cl-prolog-kit surfaces failures through the Common Lisp condition system. This page
catalogues the exported condition types, their readers, and when each is
signalled. The one-line symbol index also appears in the
[API Reference](api.md#conditions).

## Two distinct failure modes

It helps to keep two ideas separate:

- **Logical failure** — a goal simply does not prove. This is *not* an error:
  `query-prolog` returns `()` and `prolog-succeeds-p` returns `nil`.
- **A signalled condition** — something is structurally wrong (a malformed
  goal, an exhausted depth bound, a type error). These are Common Lisp
  conditions you can `handler-case`.

Depth exhaustion in particular is deliberately an error, not silent failure, so
an incomplete search is never mistaken for "no solutions".

## Depth configuration and exhaustion

| Condition                     | Reader                              | Signalled when |
| ----------------------------- | ----------------------------------- | -------------- |
| `invalid-max-depth-error`     | `invalid-max-depth-error-value`     | `:max-depth` is not `nil` or a non-negative integer. |
| `prolog-depth-limit-exceeded` | `prolog-depth-limit-exceeded-goal`  | User-rule resolution is bounded by `:max-depth` and the bound is exhausted. |

The reader returns the offending value or goal for diagnostics. See
[Semantics](semantics.md) for what each depth value permits.

## Structurally invalid goals

| Condition            | Reader                    | Signalled when |
| -------------------- | ------------------------- | -------------- |
| `invalid-goal-error` | `invalid-goal-error-goal` | A goal is not a symbol or symbol-headed list, or a `:when` guard is not a function. |

`invalid-goal-error-goal` returns the offending goal. Calling a *known* name at
an unsupported arity is different: it denotes an undefined procedure and signals
the ISO `existence_error` from the runtime hierarchy below.

## Prolog throws

| Condition          | Reader                   | Signalled when |
| ------------------ | ------------------------ | -------------- |
| `prolog-exception` | `prolog-exception-term`  | A Prolog-level `throw/1` propagates out, or an ISO error term is raised. |

`prolog-exception-term` returns the thrown Prolog term. Prolog-level `catch/3`
intercepts these inside a query.

## The runtime error hierarchy

These mirror the ISO error taxonomy. All are subtypes of
`prolog-runtime-error`:

- `prolog-runtime-error` — the root of the runtime hierarchy.
- `prolog-instantiation-error` — a goal needed a bound term but got a variable.
- `prolog-type-error` — a term was the wrong kind (e.g. an atom where a number
  was required).
- `prolog-domain-error` — a term was the right type but outside the valid
  domain (e.g. a negative length).
- `prolog-permission-error` — an operation was not permitted on that object.
- `prolog-existence-error` — a referenced procedure or object does not exist
  (e.g. a known name at an unsupported arity).
- `prolog-evaluation-error` — an arithmetic evaluation failed (e.g. division
  by zero).
- `prolog-resource-error` — a resource bound was exceeded.
- `prolog-syntax-error` — text that had to be read as a term or a number was not
  readable as one, as `read_term/2,3`, `term_to_atom/2`,
  `read_term_from_atom/3`, `number_chars/2` and `number_codes/2` all report
  (ISO 13211-1 8.14.1.3, 8.16.7.3, 8.16.8.3).
- `prolog-representation-error` — a term is well formed but outside what this
  implementation can represent, such as an integer that names no character
  (ISO 13211-1 7.12.2 (e)).

## Arithmetic diagnostics

| Condition                      | Readers |
| ------------------------------ | ------- |
| `arithmetic-evaluation-error`  | `arithmetic-error-expression`, `arithmetic-error-reason` |

The readers return the offending expression and a reason describing why
evaluation failed. See [Arithmetic and Comparison](../guide/arithmetic.md).

## Parser resource limits

| Condition                       | Readers |
| ------------------------------- | ------- |
| `prolog-parser-resource-error`  | `-resource`, `-limit`, `-observed`, `-position` |

Raised when reading Prolog *text* exceeds one of the configurable parser bounds.
This is a plain CL `error`, not a `prolog-runtime-error`, because it is raised in
the lexer beneath the engine's condition hierarchy. Inside a running
`consult`/`load_files` goal the same breach is translated into a catchable ISO
`resource_error/1` term. The full list of bounds and their defaults is on the
[Parser Resource Limits](parser-limits.md) page.

## Process-level halt

| Condition     | Reader             | Signalled when |
| ------------- | ------------------ | -------------- |
| `prolog-halt` | `prolog-halt-code` | `halt/0` or `halt/1` runs. |

!!! warning "`halt` is not catchable by `catch/3`"
    `prolog-halt` is deliberately **not** a `prolog-exception`, so Prolog-level
    `catch/3` never intercepts it. The embedding application decides how to
    translate the condition into process termination. `prolog-halt-code`
    returns the requested exit code.

## Handling conditions from Lisp

Because these are ordinary Common Lisp conditions, standard handlers apply:

```lisp
(handler-case
    (query-prolog *rb* '(ancestor tom ?who) :max-depth 2)
  (prolog-depth-limit-exceeded (c)
    (format t "~&gave up at goal: ~S~%"
            (prolog-depth-limit-exceeded-goal c))))
```

For failures *inside* Prolog, prefer Prolog-level `catch/3` and `throw/1` — see
the control goals in [Builtin Goals](../guide/builtin-goals.md).
