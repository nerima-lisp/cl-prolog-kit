;;;; The unification builtins =/2, \=/2 and unify_with_occurs_check/2, plus the
;;;; occurs_check flag lookup that the first two honor.
;;;;
;;;; Split out of control.lisp, which keeps call/N, cut, if-then-else,
;;;; catch/throw and the rest of the control constructs and loads immediately
;;;; after.

(in-package #:cl-prolog-kit)

(defun %occurs-check-flag (rulebase)
  "The rulebase's occurs_check flag value (\"TRUE\", \"FALSE\", or \"ERROR\")."
  (%prolog-flag-value rulebase (%find-prolog-flag "OCCURS_CHECK")))

(defun %flagged-unify (left right environment rulebase operation)
  "Unify LEFT and RIGHT honoring the occurs_check flag.  Returns (VALUES
EXTENDED OK): TRUE checks (fails on a cycle), FALSE allows a cyclic binding,
ERROR raises when a cycle would form.  Shared by =/2 and \\=/2."
  (let ((mode (%occurs-check-flag rulebase)))
    (cond
      ((string= mode "FALSE") (unify left right environment nil))
      ((string= mode "ERROR")
       (multiple-value-bind (extended ok) (unify left right environment t)
         (if ok
             (values extended t)
             (progn
               (when (nth-value 1 (unify left right environment nil))
                 (%raise-evaluation-error
                  "OCCURS_CHECK" environment operation
                  "unification would create a cyclic term"))
               (values nil nil)))))
      (t (unify left right environment t)))))

(define-builtin (= left right) (rulebase environment depth emit)
  ;; =/2 honors the occurs_check flag: TRUE (default) checks and fails on a
  ;; cycle, FALSE allows a cyclic binding, ERROR raises when a cycle would form.
  (let ((mode (%occurs-check-flag rulebase)))
    (if (string= mode "TRUE")
        ;; Fast, common path: single occurs-checked unify + constraint hook.
        (%constraint-unify-emit left right environment emit :occurs-check t)
        (multiple-value-bind (extended ok)
            (%flagged-unify left right environment rulebase (%iso-atom "="))
          (when ok
            (if *constraint-post-unify-hook*
                (funcall *constraint-post-unify-hook* extended emit)
                (funcall emit extended)))))))

(define-builtin (unify_with_occurs_check left right)
    (rulebase environment depth emit)
  ;; UNIFY always performs the occurs check regardless of the occurs_check flag.
  (%unify-emit left right environment emit))

(define-builtin (|\\=| left right) (rulebase environment depth emit)
  ;; \=/2 is \+ (X = Y); it honors occurs_check like =/2 (ERROR still raises).
  (unless (nth-value 1 (%flagged-unify left right environment rulebase
                                       (%iso-atom "\\=")))
    (funcall emit environment)))
