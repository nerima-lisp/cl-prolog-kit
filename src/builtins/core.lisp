;;;; Builtin goal solvers.
;;;;
;;;; Every builtin is declared with DEFINE-BUILTIN and follows the engine's
;;;; CPS contract: emit each solution environment as it is found, never
;;;; collect intermediate result lists.

(in-package #:cl-prolog-kit)

(defun %unify-emit (left right environment emit)
  "EMIT the extension of ENVIRONMENT that unifies LEFT with RIGHT, if any."
  (multiple-value-bind (extended ok)
      (unify left right environment)
    (when ok
      (funcall emit extended))))

(defun %constraint-unify-emit (left right environment emit &key (occurs-check t))
  "Unify LEFT and RIGHT, then propagate any active constraint store.
OCCURS-CHECK is forwarded to UNIFY (see the `occurs_check' flag)."
  (multiple-value-bind (extended ok)
      (unify left right environment occurs-check)
    (when ok
      (if *constraint-post-unify-hook*
          (funcall *constraint-post-unify-hook* extended emit)
          (funcall emit extended)))))

(defun %term-unify-sequence (pairs environment emit)
  "EMIT the extension of ENVIRONMENT that unifies every (LEFT . RIGHT) pair
in PAIRS, in order, or nothing if any pair fails to unify."
  (labels ((unify-next (remaining current)
             (if (endp remaining)
                 (funcall emit current)
                 (multiple-value-bind (extended ok)
                     (unify (caar remaining) (cdar remaining) current)
                   (when ok
                     (unify-next (cdr remaining) extended))))))
    (unify-next pairs environment)))

(defmacro define-iso-builtin ((name &rest arguments) operation-name &body body)
  "Define a builtin whose body starts by resolving its arguments against
ENVIRONMENT under an ISO OPERATION-NAME context.  Each of ARGUMENTS is
either a symbol -- a define-builtin parameter that also gets a
RESOLVED-<ARGUMENT> binding via %TERM-RESOLVE -- or (SYMBOL :raw), a
pure-output parameter left unresolved.  Binds OPERATION to the ISO atom
for OPERATION-NAME, then splices in BODY."
  (let* ((parameter-names (mapcar (lambda (argument)
                                    (if (consp argument) (first argument) argument))
                                  arguments))
         (resolved-bindings
           (loop for argument in arguments
                 unless (and (consp argument) (eq (second argument) :raw))
                   collect (let ((parameter (if (consp argument)
                                                (first argument)
                                                argument)))
                             `(,(intern (format nil "RESOLVED-~A" parameter))
                               (%term-resolve ,parameter environment))))))
    `(define-builtin (,name ,@parameter-names) (rulebase environment depth emit)
       (let* ((operation (%iso-atom ,operation-name))
              ,@resolved-bindings)
         ,@body))))

(defun %proper-list-p (value)
  "True when VALUE is a finite proper list."
  (loop with tortoise = value
        with tail = value
        with power = 1
        with span = 0
        do (cond
             ((null tail) (return t))
             ((atom tail) (return nil)))
           (when (= span power)
             (setf tortoise tail
                   power (if (> power (floor most-positive-fixnum 2))
                             most-positive-fixnum
                             (* 2 power))
                   span 0))
           (setf tail (cdr tail)
                 span (1+ span))
           (when (eq tail tortoise)
             (return nil))))

(defun %entry-head (clause)
  (clause-head clause))

(defun %entry-body-term (clause)
  (let ((body (clause-body clause)))
    (cond
      ((null body) 'true)
      ((null (rest body)) (first body))
      (t (cons 'and body)))))

(defparameter +prolog-rule-functor+ (%intern-prolog-atom ":-")
  "The atom `:-'.

Prolog source text reads a rule as the `:-'/2 term this heads, so it is the
shape `assertz/1', `retract/1' and `retractall/1' exchange with Prolog code.
It is a different object from the keyword :- of the Lisp-level clause shape
(:- HEAD . BODY-GOALS), which stays the engine's internal spelling.")

(defun %prolog-rule-term-p (term)
  "True when TERM is a `:-'/2 rule term rather than a fact or the Lisp shape."
  (and (%proper-list-p term)
       (= 3 (length term))
       (symbolp (first term))
       (not (eq (first term) ':-))
       (%same-atom-text-p (first term) +prolog-rule-functor+)))

(defun %clause-rule-term (clause)
  "Render CLAUSE as the `:-'/2 term a Prolog-shaped retract/1 pattern matches.

A fact renders with the body `true', as ISO 13211-1 7.6.1 requires, so
`retract((h :- true))' finds the fact `h'."
  (list +prolog-rule-functor+ (%entry-head clause) (%entry-body-term clause)))

(defun %dynamic-pattern-term (pattern head)
  "Normalize a retract/1 or retractall/1 PATTERN to the shape stored clauses are
rendered in by %DYNAMIC-PATTERN-CLAUSE-TERM.

HEAD is PATTERN's head already in goal form, which a zero-arity head needs:
source text spells it as the bare atom `h' while the clause stores `(h)'."
  (cond
    ((symbolp pattern) head)
    ((%prolog-rule-term-p pattern) (list (first pattern) head (third pattern)))
    (t pattern)))

(defun %dynamic-pattern-clause-term (clause prolog-shape-p)
  "Render CLAUSE as the term a retract/1 or retractall/1 pattern unifies with.

A Prolog-shaped pattern -- what `(h :- B)' reads as -- matches the `:-'/2
rendering, in which a fact's body is `true'.  A Lisp-shaped pattern matches the
internal (:- HEAD . BODY-GOALS) shape, and a fact by its head alone."
  (cond
    (prolog-shape-p (%clause-rule-term clause))
    ((null (clause-body clause)) (%entry-head clause))
    (t (list* ':- (%entry-head clause) (clause-body clause)))))

(defun %freshen-dynamic-clause (clause)
  (%freshen-clause clause))

(defun %builtin-predicate-p (predicate arity)
  (and (symbolp predicate) (%goal-solver predicate arity)))

(defun %stored-clause-matches-predicate-p (stored predicate arity)
  (multiple-value-bind (entry-predicate entry-arity)
      (%entry-predicate-arity (%stored-clause-clause stored))
    (and (eq predicate entry-predicate) (= arity entry-arity))))

(defun %predicate-clause-matcher (predicate arity)
  "Return a predicate matching a stored clause against PREDICATE/ARITY,
shared by callers that check existence (SOME) or count matches (COUNT-IF)
over one module's stored clauses."
  (lambda (stored)
    (%stored-clause-matches-predicate-p stored predicate arity)))

(defun %rulebase-defines-predicate-p (rulebase predicate arity module)
  (some (%predicate-clause-matcher predicate arity)
        (%rulebase-module-entries rulebase module)))

(defun %ensure-dynamic-predicate (rulebase predicate arity goal environment
                                  &key (operation "MODIFY")
                                    (permission-type "STATIC_PROCEDURE"))
  "Reject modification or inspection of a static PREDICATE/ARITY."
  (when (or (%builtin-predicate-p predicate arity)
            (eq :static (%rulebase-predicate-property
                         rulebase predicate arity *current-prolog-module*))
            (and (null (%rulebase-predicate-property
                        rulebase predicate arity *current-prolog-module*))
                 (%rulebase-defines-predicate-p
                  rulebase predicate arity *current-prolog-module*)))
    (%raise-permission-error
     operation permission-type (list '/ predicate arity) environment
     (first goal) "static procedures cannot be inspected or modified")))

(defun %ensure-callable (term environment operation)
  "Return TERM when it is callable, otherwise raise its ISO error."
  (when (logic-var-p term)
    (%raise-instantiation-error environment operation
                                "callable term must be instantiated"))
  (unless (or (symbolp term)
              (and (%proper-list-p term) (%goal-form-p term)))
    (%raise-type-error "CALLABLE" term environment operation
                       "expected a callable term"))
  (%ensure-goal-form term))

(defun %dynamic-clause-head (term environment operation)
  "Validate TERM as a fact or rule and return its callable head."
  (when (logic-var-p term)
    (%raise-instantiation-error environment operation
                                "dynamic clause must be instantiated"))
  (unless (or (symbolp term) (%proper-list-p term))
    (%raise-type-error "CALLABLE" term environment operation
                       "dynamic clause must be a proper callable term"))
  (cond
    ((and (consp term) (eq (first term) ':-))
     (unless (consp (rest term))
       (%raise-type-error "CALLABLE" term environment operation
                          "rule must contain a callable head"))
     (%ensure-callable (second term) environment operation))
    ((%prolog-rule-term-p term)
     (%ensure-callable (second term) environment operation))
    (t (%ensure-callable term environment operation))))

;;;; ISO Prolog flags

(defstruct (prolog-flag (:constructor %make-prolog-flag (name default-value
                                                         allowed-values)))
  name
  default-value
  allowed-values)

(defmacro define-prolog-flags (&body specifications)
  "Define the implementation's flag data independently from builtin logic."
  `(defparameter *prolog-flag-specifications*
     (list ,@(loop for (name value allowed-values) in specifications
                   collect `(%make-prolog-flag
                              ,(string-upcase (symbol-name name))
                              ,value ',allowed-values)))))

(define-prolog-flags
  (bounded "FALSE" ())
  ;; ISO 8.17 admits `unbounded', but every other resource this engine exposes
  ;; carries a configurable bound, and an unbounded arity is the one hole in
  ;; that: `functor(T, foo, HUGE)' would allocate without limit.
  (max_arity *max-prolog-term-arity* ())
  (integer_rounding_function "TOWARD_ZERO" ())
  (char_conversion "OFF" ("ON" "OFF"))
  (debug "OFF" ("ON" "OFF"))
  (unknown "ERROR" ("ERROR" "FAIL" "WARNING"))
  (double_quotes "CODES" ("CODES" "CHARS" "ATOM" "STRING"))
  (occurs_check "TRUE" ("TRUE" "FALSE" "ERROR")))

(defun %double-quotes-mode (rulebase)
  "Return the rulebase's double_quotes flag as a keyword for *DOUBLE-QUOTES*."
  (let ((value (%prolog-flag-value rulebase (%find-prolog-flag "DOUBLE_QUOTES"))))
    (cond
      ((string= value "CHARS") :chars)
      ((string= value "ATOM") :atom)
      ((string= value "STRING") :string)
      (t :codes))))

(defun %rulebase-active-char-conversions (rulebase)
  "Return the conversion table when char_conversion is on and non-empty."
  (let ((table (rulebase-char-conversions rulebase)))
    (when (and (plusp (hash-table-count table))
               (string= "ON" (%prolog-flag-value
                              rulebase (%find-prolog-flag "CHAR_CONVERSION"))))
      table)))

(define-term-guard %prolog-flag-name (term)
  :resolve t
  :instantiation "flag name must be instantiated"
  :accept (symbolp term)
  :type "ATOM"
  :type-message "flag name must be an atom"
  :result (string-upcase (symbol-name term)))

(defun %find-prolog-flag (name)
  (find name *prolog-flag-specifications*
        :key #'prolog-flag-name :test #'string=))

(defun %prolog-flag-value (rulebase flag)
  "Return FLAG's rulebase-local value, installing its immutable default lazily."
  (let* ((name (prolog-flag-name flag))
         (values (rulebase-prolog-flag-values rulebase)))
    (multiple-value-bind (value present-p) (gethash name values)
      (if present-p
          value
          (setf (gethash name values) (prolog-flag-default-value flag))))))

(defun %external-prolog-flag-value (value)
  (if (stringp value) (%iso-atom value) value))

(define-term-guard %resolve-prolog-flag-value (term)
  :resolve t
  :instantiation "flag value must be instantiated"
  :result (if (symbolp term) (string-upcase (symbol-name term)) term))

(define-builtin (current_prolog_flag name value)
    (rulebase environment depth emit)
  (let ((resolved-name (logic-substitute name environment)))
    (if (logic-var-p resolved-name)
        (dolist (flag *prolog-flag-specifications*)
          (%unify-emit
           name (%iso-atom (prolog-flag-name flag)) environment
           (lambda (named-environment)
             (%unify-emit value
                          (%external-prolog-flag-value
                          (%prolog-flag-value rulebase flag))
                          named-environment emit))))
        (let* ((operation (%iso-atom "CURRENT_PROLOG_FLAG"))
               (flag (%find-prolog-flag
                      (%prolog-flag-name name environment operation))))
          ;; ISO 13211-1 8.17.2.3: an atom that names no flag is a
          ;; domain_error(prolog_flag, Name), not a silent failure.
          (unless flag
            (%raise-domain-error "PROLOG_FLAG" resolved-name environment
                                 operation "unknown Prolog flag"))
          (when flag
            (%unify-emit value
                         (%external-prolog-flag-value
                          (%prolog-flag-value rulebase flag))
                         environment emit))))))

(define-builtin (set_prolog_flag name value)
    (rulebase environment depth emit)
  (let* ((operation (%iso-atom "SET_PROLOG_FLAG"))
         (flag-name (%prolog-flag-name name environment operation))
         (flag (%find-prolog-flag flag-name)))
    (unless flag
      (%raise-domain-error "PROLOG_FLAG" (logic-substitute name environment)
                           environment operation "unknown Prolog flag"))
    (unless (prolog-flag-allowed-values flag)
      (%raise-permission-error "MODIFY" "FLAG" (%iso-atom flag-name)
                               environment operation
                               "implementation-defined flag is read-only"))
    (let ((new-value (%resolve-prolog-flag-value value environment operation)))
      (unless (member new-value (prolog-flag-allowed-values flag) :test #'equal)
        ;; ISO 13211-1 8.17.1.3 blames the pair: which value is wrong only means
        ;; something together with the flag it was offered to.
        (%raise-domain-error "FLAG_VALUE"
                             (%iso-term "+" (logic-substitute name environment)
                                        (logic-substitute value environment))
                             environment operation "invalid Prolog flag value"))
      (setf (gethash flag-name (rulebase-prolog-flag-values rulebase)) new-value)
      (funcall emit environment))))

(defun %clause-rule-entry (head body rulebase goal environment)
  "Build a freshly renamed clause entry from a rule's HEAD and BODY goal list.

HEAD is normalized to goal form, but BODY is stored exactly as it arrived --
the same asymmetry the source loader produces, so a zero-arity body goal stays
the bare atom Prolog text spells and `clause/2' hands it back unchanged."
  (when (logic-var-p head)
    (%raise-instantiation-error environment (first goal)
                                "a rule's head must be instantiated"))
  (let ((callable-head (%ensure-goal-form head)))
    (unless (%goal-form-p callable-head)
      (%raise-type-error "CALLABLE" head environment (first goal)
                         "a rule's head must be callable"))
    (%ensure-dynamic-predicate rulebase (first callable-head)
                               (length (rest callable-head)) goal environment)
    (unless (every #'%goal-form-p (mapcar #'%ensure-goal-form body))
      (%raise-type-error "CALLABLE"
                         (find-if-not (lambda (element)
                                        (%goal-form-p (%ensure-goal-form element)))
                                      body)
                         environment (first goal)
                         "every rule body element must be callable"))
    (%freshen-clause (make-clause callable-head body))))

(defun %clause-term-entry (term rulebase goal environment)
  "Convert a substituted dynamic clause TERM to a freshly renamed entry.

TERM may be a fact, the Lisp clause shape (:- HEAD . BODY-GOALS), or the `:-'/2
term Prolog source text reads a rule as.  ISO 13211-1 7.6.1 converts all three
to a clause, so `assertz((h :- a, b))' asserts a rule instead of a fact whose
functor happens to be `:-'."
  ;; Checked before anything else: a logic variable is a symbol, so without this
  ;; `assertz(X)' fell through to the fact branch and stored a clause whose head
  ;; was a fresh variable.  ISO 13211-1 8.9.1.3 requires an instantiation_error.
  (when (logic-var-p term)
    (%raise-instantiation-error environment (first goal)
                                "a dynamic clause must be instantiated"))
  (cond
    ((%prolog-rule-term-p term)
     (%clause-rule-entry (second term) (%body-goals (third term))
                         rulebase goal environment))
    ((and (%proper-list-p term) (eq (first term) ':-))
     ;; The Lisp clause shape (:- HEAD . BODY-GOALS).  Delegated to
     ;; %CLAUSE-RULE-ENTRY rather than repeated inline so both spellings of a
     ;; rule normalize their head the same way: this branch used to test
     ;; %GOAL-FORM-P on the raw head, which rejected the bare atom head that
     ;; the `:-'/2 branch above accepts through %ENSURE-GOAL-FORM, and its
     ;; failure path named an unbound variable instead of the culprit term.
     ;; (:- ) alone is caught here because it carries no head at all to blame.
     (unless (consp (rest term))
       (%raise-type-error "CALLABLE" term environment (first goal)
                          "a rule's head must be callable"))
     (%clause-rule-entry (second term) (cddr term) rulebase goal environment))
    ((or (symbolp term) (%goal-form-p term))
     (let ((normalized (%ensure-goal-form term)))
       (%ensure-dynamic-predicate rulebase (first normalized)
                                  (length (rest normalized)) goal environment)
       (let ((table (make-hash-table :test #'eq)))
         (make-clause (%freshen-term normalized table)))))
    (t
     ;; ISO 13211-1 8.9.1.3: the culprit is the clause term itself, not the
     ;; assert goal that carried it.
     (%raise-type-error "CALLABLE" term environment (first goal)
                        "a dynamic clause must be a fact or a rule"))))

(defun %entry-predicate-arity (entry)
  (let ((head (%entry-head entry)))
    (values (first head) (length (rest head)))))
