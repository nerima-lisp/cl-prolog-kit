;;;; PHRASE-NEXT-TERMINALS: the terminals a grammar accepts after a prefix.
;;;;
;;;; Two stages, both on the ordinary prover with an unbound tail after the
;;;; known tokens and a private table session.
;;;;
;;;; Stage 1 (enumerate) runs NONTERMINAL on PREFIX ++ T.  Every goal
;;;; dispatch is classified by %PHRASE-PROBE-DISPATCH/K:
;;;;
;;;; - User predicates and monotonic builtins run normally; after such a
;;;;   builtin the tail is checked again, for bindings made without the
;;;;   post-unification hook (append/3).
;;;; - Transparent control (\+, once, ->, ...) runs in a scope.  If the
;;;;   scope binds a variable that was tainted at its entry, the outermost
;;;;   scope reruns in a relaxed form that only over-approximates: \+ and
;;;;   forall succeed, once(G) is G, (C -> T ; E) is (C, T ; E).
;;;; - Any other builtin or foreign predicate whose arguments share a tainted
;;;;   variable does not run: the branch reports what it has and fails.
;;;; - A cut is skipped once the branch has read T or been relaxed.
;;;;
;;;; The taint is T before the read and vars(H) + Rest after the branch reads
;;;; T as [H|Rest].  The branch then reports H when it reads Rest, reaches a
;;;; solution, stops at a builtin, or runs out of its phase budget, and fails.
;;;;
;;;; Stage 2 (verify) runs NONTERMINAL on PREFIX ++ [Token|R] with normal
;;;; semantics and accepts once the derivation reads R, passes R to a
;;;; builtin outside the monotonic set, or succeeds.  Each ground candidate
;;;; of stage 1 is reported only after it passes stage 2.  END-P is
;;;; phrase/2 on PREFIX itself.

(in-package #:cl-prolog-kit)

(defparameter +phrase-probe-monotonic-builtins+
  '("AND" "OR" "CALL" "=" "TRUE" "FAIL" "FALSE" "MEMBER" "APPEND" "BETWEEN"
    "LENGTH" "SELECT" "PHRASE"
    ;; def-dcg-rule's matchers bind the probe's open input to the expected
    ;; token (dcg-runtime.lisp), and its alternatives are a disjunction.
    "DCG-TOKEN-MATCH" "DCG-TOKEN-MATCH-VALUE" "DCG-ALT" "DCG-OPT")
  "Builtins whose solutions only bind variables, never test their binding
state, so running them on a partial input loses no solution of a full one.")

(defparameter +phrase-probe-transparent-control+
  '("NOT" "ONCE" "->" "IF-THEN-ELSE" "*->" "SOFT-IF-THEN-ELSE" "FORALL"
    "IGNORE")
  "Control constructs the probe runs in a scope and relaxes when needed.")

(defparameter *phrase-probe-phase-budget* 250
  "Goal dispatches one branch may make after it read the tail before the
probe reports the read token as it stood at the read.

A generator of ever larger tokens (nat(s(N)) :- nat(N)) reports one
candidate every few dispatches, and checking a candidate in stage 2 costs
time that grows with its size, which the inference count does not charge.
The whole run grows with the cube of this budget: 0.4 s at 250, 24 s at
1000 for that generator.  Guards on a typed token need far fewer
dispatches than 250.")

(defvar *phrase-probe-relaxed* nil
  "True in a branch that runs a relaxed control construct.")
(defvar *phrase-probe-scope* nil
  "CATCH tag of the innermost transparent-control scope, or NIL.")
(defvar *phrase-probe-scope-taint* nil
  "Tainted variables at the entry of the innermost scope.")

(defstruct (%phrase-probe (:constructor %make-phrase-probe
                              (mode tail report accept-tag)))
  "The open tail one probe run watches and what it does on events."
  (mode nil :type (member :enumerate :verify) :read-only t)
  (tail nil :read-only t)
  (report nil :read-only t)
  (accept-tag nil :read-only t))

(defstruct (%phrase-probe-phase
            (:constructor %make-phrase-probe-phase (head rest index)))
  "One branch's read of the tail as [HEAD|REST]."
  (head nil :read-only t)
  (rest nil :read-only t)
  ;; The environment at the read, to report HEAD as it stood then.
  (index nil :read-only t)
  (dispatches 0 :type fixnum))

(defun %phrase-probe-report-term (term)
  "Report TERM, already substituted, as a terminal or :ANY when unbound."
  (funcall (%phrase-probe-report *phrase-probe*)
           (if (logic-var-p term) :any :terminal)
           term))

(defun %phrase-probe-end-phase (phase index)
  "Report PHASE's head under INDEX and fail the branch by returning."
  (%phrase-probe-report-term
   (%logic-substitute-indexed (%phrase-probe-phase-head phase) index))
  nil)

(defun %phrase-probe-stop (index)
  "Report what the branch can claim without running further, and fail it."
  (let ((phase *phrase-probe-phase*))
    (if phase
        (%phrase-probe-end-phase phase index)
        (funcall (%phrase-probe-report *phrase-probe*) :any nil))
    nil))

(defun %phrase-probe-taint (index)
  "The variables a goal must not test: those of the tail as resolved now."
  (let ((term (%logic-substitute-indexed (%phrase-probe-tail *phrase-probe*) index)))
    (if (logic-var-p term)
        (list term)
        (%collect-variables term))))

(defun %phrase-probe-shares-taint-p (goal index)
  (let ((taint (%phrase-probe-taint index)))
    (and taint
         (intersection taint
                       (%collect-variables (%logic-substitute-indexed goal index))))))

(defun %phrase-probe-open-input-p (variable environment)
  "True when VARIABLE, an unbound input stream, is the probe's watched tail
or the rest of the current phase's read."
  (let ((index (%make-environment-index environment))
        (phase *phrase-probe-phase*))
    (or (eq variable (%walk-term-indexed (%phrase-probe-tail *phrase-probe*) index))
        (and phase
             (eq variable (%walk-term-indexed (%phrase-probe-phase-rest phase) index))))))

(defun %phrase-probe-table-key (key goal state)
  "Extend table KEY so the probe's watched variables stay distinct.

Variant keys rename every variable, so a recursive call on the open tail's
unread rest would otherwise replay the table of the call on the tail itself,
whose answers the probe pruned.  The extension tags each distinct variable
of GOAL, in first-occurrence order, as the tail, the current phase's rest,
or neither."
  (let* ((index (proof-state-environment-index state))
         (probe *phrase-probe*)
         (phase *phrase-probe-phase*)
         (tail (%walk-term-indexed (%phrase-probe-tail probe) index))
         (rest (and phase (%walk-term-indexed (%phrase-probe-phase-rest phase) index)))
         (seen (make-hash-table :test #'eq))
         (on-path (make-hash-table :test #'eq))
         (tags '()))
    (labels ((walk (term)
               (let ((term (%walk-term-indexed term index)))
                 (cond
                   ((logic-var-p term)
                    (unless (gethash term seen)
                      (setf (gethash term seen) t)
                      (push (cond ((eq term tail) :tail)
                                  ((eq term rest) phase))
                            tags)))
                   ((and (consp term) (not (gethash term on-path)))
                    (setf (gethash term on-path) t)
                    (walk (car term))
                    (walk (cdr term))
                    (remhash term on-path))))))
      (walk goal))
    (if (some #'identity tags)
        (append key (list (nreverse tags)))
        key)))

(defun %phrase-probe-check (index continue)
  "Check the branch after a unification with environment INDEX; call
CONTINUE, a thunk, to let it go on."
  (let* ((probe *phrase-probe*)
         (tail (%walk-term-indexed (%phrase-probe-tail probe) index)))
    (ecase (%phrase-probe-mode probe)
      (:verify
       (if (logic-var-p tail)
           (funcall continue)
           (cl:throw (%phrase-probe-accept-tag probe) t)))
      (:enumerate
       (let ((phase *phrase-probe-phase*)
             (scope *phrase-probe-scope*))
         (cond
           ((and scope
                 (some (lambda (variable)
                         (not (logic-var-p (%walk-term-indexed variable index))))
                       *phrase-probe-scope-taint*))
            (cl:throw scope :taint))
           ((null phase)
            (cond
              ((logic-var-p tail) (funcall continue))
              ;; End of input: phrase/2 on the prefix decides END-P.
              ((null tail) nil)
              (scope (cl:throw scope :taint))
              ((consp tail)
               (if (logic-var-p (%walk-term-indexed (cdr tail) index))
                   (let ((*phrase-probe-phase*
                           (%make-phrase-probe-phase (car tail) (cdr tail) index)))
                     (funcall continue))
                   ;; The branch read the last token at once ([H] against a
                   ;; remainder of []): nothing follows to confirm it.
                   (%phrase-probe-report-term
                    (%logic-substitute-indexed (car tail) index))))))
           ((logic-var-p (%walk-term-indexed (%phrase-probe-phase-rest phase) index))
            (funcall continue))
           (scope (cl:throw scope :taint))
           (t (%phrase-probe-end-phase phase index))))))))

(defun %phrase-probe-continue (previous environment emit)
  (if previous
      (funcall previous environment emit)
      (funcall emit environment)))

(defun %make-phrase-probe-hook (previous)
  "Return the post-unification hook of a probe, chaining to PREVIOUS."
  (lambda (environment emit)
    (if (null *phrase-probe*)
        (%phrase-probe-continue previous environment emit)
        (%phrase-probe-continue
         previous environment
         (lambda (extended)
           (%phrase-probe-check (%make-environment-index extended)
                                (lambda () (funcall emit extended))))))))

(defun %phrase-probe-check-state (state succeed)
  (%phrase-probe-check (proof-state-environment-index state)
                       (lambda () (funcall succeed state))))

(defun %phrase-probe-goal-name (goal)
  (let ((head (if (consp goal) (first goal) goal)))
    (and (symbolp head) (symbol-name head))))

(defun %phrase-probe-relax/k (goal state succeed)
  "Run the over-approximating form of transparent control GOAL."
  (let ((*phrase-probe-relaxed* t)
        (name (%phrase-probe-goal-name goal)))
    (flet ((prove (relaxed) (%prove-goal/k relaxed state succeed)))
      (cond
        ((member name '("NOT" "FORALL") :test #'string=) (funcall succeed state))
        ((string= name "ONCE") (prove (second goal)))
        ((string= name "IGNORE") (prove `(or ,(second goal) true)))
        ((member name '("->" "*->") :test #'string=)
         (prove `(and ,(second goal) ,(third goal))))
        ((member name '("IF-THEN-ELSE" "SOFT-IF-THEN-ELSE") :test #'string=)
         (prove `(or (and ,(second goal) ,(third goal)) ,(fourth goal))))
        (t (%phrase-probe-stop (proof-state-environment-index state)))))))

(defun %phrase-probe-scope/k (goal explicit-module solver state succeed)
  "Run transparent control GOAL in a scope; relax it if the scope touches
the taint and no enclosing scope is left to do so."
  (let* ((tag (list '%phrase-probe-scope))
         (outer *phrase-probe-scope*)
         (outer-taint *phrase-probe-scope-taint*)
         (result
           (cl:catch tag
             (let ((*phrase-probe-scope* tag)
                   (*phrase-probe-scope-taint*
                     (%phrase-probe-taint (proof-state-environment-index state))))
               (%prove-resolved-goal/k
                goal explicit-module solver state
                (lambda (next-state)
                  (let ((*phrase-probe-scope* outer)
                        (*phrase-probe-scope-taint* outer-taint))
                    (funcall succeed next-state)))))
             :done)))
    (when (eq result :taint)
      ;; Relaxing an inner scope can flip the polarity of an outer one
      ;; (\+ \+ G), so only the outermost scope relaxes.
      (if outer
          (cl:throw outer :taint)
          (%phrase-probe-relax/k goal state succeed)))))

(defun %phrase-probe-dispatch/k (goal explicit-module solver state succeed)
  "Prove GOAL, resolved by %RESOLVE-DISPATCH-TARGET, under the probe."
  (let ((probe *phrase-probe*)
        (phase *phrase-probe-phase*)
        (index (proof-state-environment-index state)))
    (when (and phase (eq (%phrase-probe-mode probe) :enumerate)
               (> (incf (%phrase-probe-phase-dispatches phase))
                  *phrase-probe-phase-budget*))
      (return-from %phrase-probe-dispatch/k
        (%phrase-probe-end-phase phase (%phrase-probe-phase-index phase))))
    (let ((name (%phrase-probe-goal-name goal))
          (enumerate-p (eq (%phrase-probe-mode probe) :enumerate)))
      (cond
        ((and (eq (first goal) '!) (null (rest goal)))
         ;; After the read, or in a relaxed construct, a cut would commit to
         ;; choices made for one token, or for no real input at all.
         (if (and enumerate-p (or phase *phrase-probe-relaxed*))
             (funcall succeed state)
             (%prove-resolved-goal/k goal explicit-module solver state succeed)))
        ((null solver) (%prove-resolved-goal/k goal explicit-module solver state succeed))
        ((member name +phrase-probe-monotonic-builtins+ :test #'string=)
         (%prove-resolved-goal/k goal explicit-module solver state
                                 (lambda (next-state)
                                   (%phrase-probe-check-state next-state succeed))))
        ((member name +phrase-probe-transparent-control+ :test #'string=)
         (if enumerate-p
             (%phrase-probe-scope/k goal explicit-module solver state succeed)
             (%prove-resolved-goal/k goal explicit-module solver state succeed)))
        ((%phrase-probe-shares-taint-p goal index)
         (cond
           ((not enumerate-p) (cl:throw (%phrase-probe-accept-tag probe) t))
           (*phrase-probe-scope* (cl:throw *phrase-probe-scope* :taint))
           (t (%phrase-probe-stop index))))
        (t (%prove-resolved-goal/k goal explicit-module solver state succeed))))))

(defun %phrase-probe-run (probe base-hook rulebase goal max-depth on-solution)
  "Prove GOAL under PROBE with a private table session."
  (%with-prolog-top-level-call (rulebase)
    (let ((*unification-scratch* (%make-unification-scratch))
          (*constraint-post-unify-hook* (%make-phrase-probe-hook base-hook))
          (*phrase-probe* probe)
          (*phrase-probe-phase* nil)
          (*phrase-probe-relaxed* nil)
          (*phrase-probe-scope* nil)
          (*phrase-probe-scope-taint* nil)
          (cut-tag (%make-cut-tag)))
      (%with-logic-variable-order
        (%collect-variables goal)
        (cl:catch cut-tag
          (%prove-goals/k
           (%normalize-query (list goal))
           (%make-proof-state
            rulebase '() (%make-environment-index '()) max-depth
            +default-prolog-module+
            ;; The shared session outlives the query, and tables built
            ;; under a probe hold only the answers that did not read the
            ;; tail.
            (%make-table-session (%make-term-hash-table)
                                 (make-hash-table :test #'equal)
                                 (make-hash-table :test #'equal)
                                 (%make-term-hash-table))
            cut-tag)
           on-solution))))))

(defun %phrase-probe-verify (base-hook rulebase nonterminal prefix token max-depth)
  "True when some derivation of PREFIX ++ [TOKEN|R] survives until it reads
or tests R, or succeeds."
  (let* ((rest (fresh-logic-variable "?PROBE-REST"))
         (tag (list '%phrase-probe-accept))
         (probe (%make-phrase-probe :verify rest nil tag)))
    (eq t (cl:catch tag
            (%phrase-probe-run
             probe base-hook rulebase
             (%dcg-call-with-streams nonterminal (append prefix (list* token rest)) '())
             max-depth
             (lambda (state)
               (declare (cl:ignore state))
               (cl:throw tag t)))
            nil))))

(defun %phrase-probe-enumerate (report base-hook rulebase nonterminal prefix max-depth)
  (let* ((tail (fresh-logic-variable "?PROBE-TAIL"))
         (probe (%make-phrase-probe :enumerate tail report nil)))
    (%phrase-probe-run
     probe base-hook rulebase
     (%dcg-call-with-streams nonterminal (append prefix tail) '())
     max-depth
     (lambda (state)
       (let ((index (proof-state-environment-index state))
             (phase *phrase-probe-phase*))
         (if phase
             (%phrase-probe-end-phase phase index)
             (let ((resolved (%walk-term-indexed tail index)))
               (cond
                 ((logic-var-p resolved) (funcall report :any nil))
                 ((consp resolved)
                  (%phrase-probe-report-term
                   (%logic-substitute-indexed (car resolved) index)))))))
       nil))))

(defun %make-phrase-probe-reporter (function verify)
  "Return a reporter calling FUNCTION once per distinct (KIND VALUE).  A
ground terminal is passed on only when VERIFY, called with it, is true."
  (let ((terminals (%make-term-hash-table))
        (any-p nil))
    (lambda (kind value)
      (ecase kind
        (:any (unless any-p
                (setf any-p t)
                (funcall function :any nil)))
        (:terminal
         (multiple-value-bind (canonical cyclic-p variables-p)
             (%canonicalize-variant value)
           (let ((key (if cyclic-p (%variant-graph-key canonical) canonical)))
             (unless (gethash key terminals)
               (setf (gethash key terminals) t)
               (cond
                 (variables-p
                  (funcall function :terminal
                           (%instantiate-variant canonical cyclic-p)))
                 ((funcall verify value)
                  (funcall function :terminal value)))))))))))

(defun map-phrase-next-terminals (function rulebase nonterminal prefix
                                  &key (max-depth *max-prolog-depth*)
                                    (max-inferences *max-prolog-inferences*
                                     max-inferences-p)
                                    (deadline *prolog-deadline* deadline-p))
  "Call FUNCTION with (KIND VALUE) for what NONTERMINAL accepts after PREFIX.

KIND is :TERMINAL with the terminal as VALUE, :ANY when the candidates
cannot be enumerated, or :END when PREFIX is itself a complete phrase in the
sense of phrase/2.  Each distinct item is reported once, in the order found,
and before any limit condition is signalled.  MAX-DEPTH, MAX-INFERENCES and
DEADLINE behave as the query options of the same names and bound all stages
together.  Returns NIL."
  (unless (%proper-list-p prefix)
    (error 'type-error :datum prefix :expected-type 'list))
  (let* ((max-depth (%validate-max-depth max-depth))
         (base-hook *constraint-post-unify-hook*)
         (escape (list '%phrase-probe-callback))
         (callback-condition
           (cl:catch escape
             (%with-inference-limits
                 ((if max-inferences-p
                      (%validate-max-inferences max-inferences)
                      :default)
                  (if deadline-p (%validate-deadline deadline) :default))
               (flet ((deliver (kind value)
                        ;; FUNCTION runs inside the probed derivation; a
                        ;; condition it does not handle is re-signalled from
                        ;; here, so no catch/3 in the grammar intercepts it.
                        (handler-case (funcall function kind value)
                          (serious-condition (condition)
                            (cl:throw escape condition)))))
                 (%phrase-probe-enumerate
                  (%make-phrase-probe-reporter
                   #'deliver
                   (lambda (token)
                     (%phrase-probe-verify base-hook rulebase nonterminal prefix
                                           token max-depth)))
                  base-hook rulebase nonterminal prefix max-depth)
                 ;; Last: phrase/2 on the prefix need not terminate, and the
                 ;; candidates found so far are delivered before a limit.
                 (when (nth-value 1 (query-prolog-first
                                     rulebase
                                     (%dcg-call-with-streams nonterminal prefix '())
                                     :max-depth max-depth))
                   (deliver :end nil))))
             nil)))
    (when callback-condition
      (error callback-condition)))
  nil)

(defun phrase-token-valid-p (rulebase nonterminal prefix token
                             &key (max-depth *max-prolog-depth*)
                               (max-inferences *max-prolog-inferences*
                                max-inferences-p)
                               (deadline *prolog-deadline* deadline-p))
  "True when some derivation of NONTERMINAL on PREFIX followed by the ground
TOKEN survives until it asks for the token after TOKEN, or succeeds.

False means no input that starts with PREFIX and TOKEN is a phrase, so TOKEN
can be marked invalid.  True does not promise that one is.  The options
behave as for MAP-PHRASE-NEXT-TERMINALS."
  (unless (%proper-list-p prefix)
    (error 'type-error :datum prefix :expected-type 'list))
  (let ((max-depth (%validate-max-depth max-depth))
        (base-hook *constraint-post-unify-hook*))
    (%with-inference-limits
        ((if max-inferences-p
             (%validate-max-inferences max-inferences)
             :default)
         (if deadline-p (%validate-deadline deadline) :default))
      (%phrase-probe-verify base-hook rulebase nonterminal prefix token max-depth))))

(defun phrase-next-terminals (rulebase nonterminal prefix &rest options
                              &key max-depth max-inferences deadline)
  "Return what NONTERMINAL accepts after the token list PREFIX.

Returns (VALUES TERMINALS ANY-P END-P): the distinct candidate terminals in
the order found, whether the candidates could not be enumerated, and whether
PREFIX is itself a complete phrase.  See MAP-PHRASE-NEXT-TERMINALS for the
options; a limit signals its condition."
  (declare (cl:ignore max-depth max-inferences deadline))
  (let ((terminals '())
        (any-p nil)
        (end-p nil))
    (apply #'map-phrase-next-terminals
           (lambda (kind value)
             (ecase kind
               (:terminal (push value terminals))
               (:any (setf any-p t))
               (:end (setf end-p t))))
           rulebase nonterminal prefix options)
    (values (nreverse terminals) any-p end-p)))
