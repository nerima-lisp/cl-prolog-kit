;;;; PHRASE-NEXT-TERMINALS: the terminals a grammar accepts after a prefix.
;;;;
;;;; The grammar runs through the ordinary prover on PREFIX followed by an
;;;; unbound tail, so cut, if-then-else, call//N, tabling and constraint
;;;; propagation behave exactly as in phrase/2.  A probe installed as the
;;;; post-unification hook watches that tail:
;;;;
;;;; - While the tail is unbound the branch is seeking.  The first time it is
;;;;   bound to [H|Rest] the branch enters a phase: the read is recorded only
;;;;   once the branch survives until Rest is read, a solution is reached, a
;;;;   cut that would prune alternatives chosen before the read runs, a table
;;;;   begun before the read gets an answer, or an instantiation error is
;;;;   raised.  The branch then reports H (or :ANY while H is unbound) and
;;;;   fails, as a real token other than H would.  The phase lets a guard
;;;;   after the terminal reject it and lets a guard bind a variable terminal.
;;;; - A tail bound to [] reports :END and fails.

(in-package #:cl-prolog-kit)

(defstruct (%phrase-probe (:constructor %make-phrase-probe (tail report)))
  "The open tail one PHRASE-NEXT-TERMINALS call watches and its reporter."
  (tail nil :read-only t)
  (report nil :type function :read-only t))

(defstruct (%phrase-probe-phase
            (:constructor %make-phrase-probe-phase (head rest rest-open-p tag)))
  "One branch's read of the open tail as [HEAD|REST], awaiting confirmation."
  (head nil :read-only t)
  (rest nil :read-only t)
  ;; When REST is already bound at the read (phrase/2 passes [] as the
  ;; remainder, so a last terminal arrives as [H]), only the other events
  ;; can end the phase.
  (rest-open-p nil :read-only t)
  (tag nil :read-only t)
  (reported-p nil))

(defun %phrase-probe-report-term (term)
  "Report TERM, already substituted, as a terminal or :ANY when unbound."
  (funcall (%phrase-probe-report *phrase-probe*)
           (if (logic-var-p term) :any :terminal)
           term))

(defun %phrase-probe-end-phase (phase index)
  "Report PHASE's head under INDEX and fail the branch by returning."
  (setf (%phrase-probe-phase-reported-p phase) t)
  (%phrase-probe-report-term
   (%logic-substitute-indexed (%phrase-probe-phase-head phase) index))
  nil)

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

(defun %phrase-probe-continue (previous environment emit)
  (if previous
      (funcall previous environment emit)
      (funcall emit environment)))

(defun %phrase-probe-start-phase (previous head rest index environment emit)
  "Run the rest of the branch as a phase for the read [HEAD|REST]."
  (let* ((tag (list '%phrase-probe-phase))
         (phase (%make-phrase-probe-phase
                 head rest (logic-var-p (%walk-term-indexed rest index)) tag)))
    (let ((*phrase-probe-phase* phase))
      (cl:catch tag
        (handler-bind
            ((prolog-instantiation-error
               ;; A guard could not evaluate the unknown token; the read is
               ;; reported as it stood when the phase began.
               (lambda (condition)
                 (declare (cl:ignore condition))
                 (%phrase-probe-end-phase phase index)
                 (cl:throw tag nil))))
          (%phrase-probe-continue previous environment emit))))
    ;; A variable terminal whose guard failed while it was unbound (a type
    ;; test, a negation) is reported as :ANY rather than hidden.
    (when (and (not (%phrase-probe-phase-reported-p phase))
               (logic-var-p (%walk-term-indexed head index)))
      (%phrase-probe-report-term head))))

(defun %make-phrase-probe-hook (previous)
  "Return the post-unification hook of a probe, chaining to PREVIOUS."
  (lambda (environment emit)
    (let ((probe *phrase-probe*)
          (phase *phrase-probe-phase*))
      (if (null probe)
          (%phrase-probe-continue previous environment emit)
          (let ((index (%make-environment-index environment)))
            (if phase
                (if (and (%phrase-probe-phase-rest-open-p phase)
                         (not (logic-var-p
                               (%walk-term-indexed
                                (%phrase-probe-phase-rest phase) index))))
                    (%phrase-probe-end-phase phase index)
                    (%phrase-probe-continue previous environment emit))
                (let ((tail (%walk-term-indexed (%phrase-probe-tail probe) index)))
                  (cond
                    ((logic-var-p tail)
                     (%phrase-probe-continue previous environment emit))
                    ((null tail)
                     (funcall (%phrase-probe-report probe) :end nil))
                    ((consp tail)
                     (%phrase-probe-start-phase previous (car tail) (cdr tail)
                                                index environment emit))))))))))

(defun %phrase-probe-solution (state)
  "Handle a solution of the probed goal."
  (let ((index (proof-state-environment-index state))
        (phase *phrase-probe-phase*)
        (report (%phrase-probe-report *phrase-probe*)))
    (if phase
        (%phrase-probe-end-phase phase index)
        ;; Only a binding the hook did not see (one made by a builtin such
        ;; as append/3) reaches a solution without a phase.
        (let ((tail (%walk-term-indexed (%phrase-probe-tail *phrase-probe*) index)))
          (cond
            ((logic-var-p tail)
             (funcall report :end nil)
             (funcall report :any nil))
            ((null tail) (funcall report :end nil))
            ((consp tail)
             (%phrase-probe-report-term
              (%logic-substitute-indexed (car tail) index))))))))

(defun %make-phrase-probe-reporter (function)
  "Return a reporter calling FUNCTION once per distinct (KIND VALUE)."
  (let ((terminals (make-hash-table :test #'equal))
        (any-p nil)
        (end-p nil))
    (lambda (kind value)
      (ecase kind
        (:any (unless any-p
                (setf any-p t)
                (funcall function :any nil)))
        (:end (unless end-p
                (setf end-p t)
                (funcall function :end nil)))
        (:terminal
         (multiple-value-bind (canonical cyclic-p variables-p)
             (%canonicalize-variant value)
           (let ((key (if cyclic-p (%variant-graph-key canonical) canonical)))
             (unless (gethash key terminals)
               (setf (gethash key terminals) t)
               (funcall function :terminal
                        (if variables-p
                            (%instantiate-variant canonical cyclic-p)
                            value))))))))))

(defun %probe-next-terminals (function rulebase nonterminal prefix max-depth)
  (let* ((tail (fresh-logic-variable "?PROBE-TAIL"))
         (goal (%dcg-call-with-streams nonterminal (append prefix tail) '()))
         (probe (%make-phrase-probe tail (%make-phrase-probe-reporter function))))
    (%with-prolog-top-level-call (rulebase)
      (let ((*unification-scratch* (%make-unification-scratch))
            (*constraint-post-unify-hook*
              (%make-phrase-probe-hook *constraint-post-unify-hook*))
            (*phrase-probe* probe)
            (*phrase-probe-phase* nil)
            (cut-tag (%make-cut-tag)))
        (%with-logic-variable-order
          (%collect-variables goal)
          (cl:catch cut-tag
            (%prove-goals/k
             (%normalize-query (list goal))
             (%make-proof-state
              rulebase '() (%make-environment-index '()) max-depth
              +default-prolog-module+
              ;; A private session: the shared one outlives the query, and
              ;; tables built under the probe hold only the answers that did
              ;; not read the tail.
              (%make-table-session (make-hash-table :test #'equal)
                                   (make-hash-table :test #'equal)
                                   (make-hash-table :test #'equal)
                                   (make-hash-table :test #'equal))
              cut-tag)
             #'%phrase-probe-solution)))))))

(defun map-phrase-next-terminals (function rulebase nonterminal prefix
                                  &key (max-depth *max-prolog-depth*)
                                    (max-inferences *max-prolog-inferences*
                                     max-inferences-p)
                                    (deadline *prolog-deadline* deadline-p))
  "Call FUNCTION with (KIND VALUE) for what NONTERMINAL accepts after PREFIX.

KIND is :TERMINAL with the terminal as VALUE, :ANY when some derivation
accepts an arbitrary token, or :END when PREFIX is itself a complete phrase
in the sense of phrase/2.  Each distinct item is reported once, in the order
found, and before any limit condition is signalled.  MAX-DEPTH,
MAX-INFERENCES and DEADLINE behave as the query options of the same names.
Returns NIL."
  (unless (%proper-list-p prefix)
    (error 'type-error :datum prefix :expected-type 'list))
  (let ((max-depth (%validate-max-depth max-depth)))
    (%with-inference-limits
        ((if max-inferences-p
             (%validate-max-inferences max-inferences)
             :default)
         (if deadline-p (%validate-deadline deadline) :default))
      (%probe-next-terminals function rulebase nonterminal prefix max-depth)))
  nil)

(defun phrase-next-terminals (rulebase nonterminal prefix &rest options
                              &key max-depth max-inferences deadline)
  "Return what NONTERMINAL accepts after the token list PREFIX.

Returns (VALUES TERMINALS ANY-P END-P): the distinct terminals in the order
found, whether an arbitrary token is accepted, and whether PREFIX is itself
a complete phrase.  All three empty means PREFIX cannot start a phrase.  See
MAP-PHRASE-NEXT-TERMINALS for the options; a limit signals its condition."
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
