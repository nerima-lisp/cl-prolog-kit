;;;; Proof-search state: the dynamic variables scoping a proof, the
;;;; immutable PROOF-STATE record carried through the continuation, and the
;;;; transitions that derive one state from another.  The search algorithm
;;;; that drives these transitions lives in prover.lisp.

(in-package #:cl-prolog-kit)

(defvar *current-prolog-module* +default-prolog-module+)
(defvar *current-table-session* nil
  "Table session inherited by proof searches nested through builtins.")
(defvar *tabled-search-active-p* nil
  "True while the current proof has entered a tabled or left-recursive call.")
(defvar *call-depth-limit-token* nil)
(defvar *call-depth-limit-remaining* nil)
(defvar *call-depth-limit-used* 0)
(defvar *depth-limited-search-p* nil)
(defvar *constraint-post-unify-hook* nil
  "Function called after builtin =/2 extends an environment.")
(defvar *constraints-active-p-hook* nil
  "Function reporting whether a dynamically scoped constraint store is active.")

(defvar *caller-cut-tag* nil
  "Cut barrier of the goal invocation currently dispatching a builtin solver.

Cut-transparent control constructs (AND, OR, the THEN/ELSE branches of
IF-THEN-ELSE) read this at solver entry so a cut inside them prunes the
caller's clause alternatives, as ISO requires.")

(defun %make-cut-tag ()
  "Return a fresh CATCH tag identifying one cut barrier."
  (list '%cut-barrier))

(defstruct (proof-state
            (:constructor %make-proof-state
                (rulebase bindings environment-index remaining-depth module
                 table-session cut-tag)))
  "Immutable data carried through the proof-search continuation."
  (rulebase (make-rulebase) :type rulebase :read-only t)
  (bindings (quote ()) :type list :read-only t)
  (environment-index (%make-environment-index (quote ()))
                     :type %environment-index
                     :read-only t)
  (module +default-prolog-module+ :type symbol :read-only t)
  (table-session nil :type (or null %table-session) :read-only t)
  (cut-tag nil :type list :read-only t)
  (remaining-depth *max-prolog-depth*
                   :type (or null (integer 0 *))
                   :read-only t))

(defun %state-with (state &key (bindings nil bindings-p)
                                (environment-index nil environment-index-p)
                                (module nil module-p)
                                (table-session nil table-session-p)
                                (cut-tag nil cut-tag-p)
                                (remaining-depth nil remaining-depth-p))
  "Return STATE with supplied slots replaced while keeping bindings and their
index synchronized. Supplying only an unchanged CUT-TAG returns STATE."
  (if (and cut-tag-p (eq (proof-state-cut-tag state) cut-tag)
           (not bindings-p) (not environment-index-p) (not module-p)
           (not table-session-p) (not remaining-depth-p))
      state
      (let* ((old-bindings (proof-state-bindings state))
             (old-index (proof-state-environment-index state))
             (next-bindings (if bindings-p bindings old-bindings))
             (next-index
               (cond
                 (environment-index-p environment-index)
                 (bindings-p
                  (%environment-index-after-bindings
                   next-bindings old-bindings old-index))
                 (t old-index))))
        (%make-proof-state
         (proof-state-rulebase state)
         next-bindings
         next-index
         (if remaining-depth-p
             remaining-depth
             (proof-state-remaining-depth state))
         (if module-p module (proof-state-module state))
         (if table-session-p
             table-session
             (proof-state-table-session state))
         (if cut-tag-p cut-tag (proof-state-cut-tag state))))))

(defun %state-descending-into-rule
    (state bindings environment-index goal cut-tag)
  "Return the state for proving a matched rule body."
  (let ((remaining (proof-state-remaining-depth state)))
    (when (eql remaining 0)
      (%raise-resource-error "DEPTH_LIMIT"
                             (proof-state-bindings state)
                             (%iso-atom "CALL")
                             "explicit rule-resolution depth limit exceeded"
                             :condition-type (quote prolog-depth-limit-exceeded)
                             :goal goal))
    (%state-with state
                 :bindings bindings
                 :environment-index environment-index
                 :remaining-depth (and remaining (1- remaining))
                 :cut-tag cut-tag)))
