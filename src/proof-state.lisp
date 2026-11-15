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
;;; Inference and deadline limits.
;;;
;;; Every goal dispatch decrements *INFERENCE-COUNTDOWN*; only when it goes
;;; negative does %INFERENCE-COUNTDOWN-EXPIRED consult the limits.  The
;;; countdown covers at most +INFERENCE-DEADLINE-POLL-INTERVAL+ dispatches
;;; while a deadline is active, and the whole budget otherwise; the rest of
;;; the budget waits in the state's RESERVE.  Without limits the countdown
;;; starts at MOST-POSITIVE-FIXNUM, so the hot path is one decrement and one
;;; sign test either way.

(defconstant +inference-deadline-poll-interval+ 256
  "Most goal dispatches between two deadline checks.")

(defparameter *inference-limit-cleanup-allowance* 1000
  "Inferences shared by every cleanup goal that runs after a query exceeded
its inference or deadline limit.  Without it a cleanup would trip the
exhausted limit at its first dispatch.  The pool is minted once per limits
object, so neither repeated nor nested cleanups can extend it, and it stays
below the recursion depth that exhausts the default control stack.")

(defparameter *inference-limit-cleanup-grace*
  (ceiling internal-time-units-per-second 2)
  "GET-INTERNAL-REAL-TIME units (500 ms) the cleanup pool may run past the
moment its query stopped: past the query's deadline when that tripped,
otherwise past the moment the pool was first entered.  Long enough that a GC
pause between the trip and the cleanup does not cancel a cheap cleanup.")

(declaim (type fixnum *inference-countdown*))
(defvar *inference-countdown* most-positive-fixnum
  "Goal dispatches left before the active inference limits are consulted.")

(defstruct (%inference-limits
            (:constructor %make-inference-limits
                (limit reserve deadline enclosing
                 &aux (cleanup-p (and enclosing
                                      (%inference-limits-cleanup-p
                                       enclosing)))))
            (:constructor %make-inference-cleanup-pool
                (limit deadline enclosing
                 &aux (reserve limit) (cleanup-pool-p t) (cleanup-p t))))
  "Limits of one top-level query, shared by the queries nested inside it."
  (limit nil :type (or null (integer 0)) :read-only t)
  (reserve nil :type (or null (integer 0)))
  (deadline nil :type (or null integer) :read-only t)
  (tripped-p nil)
  ;; Only the root of a chain of nested limits holds the cleanup pool, so
  ;; the pool does not grow with nesting depth.
  (cleanup-pool nil :type (or null %inference-limits))
  (cleanup-pool-p nil :read-only t)
  ;; The limits this query was started under: another query's limits or
  ;; the cleanup pool it runs in.  A cleanup pool points at the root that
  ;; owns it.  NIL for the root.
  (enclosing nil :type (or null %inference-limits) :read-only t)
  ;; True for the cleanup pool and for queries started inside a cleanup that
  ;; runs on it: they poll their deadline at every inference.
  (cleanup-p nil :read-only t))

(defun %inference-limits-root (limits)
  "Return the outermost limits of the chain LIMITS belongs to."
  (loop for enclosing = (%inference-limits-enclosing limits)
        while enclosing
        do (setf limits enclosing))
  limits)

(defvar *inference-limits* nil
  "The %INFERENCE-LIMITS of the running query, or NIL when it has none.")

(defun %trip-inference-limit (limits condition-type resource message initargs)
  "Mark LIMITS and every limits enclosing it exceeded, then raise
CONDITION-TYPE.  The condition unwinds through the enclosing queries too, so
their cleanups need the cleanup pool as much as this query's do.  The
countdown stays negative, so a caller that swallows the condition trips it
again at the next dispatch."
  ;; If host code between two queries handles the condition and lets the
  ;; enclosing query go on, that query's later cleanups use the pool too;
  ;; the pool is still minted once per chain, so this cannot add inferences
  ;; beyond one allowance.
  (loop for current = limits then (%inference-limits-enclosing current)
        while current
        do (setf (%inference-limits-tripped-p current) t))
  (%raise-resource-error resource nil (%iso-atom "CALL") message
                         :condition-type condition-type
                         :initargs initargs))

(defun %check-inference-deadline (limits)
  "Raise PROLOG-DEADLINE-EXCEEDED when LIMITS' deadline has passed."
  (let ((deadline (%inference-limits-deadline limits)))
    (when (and deadline (>= (get-internal-real-time) deadline))
      (%trip-inference-limit limits 'prolog-deadline-exceeded "DEADLINE"
                             "query deadline passed"
                             (list :deadline deadline)))))

(defun %inference-countdown-expired ()
  "Charge the current dispatch against the reserve, or raise when a limit is
exhausted.  Called only when *INFERENCE-COUNTDOWN* went negative."
  (let ((limits *inference-limits*))
    (if (null limits)
        (setf *inference-countdown* most-positive-fixnum)
        (let ((reserve (%inference-limits-reserve limits))
              (deadline (%inference-limits-deadline limits)))
          (when (eql reserve 0)
            (%trip-inference-limit
             limits 'prolog-inference-limit-exceeded "INFERENCES"
             "inference limit exceeded"
             (list :limit (%inference-limits-limit limits))))
          (%check-inference-deadline limits)
          (let ((chunk (%inference-chunk
                        reserve deadline
                        (%inference-limits-cleanup-p limits))))
            (when reserve
              (setf (%inference-limits-reserve limits) (- reserve chunk)))
            ;; This dispatch consumes the first inference of CHUNK.
            (setf *inference-countdown* (1- chunk)))))))

(defun %inference-chunk (reserve deadline &optional cleanup-pool-p)
  "Return how many dispatches may run before the limits are consulted again.
A cleanup pool polls its deadline at every inference: its grace period is
short, and the cleanup runs only after the query has already stopped."
  (let ((chunk (cond ((null deadline) most-positive-fixnum)
                     (cleanup-pool-p 1)
                     (t +inference-deadline-poll-interval+))))
    (if reserve (min reserve chunk) chunk)))

(defmacro %count-inference ()
  "Charge one inference (a goal dispatch or a builtin solution) against the
running query's limits."
  `(when (minusp (decf *inference-countdown*))
     (%inference-countdown-expired)))

(defun %validate-max-inferences (value)
  "Return VALUE when it is NIL or a non-negative integer, else signal TYPE-ERROR."
  (unless (typep value '(or null (integer 0)))
    (error 'type-error :datum value :expected-type '(or null (integer 0))))
  value)

(defun %validate-deadline (value)
  "Return VALUE when it is NIL or an integer, else signal TYPE-ERROR."
  (unless (typep value '(or null integer))
    (error 'type-error :datum value :expected-type '(or null integer)))
  value)

(defun %call-with-inference-limits (max-inferences deadline function)
  "Call FUNCTION as one top-level query under MAX-INFERENCES and DEADLINE.

Each argument is the caller's explicit value, NIL, or :DEFAULT when the
caller passed none.  A query nested inside a running query that has limits
(a foreign predicate re-entering the engine) and passes neither limit shares
the enclosing query's remaining budget and deadline.  Otherwise the query gets
its own limits: the explicit values, or *MAX-PROLOG-INFERENCES* and
*PROLOG-DEADLINE* for :DEFAULT, except that its deadline is never later
than the enclosing query's.  Inside a cleanup running on the cleanup pool,
its budget is also capped by what the pool has left, and the inferences it
uses are charged to the pool; see %CALL-CHARGED-TO-CLEANUP-POOL."
  (let ((enclosing *inference-limits*))
    (if (and enclosing
             (member max-inferences '(nil :default))
             (member deadline '(nil :default)))
        (progn
          (%check-inference-deadline enclosing)
          (funcall function))
        (let* ((limit (%validate-max-inferences
                       (if (eq max-inferences :default)
                           *max-prolog-inferences*
                           max-inferences)))
               (deadline (%validate-deadline
                          (if (eq deadline :default)
                              *prolog-deadline*
                              deadline))))
          ;; A nested query with limits of its own still stops at the
          ;; enclosing deadline.
          (when (and enclosing (%inference-limits-deadline enclosing))
            (setf deadline
                  (if deadline
                      (min deadline (%inference-limits-deadline enclosing))
                      (%inference-limits-deadline enclosing))))
          (cond
            ((and enclosing (%inference-limits-cleanup-pool-p enclosing))
             (%call-charged-to-cleanup-pool enclosing limit deadline function))
            ((and (null limit) (null deadline))
             (let ((*inference-limits* nil)
                   (*inference-countdown* most-positive-fixnum))
               (funcall function)))
            (t
             (%call-with-fresh-inference-limits
              limit deadline enclosing function)))))))

(defun %call-with-fresh-inference-limits (limit deadline enclosing function)
  "Call FUNCTION under new limits LIMIT and DEADLINE started inside
ENCLOSING (or NIL)."
  (let* ((countdown (%inference-chunk limit deadline))
         (limits (%make-inference-limits
                  limit (and limit (- limit countdown)) deadline enclosing))
         (*inference-limits* limits)
         (*inference-countdown* countdown))
    (%check-inference-deadline limits)
    (funcall function)))

(defun %call-charged-to-cleanup-pool (pool limit deadline function)
  "Call FUNCTION, a nested query with its own LIMIT started by a cleanup
running on POOL.  The query may use at most what POOL has left (less when
LIMIT is lower), and whatever it uses is taken from POOL, so a cleanup cannot
escape the pool by starting a query with a larger budget."
  (let* ((available (+ (%inference-limits-reserve pool)
                       (max 0 *inference-countdown*)))
         (granted (if limit (min limit available) available)))
    ;; Hand the pool's whole remainder to the nested query and take back
    ;; what it leaves unused.
    (setf (%inference-limits-reserve pool) (- available granted)
          *inference-countdown* 0)
    (let* ((countdown (%inference-chunk granted deadline t))
           (limits (%make-inference-limits
                    granted (- granted countdown) deadline pool))
           (*inference-limits* limits)
           (*inference-countdown* countdown))
      (unwind-protect
           (progn
             (%check-inference-deadline limits)
             (funcall function))
        (incf (%inference-limits-reserve pool)
              (+ (%inference-limits-reserve limits)
                 (max 0 *inference-countdown*)))))))

(defmacro %with-inference-limits ((max-inferences deadline) &body body)
  "Run BODY as one top-level query; see %CALL-WITH-INFERENCE-LIMITS."
  `(%call-with-inference-limits ,max-inferences ,deadline
                                (lambda () ,@body)))

(defun %inference-cleanup-pool (limits)
  "Return the cleanup pool of the chain LIMITS belongs to, minting it on
first use.  Its deadline counts from LIMITS, the limits that stopped."
  (let ((root (%inference-limits-root limits)))
    (or (%inference-limits-cleanup-pool root)
        (let* ((now (get-internal-real-time))
               (deadline (%inference-limits-deadline limits))
               (allowance *inference-limit-cleanup-allowance*))
          (setf (%inference-limits-cleanup-pool root)
                (%make-inference-cleanup-pool
                 allowance
                 (+ (if deadline (min deadline now) now)
                    *inference-limit-cleanup-grace*)
                 root))))))

(defun %call-with-cleanup-inference-allowance (function)
  "Call FUNCTION, a cleanup goal.  After the running query, or a query
nested inside it, tripped a limit, the cleanup draws on the single cleanup
pool of the outermost limits; a cleanup already running on the pool keeps
drawing on it.  Inferences it leaves unused return to the pool."
  (let ((limits *inference-limits*))
    (if (or (null limits)
            (%inference-limits-cleanup-pool-p limits)
            (not (%inference-limits-tripped-p limits)))
        (funcall function)
        (let* ((pool (%inference-cleanup-pool limits))
               (reserve (%inference-limits-reserve pool))
               (countdown (%inference-chunk
                           reserve (%inference-limits-deadline pool) t)))
          (setf (%inference-limits-reserve pool) (- reserve countdown))
          (let ((*inference-limits* pool)
                (*inference-countdown* countdown))
            (unwind-protect
                 (progn
                   (%check-inference-deadline pool)
                   (funcall function))
              (incf (%inference-limits-reserve pool)
                    (max 0 *inference-countdown*))))))))

(defmacro %with-cleanup-inference-allowance (&body body)
  "Run BODY, a cleanup goal; see %CALL-WITH-CLEANUP-INFERENCE-ALLOWANCE."
  `(%call-with-cleanup-inference-allowance (lambda () ,@body)))

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
