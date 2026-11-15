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

(defparameter *inference-limit-cleanup-allowance* 10000
  "Fresh inference budget a cleanup goal gets when it runs because a query
exceeded its inference or deadline limit.  Without it the cleanup would trip
the exhausted limit at its first dispatch; bounding it keeps a looping
cleanup from escaping the limit.")

(declaim (type fixnum *inference-countdown*))
(defvar *inference-countdown* most-positive-fixnum
  "Goal dispatches left before the active inference limits are consulted.")

(defstruct (%inference-limits
            (:constructor %make-inference-limits (limit reserve deadline)))
  "Limits of one top-level query, shared by the queries nested inside it."
  (limit nil :type (or null (integer 0)) :read-only t)
  (reserve nil :type (or null (integer 0)))
  (deadline nil :type (or null integer) :read-only t)
  (tripped-p nil))

(defvar *inference-limits* nil
  "The %INFERENCE-LIMITS of the running query, or NIL when it has none.")

(defun %trip-inference-limit (limits condition-type resource message initargs)
  "Mark LIMITS exceeded and raise CONDITION-TYPE.  The countdown stays
negative, so a caller that swallows the condition trips it again at the next
dispatch."
  (setf (%inference-limits-tripped-p limits) t)
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
          (let ((chunk (%inference-chunk reserve deadline)))
            (when reserve
              (setf (%inference-limits-reserve limits) (- reserve chunk)))
            ;; This dispatch consumes the first inference of CHUNK.
            (setf *inference-countdown* (1- chunk)))))))

(defun %inference-chunk (reserve deadline)
  "Return how many dispatches may run before the limits are consulted again."
  (let ((chunk (if deadline
                   +inference-deadline-poll-interval+
                   most-positive-fixnum)))
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
*PROLOG-DEADLINE* for :DEFAULT."
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
          (if (and (null limit) (null deadline))
              (let ((*inference-limits* nil)
                    (*inference-countdown* most-positive-fixnum))
                (funcall function))
              (let* ((countdown (%inference-chunk limit deadline))
                     (limits (%make-inference-limits
                              limit (and limit (- limit countdown)) deadline))
                     (*inference-limits* limits)
                     (*inference-countdown* countdown))
                (%check-inference-deadline limits)
                (funcall function)))))))

(defmacro %with-inference-limits ((max-inferences deadline) &body body)
  "Run BODY as one top-level query; see %CALL-WITH-INFERENCE-LIMITS."
  `(%call-with-inference-limits ,max-inferences ,deadline
                                (lambda () ,@body)))

(defmacro %with-cleanup-inference-allowance (&body body)
  "Run BODY, a cleanup goal, under *INFERENCE-LIMIT-CLEANUP-ALLOWANCE* when
the running query has exceeded a limit; otherwise under the query's limits."
  (let ((function (gensym "CLEANUP")))
    `(flet ((,function () ,@body))
       (if (and *inference-limits*
                (%inference-limits-tripped-p *inference-limits*))
           (%call-with-inference-limits
            *inference-limit-cleanup-allowance* nil #',function)
           (,function)))))

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
