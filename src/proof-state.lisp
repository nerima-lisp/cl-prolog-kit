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
;;; The outermost query in a thread's dynamic extent (the root) binds one
;;; counter, *INFERENCE-USED*, and every query nested inside it charges that
;;; same counter.  The limits are dynamically bound bounds on it: the cap
;;; *INFERENCE-CAP*, an absolute value of *INFERENCE-USED*, and the deadline
;;; *INFERENCE-DEADLINE*.  A nested query can only lower them.  A cleanup
;;; goal raises them by one allowance and one grace period, but never past
;;; the ceilings of the innermost query that has a bound of its own: that
;;; query's cap plus *INFERENCE-LIMIT-CLEANUP-ALLOWANCE*, and its deadline
;;; plus *INFERENCE-LIMIT-CLEANUP-GRACE*.  So the root of limit L and
;;; deadline D runs at most L + allowance inferences, however its queries
;;; nest and whatever catches their stops.  It stops by D + grace plus the
;;; time of the charges since the last clock read; see
;;; %POLL-INFERENCE-DEADLINE.
;;;
;;; No stop is recorded anywhere.  A stop takes back the charge that caused
;;; it, so the next charge under the same bounds stops again, and host code
;;; that swallows a stop cannot continue past it.
;;;
;;; The hot path is one increment and one comparison against
;;; *INFERENCE-CHECK-AT*.  Every binding of the bounds also binds it, to the
;;; smaller of the cap and, while a deadline is in force, the root's
;;; *INFERENCE-POLL-AT*.  Each clock read at count U sets *INFERENCE-POLL-AT*
;;; to at most U + 255, so every value *INFERENCE-CHECK-AT* takes is at most
;;; its binding's cap and at most 255 past the latest clock read.  Leaving a
;;; nested query or a cleanup restores a value computed earlier for the
;;; restored bounds, which keeps both properties; it can only make the next
;;; check come sooner.  So no charge exceeds the cap in force unchecked, and
;;; while a deadline is in force at most 256 charges pass between two clock
;;; reads.  Without a deadline the clock is never read.

(defconstant +inference-deadline-max-poll-interval+ 256
  "Most charges between two clock reads while a deadline is in force.")

(defconstant +inference-deadline-poll-target+
  (ceiling internal-time-units-per-second 100)
  "GET-INTERNAL-REAL-TIME units (10 ms) that the time between two clock reads
should not exceed; see %POLL-INFERENCE-DEADLINE.")

(defparameter *inference-limit-cleanup-allowance* 1000
  "Inferences a cleanup goal may run past the cap it started under.
Nested cleanups do not add up past one allowance over the cap of the query
they run in, and the allowance stays below the recursion depth that
exhausts the default control stack.")

(defparameter *inference-limit-cleanup-grace*
  (ceiling internal-time-units-per-second 2)
  "GET-INTERNAL-REAL-TIME units (500 ms) a cleanup goal may run past the
deadline it started under, never past the deadline of the query it runs in
plus one grace period.  Long enough that a GC pause does not cancel a cheap
cleanup.")

(declaim (type fixnum *inference-used* *inference-check-at*
                      *inference-poll-at* *inference-poll-interval*))
(defvar *inference-used* 0
  "Inferences charged so far to the running root query.")

(defvar *inference-check-at* most-positive-fixnum
  "Value of *INFERENCE-USED* past which a charge consults the bounds.")

(defvar *inference-poll-at* 0
  "Value of *INFERENCE-USED* past which a charge reads the clock while a
deadline is in force.  Bound by the root and never decreased.")

(defvar *inference-poll-interval* 1
  "Charges from one clock read to the next; bound by the root.")

(defvar *inference-last-poll* nil
  "GET-INTERNAL-REAL-TIME of the root's last clock read, or NIL before it.")

(defvar *inference-query-active-p* nil
  "True inside a query, so that a query started there is nested.")

(defvar *inference-cap* nil
  "Highest *INFERENCE-USED* the running query may reach, or NIL for none.")

(defvar *inference-cap-limit* nil
  "The limit PROLOG-INFERENCE-LIMIT-EXCEEDED reports for *INFERENCE-CAP*.")

(defvar *inference-cap-ceiling* nil
  "Highest *INFERENCE-CAP* a cleanup inside the running query may set.")

(defvar *inference-deadline* nil
  "Effective deadline of the running query, or NIL for none.")

(defvar *inference-deadline-ceiling* nil
  "Latest *INFERENCE-DEADLINE* a cleanup inside the running query may set.")

(defun %min* (a b)
  "Return the smaller of A and B, either of which may be NIL for unbounded."
  (cond ((null a) b) ((null b) a) (t (min a b))))

(defun %inference-next-check ()
  "Return the *INFERENCE-USED* value past which a charge must consult the
current bounds."
  (min most-positive-fixnum
       (or (%min* *inference-cap* (and *inference-deadline* *inference-poll-at*))
           most-positive-fixnum)))

(defun %inference-deadline-passed-p ()
  (let ((deadline *inference-deadline*))
    (and deadline (>= (get-internal-real-time) deadline))))

(defun %raise-deadline-exceeded ()
  (%raise-resource-error "DEADLINE" nil (%iso-atom "CALL")
                         "query deadline passed"
                         :condition-type 'prolog-deadline-exceeded
                         :initargs (list :deadline *inference-deadline*)))

(defun %check-inference-deadline ()
  "Raise PROLOG-DEADLINE-EXCEEDED when the effective deadline has passed."
  (when (%inference-deadline-passed-p)
    (%raise-deadline-exceeded)))

(defun %poll-inference-deadline ()
  "Raise PROLOG-DEADLINE-EXCEEDED, taking back the current charge, when the
deadline has passed.  Otherwise adapt the poll interval to the time since
the previous clock read and schedule the next read.

The interval halves, down to 1, when that time exceeded
+INFERENCE-DEADLINE-POLL-TARGET+, and doubles, up to
+INFERENCE-DEADLINE-MAX-POLL-INTERVAL+, when it was under a tenth of it.
Cheap inferences keep the interval at its maximum, so they read the clock
no more often than a fixed interval would.  A run of builtin calls that
each take long shrinks it toward 1.  Either way the deadline is noticed
within one interval of charges after it passes."
  (let ((now (get-internal-real-time))
        (last *inference-last-poll*)
        (interval *inference-poll-interval*))
    (when (>= now *inference-deadline*)
      (decf *inference-used*)
      (%raise-deadline-exceeded))
    (when last
      (let ((elapsed (- now last)))
        (cond ((> elapsed +inference-deadline-poll-target+)
               (setf interval (max 1 (ash interval -1))))
              ((< (* 10 elapsed) +inference-deadline-poll-target+)
               (setf interval (min +inference-deadline-max-poll-interval+
                                   (* 2 interval)))))))
    (setf *inference-poll-interval* interval
          *inference-last-poll* now
          *inference-poll-at* (+ *inference-used* interval -1))))

(defun %restart-inference-deadline-polling ()
  "Read the clock, raising PROLOG-DEADLINE-EXCEEDED when the deadline has
passed, and poll again at the next charge with the interval back at 1.  A
cleanup starts this way: it often begins right after a stop, and it may run
builtin calls far slower than the inferences that set the interval."
  (when *inference-deadline*
    (let ((now (get-internal-real-time)))
      (when (>= now *inference-deadline*)
        (%raise-deadline-exceeded))
      (setf *inference-poll-interval* 1
            *inference-last-poll* now
            *inference-poll-at* *inference-used*))))

(defun %inference-check ()
  "Raise when the current charge exceeds the cap or the deadline has passed,
else schedule the next check.  Called only when *INFERENCE-USED* passed
*INFERENCE-CHECK-AT*.  A stopping charge is taken back: its inference does
not run, so the stop leaves *INFERENCE-USED* at most at the cap, and the
next charge under the same bounds stops again."
  (let ((cap *inference-cap*))
    (cond ((and cap (> *inference-used* cap))
           (decf *inference-used*)
           (%raise-resource-error "INFERENCES" nil (%iso-atom "CALL")
                                  "inference limit exceeded"
                                  :condition-type 'prolog-inference-limit-exceeded
                                  :initargs (list :limit *inference-cap-limit*)))
          (t
           (when (and *inference-deadline*
                      (> *inference-used* *inference-poll-at*))
             (%poll-inference-deadline))
           (setf *inference-check-at* (%inference-next-check))))))

(defmacro %count-inference ()
  "Charge one inference (a goal dispatch or a builtin solution) against the
running query's limits."
  `(when (> (incf *inference-used*) *inference-check-at*)
     (%inference-check)))

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

(defun %call-with-query-bounds (limit deadline function)
  "Call FUNCTION with the bounds lowered to LIMIT more inferences and to
DEADLINE, either of which may be NIL for no bound of its own.  A bound that
does not lower the enclosing one is ignored.  A bound that does also lowers
the ceiling to itself plus one cleanup allowance or grace period, so that
cleanups inside this query cannot raise it further; without a ceiling of
its own, a query nested in a root that has no limit would let every
recursive cleanup raise the cap again."
  (let* ((own-cap (and limit (+ *inference-used* limit)))
         (own-cap-p (and own-cap
                         (or (null *inference-cap*)
                             (<= own-cap *inference-cap*))))
         (*inference-cap* (if own-cap-p own-cap *inference-cap*))
         (*inference-cap-limit* (if own-cap-p limit *inference-cap-limit*))
         (*inference-cap-ceiling*
           (if own-cap-p
               (%min* *inference-cap-ceiling*
                      (+ own-cap *inference-limit-cleanup-allowance*))
               *inference-cap-ceiling*))
         (own-deadline-p (and deadline
                              (or (null *inference-deadline*)
                                  (< deadline *inference-deadline*))))
         (*inference-deadline* (if own-deadline-p deadline *inference-deadline*))
         (*inference-deadline-ceiling*
           (if own-deadline-p
               (%min* *inference-deadline-ceiling*
                      (+ deadline *inference-limit-cleanup-grace*))
               *inference-deadline-ceiling*))
         (*inference-check-at* (%inference-next-check)))
    (%check-inference-deadline)
    (funcall function)))

(defun %call-with-inference-limits (max-inferences deadline function)
  "Call FUNCTION as one top-level query under MAX-INFERENCES and DEADLINE.

Each argument is the caller's explicit value, NIL, or :DEFAULT when the
caller passed none.  A query nested inside a running query that has limits
(a foreign predicate re-entering the engine) and passes neither limit shares
the enclosing query's bounds.  Otherwise the explicit values, or
*MAX-PROLOG-INFERENCES* and *PROLOG-DEADLINE* for :DEFAULT, can only lower
the enclosing bounds: a nested query charges the root's counter and stops at
whichever of its own and the enclosing bounds comes first."
  (if (and (or *inference-cap* *inference-deadline*)
           (member max-inferences '(nil :default))
           (member deadline '(nil :default)))
      (progn
        (%check-inference-deadline)
        (funcall function))
      (let ((limit (%validate-max-inferences
                    (if (eq max-inferences :default)
                        *max-prolog-inferences*
                        max-inferences)))
            (deadline (%validate-deadline
                       (if (eq deadline :default)
                           *prolog-deadline*
                           deadline))))
        (if *inference-query-active-p*
            (%call-with-query-bounds limit deadline function)
            (let ((*inference-query-active-p* t)
                  (*inference-used* 0)
                  (*inference-poll-at* 0)
                  (*inference-poll-interval* 1)
                  (*inference-last-poll* nil))
              (%call-with-query-bounds limit deadline function))))))

(defmacro %with-inference-limits ((max-inferences deadline) &body body)
  "Run BODY as one top-level query; see %CALL-WITH-INFERENCE-LIMITS."
  `(%call-with-inference-limits ,max-inferences ,deadline
                                (lambda () ,@body)))

(defun %call-with-cleanup-inference-allowance (function)
  "Call FUNCTION, a cleanup goal, with the cap raised by one allowance and
the deadline by one grace period, each bounded by its ceiling.  The goal
gets them however the query left it: a stop leaves *INFERENCE-USED* at most
at the cap it hit, so the cleanup can still run, and the code after the
cleanup stops again at its next charge."
  (let* ((cap *inference-cap*)
         (deadline *inference-deadline*)
         (*inference-cap*
           (and cap (%min* (+ cap *inference-limit-cleanup-allowance*)
                           *inference-cap-ceiling*)))
         (*inference-cap-limit*
           (if cap *inference-limit-cleanup-allowance* *inference-cap-limit*))
         (*inference-deadline*
           (and deadline (%min* (+ deadline *inference-limit-cleanup-grace*)
                                *inference-deadline-ceiling*)))
         (*inference-check-at* *inference-check-at*))
    (%restart-inference-deadline-polling)
    (setf *inference-check-at* (%inference-next-check))
    (funcall function)))

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
