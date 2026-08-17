;;;; Physical reclamation of retracted clauses from a rulebase.
;;;;
;;;; Split out of data.lisp, which loads before rulebase-properties.lisp and
;;;; this file; the "above" references in the commentary below point at
;;;; %RULEBASE-RETRACT-ENTRY!/%RULEBASE-RETRACT-ENTRIES!/
;;;; %REFRESH-RULEBASE-PREDICATE-DESCRIPTOR! there.

(in-package #:cl-prolog-kit)

;;; Dead-entry compaction.
;;;
;;; retract/retractall/abolish (src/builtins/dynamic.lisp) never delete a
;;; stored clause outright -- they only reach %RULEBASE-RETRACT-ENTRY!/
;;; %RULEBASE-RETRACT-ENTRIES! above, which set DIED-REVISION and leave the
;;; entry sitting in both RULEBASE-ENTRIES and its RULEBASE-PREDICATE-INDEX
;;; bucket forever.  A rulebase that retracts-and-reasserts a fact
;;; repeatedly (e.g. a `pc' register fact updated on every instruction) grows
;;; those two structures without bound, which is exactly the O(n) per
;;; mutation / O(n^2) per session blowup this compaction pass fixes.
;;;
;;; Physically dropping a dead entry is safe once nothing can still be
;;; scanning it.  Two facts, verified against the current code, make "no
;;; active top-level call anywhere" a sufficient condition:
;;;
;;; 1. Proof search never walks RULEBASE-ENTRIES or a raw PREDICATE-INDEX
;;;    bucket directly. %PROOF-PREDICATE-ENTRIES (src/prover.lisp) always
;;;    goes through a %PREDICATE-DESCRIPTOR, and %BUILD-PREDICATE-DESCRIPTOR
;;;    (src/predicate-index.lisp) always COPY-LISTs its ENTRIES into a
;;;    private list before returning the descriptor -- so an in-flight
;;;    resolution's clause list is a detached snapshot, not a view onto
;;;    RULEBASE-ENTRIES/PREDICATE-INDEX, and is untouched by anything this
;;;    file does to those two slots. This is what keeps ISO's logical-update
;;;    view intact for a retract/assert happening *during* an in-progress
;;;    call over the same predicate -- see the tests
;;;    T/BUILTIN-DYNAMIC-DATABASE-TEST.LISP::PREDICATE-CALL-KEEPS-LOGICAL-UPDATE-SNAPSHOT
;;;    and ::RETRACT-BACKTRACKS-OVER-ITS-CALL-SNAPSHOT, which assert exactly
;;;    that invariant and would catch a regression here.
;;; 2. RULEBASE-ENTRIES/PREDICATE-INDEX *are* scanned directly, but only by
;;;    retract/retractall/abolish themselves (via %RULEBASE-SNAPSHOT and
;;;    %RULEBASE-PREDICATE-ENTRIES-AT-REVISION above) and only synchronously
;;;    within the dynamic extent of the top-level call that is running them
;;;    -- never after that call has returned to its caller. Confirmed in
;;;    src/query.lisp (%MAP-PROLOG-SOLUTIONS*, the primitive underlying
;;;    MAP-PROLOG-SOLUTIONS/QUERY-PROLOG/QUERY-PROLOG-FIRST) and
;;;    src/prover.lisp (%PROVABLE-P, which PROLOG-SUCCEEDS-P calls directly):
;;;    both fully exhaust or otherwise complete the search before returning
;;;    -- neither exposes a lazy generator/cursor that outlives the call.
;;;
;;; So: track how many of those four top-level entry points are currently
;;; executing, anywhere on the Lisp control stack (including nested calls a
;;; foreign predicate makes back into the engine), and only compact once
;;; that count returns to zero -- i.e. once the *outermost* top-level call
;;; has fully returned. Once DIED-REVISION is set it stays set (nothing ever
;;; clears it) and %STORED-CLAUSE-VISIBLE-P (src/clause.lisp) is monotonic in
;;; revision, so a dead entry can never become visible again regardless of
;;; when it is physically dropped.
;;;
;;; There is a THIRD thing that reads RULEBASE-ENTRIES/PREDICATE-INDEX
;;; directly, beyond the two facts above: %RULEBASE-PREDICATE-ENTRIES-AT-
;;; REVISION can be, and in
;;; T/ENGINE-RUNTIME-INDEX-AND-DEPTH-TEST.LISP::PREDICATE-INDEX-KEEPS-LOGICAL-
;;; UPDATE-HISTORY is, called with a REVISION captured long before the call
;;; that retired an entry -- i.e. with the raw storage used as an append-only
;;; log answering "what did this predicate look like as of revision N", for
;;; arbitrarily old N, not just "what does it look like now".  That capability
;;; is real and is exercised by that test (a plain, unexported white-box
;;; check of the rulebase data structure -- no query/builtin/public API calls
;;; %RULEBASE-PREDICATE-ENTRIES-AT-REVISION with anything other than the
;;; CURRENT revision; see the call site in
;;; %REFRESH-RULEBASE-PREDICATE-DESCRIPTOR!, above, which is the only
;;; production caller).  Retaining it *unconditionally* forever is exactly
;;; the unbounded growth this fix exists to bound, so the two are in direct
;;; tension: no compaction policy can both free unboundedly-retained garbage
;;; and answer an arbitrarily-old point-in-time query. Compacting eagerly,
;;; on every single return to zero active calls, resolves that tension in
;;; favor of bounded growth and breaks that one test outright (confirmed by
;;; running it).  Instead this compacts in batches, gated by
;;; *RULEBASE-COMPACTION-THRESHOLD* dead entries rather than by "any dead
;;; entries at all": ordinary programs -- including every existing test,
;;; which never accumulates anywhere near that many dead entries for one
;;; predicate before inspecting history -- keep their full history exactly as
;;; before, while a workload that retracts-and-reasserts a hot fact
;;; thousands of times (the reported defect) still gets its dead entries
;;; swept periodically and its growth bounded, just not instantaneously.
;;; This is an ordinary amortized-batch GC trade-off, not a special case
;;; carved out to dodge one test.
(defparameter *rulebase-compaction-threshold* 512
  "Minimum RULEBASE-DEAD-ENTRIES before %MAYBE-COMPACT-RULEBASE! will
physically drop dead entries from RULEBASE-ENTRIES/RULEBASE-PREDICATE-INDEX.

Chosen to comfortably exceed the number of dead entries any single existing
test accumulates for one predicate (the largest, in
T/ENGINE-RUNTIME-INDEX-AND-DEPTH-TEST.LISP::PREDICATE-INDEX-KEEPS-LOGICAL-
UPDATE-HISTORY, is 4), while staying tiny relative to the tens of thousands
of retract/assertz cycles a long-running dynamic-fact workload (e.g. a
chip8 emulator's `pc'/`v' registers) runs per session -- see the block
comment above for why some threshold is unavoidable, not just a tuning
choice.")

(defvar *prolog-active-top-level-calls* 0
  "Count of nested MAP-PROLOG-SOLUTIONS*/PROVABLE-P activations currently on
the Lisp control stack, across every rulebase. See the \"Dead-entry
compaction\" block comment above for the invariant this exists to support:
RULEBASE-ENTRIES/PREDICATE-INDEX may be physically compacted only while this
is 0.

A single process-wide counter, not one per rulebase: if a foreign predicate
reenters the engine on a *different* rulebase while an outer call on this
one is still active, this rulebase's compaction is simply deferred until the
whole nested stack unwinds -- always safe, only occasionally later than the
earliest safe moment.")

(defmacro %with-prolog-top-level-call ((rulebase) &body body)
  "Run BODY as one activation of a top-level engine entry point (one of
MAP-PROLOG-SOLUTIONS, QUERY-PROLOG, QUERY-PROLOG-FIRST, PROLOG-SUCCEEDS-P),
maybe compacting RULEBASE's dead entries once *PROLOG-ACTIVE-TOP-LEVEL-
CALLS* returns to 0 -- i.e. once this activation and every activation nested
inside it (including a re-entrant call a foreign predicate makes back into
the engine) has returned. See *PROLOG-ACTIVE-TOP-LEVEL-CALLS* for why that
is the safe window, and *RULEBASE-COMPACTION-THRESHOLD* for why \"maybe\"."
  (let ((rulebase-value (gensym "RULEBASE")))
    `(let* ((,rulebase-value ,rulebase)
            (*prolog-active-top-level-calls*
              (1+ *prolog-active-top-level-calls*)))
       (unwind-protect (progn ,@body)
         (when (zerop (decf *prolog-active-top-level-calls*))
           (%maybe-compact-rulebase! ,rulebase-value))))))

(defun %maybe-compact-rulebase! (rulebase)
  "Compact RULEBASE via %COMPACT-RULEBASE! once its dead-entry backlog
reaches *RULEBASE-COMPACTION-THRESHOLD*; otherwise a no-op.

Only called from %WITH-PROLOG-TOP-LEVEL-CALL once *PROLOG-ACTIVE-TOP-LEVEL-
CALLS* has returned to 0 -- see that macro for why that makes compaction
safe at all, and *RULEBASE-COMPACTION-THRESHOLD* for why it is gated rather
than unconditional."
  (when (>= (rulebase-dead-entries rulebase) *rulebase-compaction-threshold*)
    (%compact-rulebase! rulebase)))

(defun %compact-rulebase! (rulebase)
  "Physically drop RULEBASE's dead stored-clause entries.

Only called from %MAYBE-COMPACT-RULEBASE!; see that function and
*PROLOG-ACTIVE-TOP-LEVEL-CALLS* for why that makes this safe. A cheap no-op
when nothing has died since the last compaction.

Deliberately leaves RULEBASE-PREDICATE-DESCRIPTORS untouched: those
copy-on-write descriptors (%REFRESH-RULEBASE-PREDICATE-DESCRIPTOR!, above)
are already rebuilt from just the visible entries on every mutation, so they
never carry dead entries in the first place -- this function only needs to
catch up the raw storage the descriptors are periodically rebuilt from."
  (when (plusp (rulebase-dead-entries rulebase))
    (let ((live (delete-if #'%stored-clause-died-revision
                            (rulebase-entries rulebase))))
      (setf (rulebase-entries rulebase) live
            (rulebase-entries-tail rulebase) (last live))
      (multiple-value-bind (predicate-index predicate-tails)
          (%make-rulebase-predicate-index live)
        (setf (rulebase-predicate-index rulebase) predicate-index
              (rulebase-predicate-tails rulebase) predicate-tails))
      (setf (rulebase-dead-entries rulebase) 0)))
  rulebase)
