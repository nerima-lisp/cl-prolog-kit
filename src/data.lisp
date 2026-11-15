;;;; The rulebase container: its struct, construction/copy/replace
;;;; lifecycle, and the ordered logical-update operations (insert, retract,
;;;; visibility snapshots) layered on top of it.  Clause representation lives
;;;; in clause.lisp and predicate lookup in predicate-index.lisp; tabling data
;;;; lives in table-variant.lisp and source-load bookkeeping in
;;;; source-registry.lisp.

(in-package #:cl-prolog-kit)

(defstruct (prolog-capability-policy
            (:constructor %make-prolog-capability-policy
                (allow-file-access allowed-foreign-predicates))
            (:copier nil))
  "Restrictions a rulebase places on the Prolog goals it runs.

ALLOW-FILE-ACCESS permits builtins and directives that open a file by name.
ALLOWED-FOREIGN-PREDICATES is :ALL or a list of (NAME ARITY) indicators naming
the foreign predicates goals may call."
  (allow-file-access nil :read-only t)
  (allowed-foreign-predicates '() :type (or (eql :all) list) :read-only t))

(defun make-prolog-capability-policy (&key (allow-file-access nil)
                                           (allowed-foreign-predicates '()))
  "Return a capability policy; see PROLOG-CAPABILITY-POLICY."
  (unless (or (eq allowed-foreign-predicates :all)
              (and (listp allowed-foreign-predicates)
                   (every (lambda (indicator)
                            (and (consp indicator)
                                 (symbolp (first indicator))
                                 (consp (rest indicator))
                                 (typep (second indicator) '(integer 0 *))
                                 (null (cddr indicator))))
                          allowed-foreign-predicates)))
    (error 'type-error
           :datum allowed-foreign-predicates
           :expected-type '(or (eql :all) list)))
  (%make-prolog-capability-policy
   (and allow-file-access t)
   (if (eq allowed-foreign-predicates :all)
       :all
       (mapcar #'copy-list allowed-foreign-predicates))))

(defstruct (rulebase (:copier nil) (:constructor %make-rulebase))
  "An ordered logical-update database of clauses."
  (entries '() :type list)
  (entries-tail '() :type list)
  (predicate-index (make-hash-table :test #'equal) :type hash-table)
  (predicate-tails (make-hash-table :test #'equal) :type hash-table)
  (predicate-descriptors (make-hash-table :test #'eq) :type hash-table)
  (revision 0 :type (integer 0 *))
  ;; Count of entries in ENTRIES (and therefore in the buckets of
  ;; PREDICATE-INDEX) whose DIED-REVISION is non-NIL but which have not yet
  ;; been physically dropped by %COMPACT-RULEBASE!.  See
  ;; *PROLOG-ACTIVE-TOP-LEVEL-CALLS* below for why they are not dropped
  ;; immediately when they die.
  (dead-entries 0 :type (integer 0 *))
  (operator-table *standard-operator-table* :type operator-table)
  (predicate-properties (make-hash-table :test #'equal) :type hash-table)
  (io-context (make-prolog-io-context) :type prolog-io-context)
  ;; NIL means unrestricted.  Read on every foreign dispatch and file open.
  (capability-policy nil :type (or null prolog-capability-policy))
  (module-registry (make-module-registry) :type module-registry)
  (source-registry (%make-source-registry) :type hash-table)
  (prolog-flag-values (make-hash-table :test #'equal) :type hash-table)
  (char-conversions (make-hash-table :test #'eql) :type hash-table)
  (table-declarations (make-hash-table :test #'equal) :type hash-table)
  (table-session-cache (make-hash-table :test #'eql)
                       :type hash-table :read-only t)
  (module-entries-cache (make-hash-table :test #'equal)
                        :type hash-table :read-only t)
  (left-recursion-analysis (make-hash-table :test #'equal)
                           :type hash-table :read-only t))

(defun %rulebase-predicate-descriptor (rulebase module predicate arity)
  "Return the current immutable descriptor, first rebuilding it when stale.
A rebuild installs a fresh descriptor; a lookup never changes one in place."
  (let* ((descriptors (rulebase-predicate-descriptors rulebase))
         (predicates (gethash module descriptors))
         (arities (and predicates (gethash predicate predicates)))
         (descriptor (and arities (gethash arity arities))))
    (if (eq descriptor :stale)
        (let ((entries (%rulebase-predicate-entries-at-revision
                        rulebase module predicate arity
                        (rulebase-revision rulebase))))
          (%set-rulebase-predicate-descriptor!
           descriptors module predicate arity
           (and entries (%build-predicate-descriptor entries))))
        descriptor)))

(defun %invalidate-rulebase-predicate-descriptor! (rulebase module predicate arity)
  "Mark the one descriptor affected by a rulebase mutation stale.
Rebuilding on every mutation made N inserts into one predicate cost O(N^2).
Every revision change that alters this predicate's visible clauses passes
through here, so the next lookup rebuilding at the then-current revision
yields what an eager rebuild would have; a descriptor a running query already
holds is never modified."
  (%set-rulebase-predicate-descriptor!
   (rulebase-predicate-descriptors rulebase)
   module predicate arity :stale))

(defun %rulebase-source-state (rulebase canonical-pathname)
  "Return CANONICAL-PATHNAME's load state and whether it is registered."
  (let ((record (gethash canonical-pathname
                         (rulebase-source-registry rulebase))))
    (and record (%source-record-state record))))

(defun %rulebase-source-record (rulebase canonical-pathname)
  (gethash canonical-pathname (rulebase-source-registry rulebase)))

(defun %set-rulebase-source-state! (rulebase canonical-pathname state)
  "Record STATE for CANONICAL-PATHNAME and return STATE."
  (check-type state (member :loading :loaded))
  (let ((record (or (%rulebase-source-record rulebase canonical-pathname)
                    (%make-source-record state))))
    (setf (%source-record-state record) state
          (gethash canonical-pathname (rulebase-source-registry rulebase)) record)
    state))

(defun make-rulebase (&key (clauses '())
                           (io-context (make-prolog-io-context))
                           (capability-policy nil))
  "Return a rulebase containing CLAUSES in resolution order.
CAPABILITY-POLICY, a PROLOG-CAPABILITY-POLICY or NIL, restricts its goals."
  (let ((entries
          (mapcar (lambda (clause)
                    (%make-owned-stored-clause
                     clause +default-prolog-module+ 0))
                  clauses)))
    (multiple-value-bind (predicate-index predicate-tails)
        (%make-rulebase-predicate-index entries)
      (%make-rulebase
       :entries entries
       :entries-tail (last entries)
       :predicate-index predicate-index
       :predicate-tails predicate-tails
       :predicate-descriptors
       (%make-rulebase-predicate-descriptors predicate-index 0)
       :io-context io-context
       :capability-policy capability-policy))))

(defun %copy-rulebase (rulebase &optional (copy-clause #'identity))
  "Return a detached mutable copy suitable for transactional updates.
COPY-CLAUSE maps each stored clause into the copy; immutable templates are shared."
  (let ((entries
          (mapcar
           (lambda (entry)
             (let ((copy
                     (%make-stored-clause
                      (funcall copy-clause (%stored-clause-clause entry))
                      (%stored-clause-template entry)
                      (%stored-clause-module entry)
                      (%stored-clause-born-revision entry)
                      (%stored-clause-source entry))))
               (setf (%stored-clause-died-revision copy)
                     (%stored-clause-died-revision entry))
               copy))
           (rulebase-entries rulebase))))
    (multiple-value-bind (predicate-index predicate-tails)
        (%make-rulebase-predicate-index entries)
      (%make-rulebase
       :entries entries
       :entries-tail (last entries)
       :predicate-index predicate-index
       :predicate-tails predicate-tails
       :predicate-descriptors
       (%make-rulebase-predicate-descriptors
        predicate-index (rulebase-revision rulebase))
       :revision (rulebase-revision rulebase)
       :dead-entries (rulebase-dead-entries rulebase)
       :operator-table (rulebase-operator-table rulebase)
       :predicate-properties
       (%copy-hash-table (rulebase-predicate-properties rulebase))
       :io-context (%copy-prolog-io-context (rulebase-io-context rulebase))
       :capability-policy (rulebase-capability-policy rulebase)
       :module-registry (module-registry-copy (rulebase-module-registry rulebase))
       :source-registry (%copy-source-registry (rulebase-source-registry rulebase))
       :prolog-flag-values
       (%copy-hash-table (rulebase-prolog-flag-values rulebase))
       :char-conversions
       (%copy-hash-table (rulebase-char-conversions rulebase))
       :table-declarations
       (let ((copy (make-hash-table :test #'equal)))
         (maphash (lambda (key owners)
                    (setf (gethash key copy) (copy-list owners)))
                  (rulebase-table-declarations rulebase))
         copy)
       :table-session-cache (make-hash-table :test #'eql)
       :module-entries-cache (make-hash-table :test #'equal)))))

(defun copy-rulebase (rulebase)
  "Return a detached copy of RULEBASE, including its complete runtime state.

Stored clauses and their cons-based terms are copied, so mutating terms
reachable from one rulebase never affects the other. Immutable atoms and
persistent metadata such as operator tables may be shared."
  (check-type rulebase rulebase)
  (%copy-rulebase rulebase #'%copy-clause))

(defun rulebase-extend (rulebase clauses)
  "Return a detached copy of RULEBASE shadow-extended by CLAUSES.

CLAUSES retain their order and precede the clauses already visible in
RULEBASE.  Operator declarations, predicate properties, I/O state, modules,
source registrations, flags, and character conversions are copied as well."
  (rulebase-insert-clauses! (copy-rulebase rulebase) clauses :position :first))

(macrolet ((transfer-slots! (&rest readers)
             `(setf ,@(loop for reader in readers
                            append `((,reader target) (,reader source))))))
  (defun %replace-rulebase! (target source)
    "Replace TARGET's complete state with SOURCE after a successful transaction."
    (transfer-slots! rulebase-entries
                     rulebase-entries-tail
                     rulebase-predicate-index
                     rulebase-predicate-tails
                     rulebase-dead-entries
                     rulebase-revision
                     rulebase-operator-table
                     rulebase-predicate-properties
                     rulebase-io-context
                     rulebase-capability-policy
                     rulebase-module-registry
                     rulebase-source-registry
                     rulebase-prolog-flag-values
                     rulebase-char-conversions
                     rulebase-table-declarations)
  (setf (rulebase-predicate-descriptors target)
        (%make-rulebase-predicate-descriptors
         (rulebase-predicate-index source)
         (rulebase-revision source)))
  (clrhash (rulebase-table-session-cache target))
  (clrhash (rulebase-module-entries-cache target))
  (clrhash (rulebase-left-recursion-analysis target))
  target))

(defun %next-rulebase-revision! (rulebase)
  (clrhash (rulebase-table-session-cache rulebase))
  (clrhash (rulebase-module-entries-cache rulebase))
  (clrhash (rulebase-left-recursion-analysis rulebase))
  (incf (rulebase-revision rulebase)))

(defun %rulebase-snapshot (rulebase)
  "Return the current revision and a detached list of visible internal entries."
  (let ((revision (rulebase-revision rulebase)))
    (values revision
            (loop for entry in (rulebase-entries rulebase)
                  when (%stored-clause-visible-p entry revision)
                    collect entry))))

(defun rulebase-visible-clauses (rulebase)
  "Return detached clauses visible at one current logical-update snapshot."
  (multiple-value-bind (revision entries) (%rulebase-snapshot rulebase)
    (declare (cl:ignore revision))
    (mapcar (lambda (entry)
              (%copy-clause (%stored-clause-clause entry)))
            entries)))

(defun %rulebase-module-entries (rulebase module)
  "Return the visible stored clauses defined by MODULE."
  (let* ((revision (rulebase-revision rulebase))
         (key (list revision module))
         (cache (rulebase-module-entries-cache rulebase)))
    (multiple-value-bind (entries presentp)
        (gethash key cache)
      (if presentp
          entries
          (setf (gethash key cache)
                (multiple-value-bind (snapshot-revision snapshot-entries)
                    (%rulebase-snapshot rulebase)
                  (declare (cl:ignore snapshot-revision))
                  (remove module snapshot-entries
                          :test-not #'eq
                          :key #'%stored-clause-module)))))))

(defun %rulebase-predicate-entries-at-revision
    (rulebase module predicate arity revision)
  "Return PREDICATE/ARITY clauses visible in MODULE at REVISION."
  (%visible-stored-clauses
   (gethash (list module predicate arity) (rulebase-predicate-index rulebase))
   revision))

(defun %rulebase-predicate-visible-p
    (rulebase module predicate arity revision)
  "Return true when PREDICATE/ARITY has a clause visible in MODULE at REVISION."
  (loop for entry in
          (gethash (list module predicate arity)
                   (rulebase-predicate-index rulebase))
        thereis (%stored-clause-visible-p entry revision)))

(defun %rulebase-predicate-entries (rulebase module predicate arity)
  "Return the current revision and immutable visible entries for one predicate."
  (let ((revision (rulebase-revision rulebase))
        (descriptor
          (%rulebase-predicate-descriptor rulebase module predicate arity)))
    (values revision
            (and descriptor (%predicate-descriptor-entries descriptor)))))

(defun rulebase-insert-clause! (rulebase clause
                                &key
                                  (position :last)
                                  (module +default-prolog-module+)
                                  source)
  "Insert CLAUSE at POSITION (:FIRST or :LAST) and return RULEBASE."
  (let ((entry
          (%make-owned-stored-clause
           clause module (%next-rulebase-revision! rulebase) source)))
    (ecase position
      (:first
       (let ((cell (cons entry (rulebase-entries rulebase))))
         (setf (rulebase-entries rulebase) cell)
         (when (null (cdr cell))
           (setf (rulebase-entries-tail rulebase) cell))))
      (:last
       (let ((cell (list entry))
             (tail (rulebase-entries-tail rulebase)))
         (if tail
             (setf (cdr tail) cell)
             (setf (rulebase-entries rulebase) cell))
         (setf (rulebase-entries-tail rulebase) cell))))
    (let ((key (%stored-clause-predicate-key entry)))
      (when key
        (%insert-index-entry!
         key entry position
         (rulebase-predicate-index rulebase)
         (rulebase-predicate-tails rulebase))
        (destructuring-bind (entry-module predicate arity) key
          (%invalidate-rulebase-predicate-descriptor!
           rulebase entry-module predicate arity)))))
  rulebase)

(defun rulebase-insert-clauses! (rulebase clauses
                                 &key
                                   (position :last)
                                   (module +default-prolog-module+)
                                   source)
  "Insert CLAUSES at POSITION (:FIRST or :LAST) in their order and return RULEBASE.

The result equals inserting each clause with RULEBASE-INSERT-CLAUSE! at :LAST,
or, for :FIRST, placing CLAUSES in their order before the existing clauses.
All of CLAUSES share one new revision, so a running query sees none of them,
and each affected predicate's descriptor is invalidated once.  An empty CLAUSES
leaves RULEBASE and its revision unchanged."
  (check-type position (member :first :last))
  (when (null clauses)
    (return-from rulebase-insert-clauses! rulebase))
  ;; Snapshot every clause before touching RULEBASE, so a malformed clause
  ;; leaves it unchanged.
  (let* ((revision (1+ (rulebase-revision rulebase)))
         (entries (mapcar (lambda (clause)
                            (%make-owned-stored-clause
                             clause module revision source))
                          clauses))
         (last-cell (last entries))
         ;; Per-bucket :FIRST insertion runs back to front to keep CLAUSES'
         ;; order; reversed before splicing ENTRIES into the rulebase.
         (index-order (if (eq position :first) (reverse entries) entries))
         (touched-keys (make-hash-table :test #'equal)))
    (%next-rulebase-revision! rulebase)
    (ecase position
      (:first
       (setf (cdr last-cell) (rulebase-entries rulebase))
       (when (null (cdr last-cell))
         (setf (rulebase-entries-tail rulebase) last-cell))
       (setf (rulebase-entries rulebase) entries))
      (:last
       (let ((tail (rulebase-entries-tail rulebase)))
         (if tail
             (setf (cdr tail) entries)
             (setf (rulebase-entries rulebase) entries))
         (setf (rulebase-entries-tail rulebase) last-cell))))
    (dolist (entry index-order)
      (let ((key (%stored-clause-predicate-key entry)))
        (when key
          (%insert-index-entry!
           key entry position
           (rulebase-predicate-index rulebase)
           (rulebase-predicate-tails rulebase))
          (setf (gethash key touched-keys) t))))
    (loop for key being the hash-keys of touched-keys
          do (destructuring-bind (entry-module predicate arity) key
               (%invalidate-rulebase-predicate-descriptor!
                rulebase entry-module predicate arity))))
  rulebase)

(defun %rulebase-retract-entry! (rulebase entry)
  "Mark ENTRY dead and invalidate its predicate descriptor."
  (when (null (%stored-clause-died-revision entry))
    (let ((key (%stored-clause-predicate-key entry)))
      (setf (%stored-clause-died-revision entry)
            (%next-rulebase-revision! rulebase))
      (incf (rulebase-dead-entries rulebase))
      (when key
        (destructuring-bind (module predicate arity) key
          (%invalidate-rulebase-predicate-descriptor!
           rulebase module predicate arity))))
    t))

(defun %rulebase-retract-entries! (rulebase entries)
  "Assign one death revision to live ENTRIES and invalidate affected descriptors."
  (let ((live (remove-if #'%stored-clause-died-revision entries)))
    (when live
      (let ((revision (%next-rulebase-revision! rulebase))
            (keys
              (remove-duplicates
               (remove nil (mapcar #'%stored-clause-predicate-key live))
               :test #'equal)))
        (dolist (entry live)
          (setf (%stored-clause-died-revision entry) revision))
        (incf (rulebase-dead-entries rulebase) (length live))
        (dolist (key keys)
          (destructuring-bind (module predicate arity) key
            (%invalidate-rulebase-predicate-descriptor!
             rulebase module predicate arity)))))
    (not (null live))))

(defun rulebase-snapshot (rulebase)
  "Return a detached rulebase with RULEBASE's current visible state.

Unlike COPY-RULEBASE, the immutable stored clause terms and compiled clause
templates are shared rather than copied, so a snapshot costs one new
stored-clause record per entry plus fresh lookup tables.  Every mutable
part (clause lifetimes, predicate index, properties, flags, modules, source
registry, table declarations, caches, and the I/O stream registry) belongs
to the snapshot alone: later asserts, retracts, or declarations on either
rulebase never affect the other.  The underlying Lisp streams of the I/O
context are shared.

A rulebase may be used by one thread at a time; hand another thread a
snapshot rather than the rulebase itself."
  (check-type rulebase rulebase)
  (%copy-rulebase rulebase))

(defun %rulebase-live-entries-sharing-templates (rulebase entries)
  "Return RULEBASE's live stored clauses whose template belongs to ENTRIES.

A loading transaction publishes copied stored-clause records that keep the
original template (%COPY-RULEBASE, %REPLACE-RULEBASE!), and every insertion
compiles a fresh template, so the template identifies one inserted clause
even after its record has been replaced."
  (let ((templates (make-hash-table :test #'eq))
        (keys '())
        (live '()))
    (dolist (entry entries)
      (setf (gethash (%stored-clause-template entry) templates) t)
      (pushnew (%stored-clause-predicate-key entry) keys :test #'equal))
    (flet ((collect (candidates)
             (dolist (candidate candidates)
               (when (and (null (%stored-clause-died-revision candidate))
                          (gethash (%stored-clause-template candidate)
                                   templates))
                 (push candidate live)))))
      ;; Clauses with a malformed head have no predicate key and live only
      ;; in RULEBASE-ENTRIES; every other entry sits in exactly one bucket.
      (if (member nil keys)
          (collect (rulebase-entries rulebase))
          (dolist (key keys)
            (collect (gethash key (rulebase-predicate-index rulebase))))))
    live))

(defun call-with-overlay-facts (rulebase clauses function)
  "Insert CLAUSES at the end of RULEBASE, call FUNCTION, then retract them.

CLAUSES may be facts or rules.  The inserted clauses are retracted when
FUNCTION returns or exits non-locally, and only those clauses: anything
FUNCTION itself asserts or retracts is left as it is, and an overlay clause
FUNCTION already retracted is not retracted again.  Returns FUNCTION's
values."
  (check-type rulebase rulebase)
  (let ((inserted '()))
    (unwind-protect
         (progn
           (when clauses
             (let ((old-tail (rulebase-entries-tail rulebase)))
               (rulebase-insert-clauses! rulebase clauses :position :last)
               ;; Bulk insertion splices the new cells after the old tail.
               (setf inserted (copy-list (if old-tail
                                             (cdr old-tail)
                                             (rulebase-entries rulebase))))))
           (funcall function))
      (when inserted
        (%rulebase-retract-entries!
         rulebase
         (%rulebase-live-entries-sharing-templates rulebase inserted))))))

(defmacro with-overlay-facts ((rulebase clauses) &body body)
  "Evaluate BODY with CLAUSES temporarily appended to RULEBASE.
See CALL-WITH-OVERLAY-FACTS."
  `(call-with-overlay-facts ,rulebase ,clauses (lambda () ,@body)))
