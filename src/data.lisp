;;;; The rulebase container: its struct, construction/copy/replace
;;;; lifecycle, and the ordered logical-update operations (insert, retract,
;;;; visibility snapshots) layered on top of it.  Clause representation lives
;;;; in clause.lisp and predicate lookup in predicate-index.lisp; tabling data
;;;; lives in table-variant.lisp and source-load bookkeeping in
;;;; source-registry.lisp.

(in-package #:cl-prolog-kit)

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
  "Return the current immutable descriptor without changing any lookup table."
  (let* ((predicates (gethash module (rulebase-predicate-descriptors rulebase)))
         (arities (and predicates (gethash predicate predicates))))
    (and arities (gethash arity arities))))

(defun %refresh-rulebase-predicate-descriptor! (rulebase module predicate arity)
  "Copy-on-write the one descriptor affected by a rulebase mutation."
  (let ((entries (%rulebase-predicate-entries-at-revision
                  rulebase module predicate arity
                  (rulebase-revision rulebase))))
    (%set-rulebase-predicate-descriptor!
     (rulebase-predicate-descriptors rulebase)
     module predicate arity
     (and entries (%build-predicate-descriptor entries)))))

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
                           (io-context (make-prolog-io-context)))
  "Return a rulebase containing CLAUSES in resolution order."
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
       :io-context io-context))))

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
  (let ((extended (copy-rulebase rulebase)))
    (dolist (clause (reverse clauses) extended)
      (rulebase-insert-clause! extended clause :position :first))))

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
          (%refresh-rulebase-predicate-descriptor!
           rulebase entry-module predicate arity)))))
  rulebase)

(defun %rulebase-retract-entry! (rulebase entry)
  "Mark ENTRY dead and copy-on-write its predicate descriptor."
  (when (null (%stored-clause-died-revision entry))
    (let ((key (%stored-clause-predicate-key entry)))
      (setf (%stored-clause-died-revision entry)
            (%next-rulebase-revision! rulebase))
      (incf (rulebase-dead-entries rulebase))
      (when key
        (destructuring-bind (module predicate arity) key
          (%refresh-rulebase-predicate-descriptor!
           rulebase module predicate arity))))
    t))

(defun %rulebase-retract-entries! (rulebase entries)
  "Assign one death revision to live ENTRIES and refresh affected descriptors."
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
            (%refresh-rulebase-predicate-descriptor!
             rulebase module predicate arity)))))
    (not (null live))))
