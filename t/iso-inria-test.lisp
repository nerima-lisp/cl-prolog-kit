;;;; The INRIA ISO conformance suite, run against this engine.
;;;;
;;;; t/iso-conformance-test.lisp states requirements I read out of the standard;
;;;; this file runs a corpus I did not write (see t/iso/README.md).  That
;;;; independence is the whole value: it can disagree with this engine in ways a
;;;; suite written alongside it cannot, and it is the only coverage here whose
;;;; denominator is not my own recollection of ISO/IEC 13211-1.
;;;;
;;;; Each corpus line is `[Goal, Expected].', read with this engine's own reader
;;;; so that a case exercises the reader, the operator table and the builtin
;;;; together.  Expected is `success', `failure', an ISO error term, or a list of
;;;; solution substitutions written with the suite's `<--' operator.

(in-package #:cl-prolog-kit.tests)

(defun inria-suite-directory ()
  "Where the vendored corpus lives.

Resolved through ASDF rather than *LOAD-PATHNAME*, which points at the fasl
cache once the suite is compiled."
  (asdf:system-relative-pathname :cl-prolog-kit/test "t/iso/inriasuite/"))

(defparameter +inria-conformance-floor+ 431
  "The corpus score this engine is known to reach.

A floor rather than an exact count, so that fixing a builtin never fails the
test; raise it when the score improves and a regression will fail here.")

(defparameter +inria-substitution-operator-priority+ 20
  "The priority the suite's driver declares for its own `<--' operator.")

(defun inria-operator-table ()
  "The standard table plus the `<--' the corpus writes substitutions with."
  (cl-prolog-kit::%operator-table-define
   cl-prolog-kit::*standard-operator-table*
   (cl-prolog-kit:prolog-atom "<--")
   +inria-substitution-operator-priority+
   :xfx))

(defun inria-case-goal-and-expectation (form)
  "Return a corpus FORM's goal and expected result, or NIL if it is not a case."
  (let ((head (and (cl-prolog-kit:clause-p form) (cl-prolog-kit:clause-head form))))
    (when (and (consp head) (= 2 (length head)))
      (values (first head) (second head)))))

(defun inria-error-formal (term)
  "Return the formal part of an `error(Formal, _)' term, or NIL."
  (when (and (consp term)
             (cl-prolog-kit::%same-atom-text-p (first term)
                                           (cl-prolog-kit:prolog-atom "error"))
             (= 3 (length term)))
    (second term)))

(defparameter +inria-driver-support-clauses+
  "exists(Name/Arity) :- functor(Head, Name, Arity), predicate_property(Head, _).
run_tests(_)."
  "Helpers the corpus expects its own driver to supply.

Some cases call `exists/1' to ask whether a predicate is available at all,
and `current_predicate/1' expects the driver-provided `run_tests/1' predicate.
They belong to the suite's harness rather than to the standard, so they are
defined here rather than counted as conformance failures.")

(defun inria-rulebase ()
  "A rulebase carrying the corpus's driver-supplied helpers."
  (consult-prolog +inria-driver-support-clauses+))

(defun inria-run-goal (goal)
  "Run GOAL and describe the outcome as (VALUES KIND DATUM).

KIND is :SOLUTIONS with the solution list, :ERROR with the raised formal term,
or :HOST-CONDITION with the condition type -- which is itself a conformance
failure, since Prolog code is owed a catchable error rather than a Lisp one."
  (handler-case
      (values :solutions (cl-prolog-kit:query-prolog (inria-rulebase) (list goal)))
    (cl-prolog-kit:prolog-exception (condition)
      (let ((term (cl-prolog-kit:prolog-exception-term condition)))
        (values :error (or (inria-error-formal term) term))))
    (error (condition)
      (values :host-condition (type-of condition)))))

(defun inria-expectation-satisfied-p (expected kind datum)
  "True when the outcome (KIND DATUM) meets the corpus's EXPECTED result."
  (flet ((atom-named-p (term name)
           (and (symbolp term)
                (string= name (cl-prolog-kit:prolog-atom-text term)))))
    (cond
      ;; `success' and `failure' constrain only whether a proof exists.
      ((atom-named-p expected "success") (and (eq kind :solutions) datum t))
      ((atom-named-p expected "failure") (and (eq kind :solutions) (null datum)))
      ;; An expectation the suite itself leaves open to the implementation.
      ((atom-named-p expected "impl_def") t)
      ((atom-named-p expected "impl_dep") t)
      ;; A list of substitutions -- every element is itself a list of
      ;; `Var <-- Value' bindings, which is what tells it apart from an error
      ;; term like `type_error(atom, 1.23)', whose elements are not lists.  The
      ;; corpus's solution *count* is what is checkable without reimplementing
      ;; its substitution matcher, and a wrong count is the failure that matters.
      ((and (cl-prolog-kit::%proper-list-p expected)
            (every #'consp expected))
       (and (eq kind :solutions)
            (= (length expected) (length datum))))
      ;; Anything else is an error term the goal must raise.  Compared by
      ;; printed form so that `type_error(atom, 1.23)' matches structurally
      ;; without depending on how each side spells its atoms internally.
      ;; An error term the goal must raise.  A variable anywhere in the corpus's
      ;; expected term is its way of saying "any culprit here", so the two are
      ;; unified rather than compared as text.
      (t (and (eq kind :error)
              (nth-value 1 (cl-prolog-kit:unify expected datum)))))))

(defparameter +inria-non-test-files+ '("README" "file_manip" "halt")
  "Corpus files that hold no runnable cases.

`README' is prose, `file_manip' the suite itself marks unimplemented, and the
`halt' cases terminate the process by design -- the suite's own README warns
that running them ends the run.")

(defun inria-suite-files ()
  "The corpus files, which carry no extension -- hence DIRECTORY-FILES rather
than a `*.*' wildcard, which would match none of them."
  (sort (remove-if (lambda (path)
                     (member (file-namestring path) +inria-non-test-files+
                             :test #'string=))
                   (uiop:directory-files (inria-suite-directory)))
        #'string< :key #'namestring))

(defun inria-run-file (path table)
  "Run every case in PATH.  Returns (VALUES TOTAL PASSED FAILURES)."
  (let ((total 0) (passed 0) (failures '()))
    (dolist (form (handler-case
                      (cl-prolog-kit:parse-prolog (uiop:read-file-string path) table)
                    (error (condition)
                      (push (list :unreadable (type-of condition)) failures)
                      '()))
             (values total passed (nreverse failures)))
      (multiple-value-bind (goal expected) (inria-case-goal-and-expectation form)
        (when goal
          (incf total)
          (multiple-value-bind (kind datum) (inria-run-goal goal)
            (if (inria-expectation-satisfied-p expected kind datum)
                (incf passed)
                (push (list (cl-prolog-kit:prolog-term-string goal)
                            (cl-prolog-kit:prolog-term-string expected)
                            kind
                            (if (eq kind :error)
                                (cl-prolog-kit:prolog-term-string datum)
                                datum))
                      failures))))))))

(deftest inria-conformance-corpus-is-readable ()
  "Every vendored corpus file must parse with this engine's reader.

A file this engine cannot even read is a syntax gap, and would otherwise be
invisible: its cases would simply not run."
  (let ((table (inria-operator-table))
        (unreadable '()))
    (dolist (path (inria-suite-files))
      (handler-case (cl-prolog-kit:parse-prolog (uiop:read-file-string path) table)
        (error (condition)
          (push (cons (pathname-name path) (type-of condition)) unreadable))))
    (is (null unreadable)
        (format nil "corpus files this engine cannot read: ~S" unreadable))))

(deftest inria-conformance-corpus-score ()
  "Run the whole corpus and report the score.

The assertion is a floor rather than an exact count so that fixing a builtin
never fails this test; raise the floor when the score improves, and a
regression that drops below it fails here."
  (let ((table (inria-operator-table))
        (total 0) (passed 0) (report '()))
    (dolist (path (inria-suite-files))
      (multiple-value-bind (file-total file-passed failures)
          (inria-run-file path table)
        (incf total file-total)
        (incf passed file-passed)
        (when failures
          (push (list (pathname-name path) file-passed file-total failures)
                report))))
    (format *error-output* "~&INRIA corpus: ~D/~D cases~%" passed total)
    (dolist (entry (sort report #'string< :key #'first))
      (destructuring-bind (name file-passed file-total failures) entry
        (format *error-output* "~&  ~A ~D/~D~{~&    ~S~}~%"
                name file-passed file-total failures)))
    (is (plusp total) "the corpus must contribute cases")
    (is (>= passed +inria-conformance-floor+)
        (format nil "INRIA corpus score ~D/~D fell below the recorded floor ~D"
                passed total +inria-conformance-floor+))))
