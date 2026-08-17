;;;; Source-loading fixtures, input media, and directive/rollback atomicity
;;;; tests.  consult/load_files/ensure_loaded semantics live in
;;;; source-loader-transactions-test.lisp; interning/resource-error safety in
;;;; source-loader-limits-test.lisp.

(in-package #:cl-prolog-kit.tests)

(defun %temporary-prolog-pathname ()
  (merge-pathnames
   (make-pathname :name (format nil ".cl-prolog-kit-source-~A" (gensym))
                  :type "pl")
   (truename ".")))

(defun rewrite-prolog-file! (pathname text)
  "Overwrite PATHNAME's contents with TEXT, superseding any existing file."
  (with-open-file (output pathname :direction :output :if-exists :supersede)
    (write-string text output)))

(defun %call-with-temporary-prolog-files (sources function)
  (let ((pathnames (loop repeat (length sources)
                         collect (%temporary-prolog-pathname))))
    (unwind-protect
         (progn
           (loop for pathname in pathnames
                 for source in sources
                 do (rewrite-prolog-file! pathname source))
           (funcall function pathnames))
      (dolist (pathname pathnames)
        (when (probe-file pathname)
          (delete-file pathname))))))

(defmacro with-temporary-prolog-files ((&rest bindings) &body body)
  `(%call-with-temporary-prolog-files
    (list ,@(mapcar #'second bindings))
    (lambda (pathnames)
      (destructuring-bind ,(mapcar #'first bindings) pathnames
        ,@body))))

(defun %prolog-path-atom (pathname)
  (with-output-to-string (output)
    (write-char #\' output)
    (loop for character across (namestring pathname)
          do (write-char character output)
             (when (char= character #\')
               (write-char character output)))
    (write-char #\' output)))

(defun %prolog-path-list (pathnames)
  (format nil "[~{~A~^, ~}]" (mapcar #'%prolog-path-atom pathnames)))

(defun %call-with-source-medium (medium source function)
  (ecase medium
    (:string (funcall function source))
    (:stream
     (with-input-from-string (stream source)
       (let ((result (funcall function stream)))
         (is (open-stream-p stream))
         result)))
    (:pathname
     (let ((pathname (%temporary-prolog-pathname)))
       (unwind-protect
            (progn
              (with-open-file (output pathname
                                      :direction :output
                                      :if-exists :supersede)
                (write-string source output))
              (funcall function pathname))
         (when (probe-file pathname)
           (delete-file pathname)))))))

(defun %consult-through-medium (medium source &optional (rulebase (make-rulebase)))
  (%call-with-source-medium
   medium source
   (lambda (input)
     (consult-prolog input rulebase))))

(defun %source-query-succeeds-p (rulebase source)
  (prolog-succeeds-p rulebase (%read-prolog-query rulebase source)))

(defun %source-list-query-succeeds-p (rulebase sources query-format)
  "Check that QUERY-FORMAT succeeds when SOURCES are rendered as a Prolog list."
  (%source-query-succeeds-p
   rulebase
   (format nil query-format (%prolog-path-list sources))))

(defmacro %source-queries-succeed-p (rulebase &rest sources)
  "Assert that each source query succeeds against RULEBASE."
  `(progn
     ,@(mapcar (lambda (source)
                 `(is (%source-query-succeeds-p ,rulebase ,source)))
               sources)))

(defmacro %source-queries-fail-p (rulebase &rest sources)
  "Assert that each source query fails against RULEBASE."
  `(progn
     ,@(mapcar (lambda (source)
                 `(is (not (%source-query-succeeds-p ,rulebase ,source))))
               sources)))

(defun %source-predicate-defined-p (rulebase name)
  (find name (rulebase-visible-clauses rulebase)
        :key (lambda (clause)
               (car (clause-head clause)))
        :test #'eq))

(deftest source-loader-accepts-each-public-input-medium ()
  (dolist (medium '(:string :stream :pathname))
    (let ((rulebase (%consult-through-medium medium "loaded(ok).")))
      (is (%source-query-succeeds-p rulebase "loaded(ok)")))))

(deftest source-loader-syntax-errors-are-catchable-iso-errors ()
  (with-temporary-prolog-files ((source "broken( ."))
    (let ((rulebase (make-rulebase)))
      (is
       (prolog-succeeds-p
        rulebase
        (read-prolog-term
         (format nil
                 "catch(consult(~A), error(syntax_error(_), context(consult, _)), true)."
                 (%prolog-path-atom source))))))))

(deftest source-loader-applies-directives-in-source-order ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   ":- op(500, yfx, relates).~%~
                    relation(a relates b).~%~
                    :- dynamic(started/1).~%~
                    :- initialization(assertz(started(ok)))."))))
    (%source-queries-succeed-p
     rulebase
     "relation(a relates b)"
     "started(ok)")))

(deftest source-loader-dynamic-declarations-control-database-updates ()
  (let ((rulebase
          (consult-prolog
           ":- dynamic(mutable/1). mutable(initial). static.")))
    (%source-queries-succeed-p
     rulebase
     "assertz(mutable(added))"
     "mutable(added)")
    (signals-error
     (query-prolog rulebase (read-prolog-term "assertz(static)")))))

(deftest source-loader-expands-standard-dcg-rules ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   "sentence --> noun_phrase, verb_phrase.~%~
                    noun_phrase --> [the], noun.~%~
                    noun --> [cat] ; [dog].~%~
                    verb_phrase --> [sleeps]."))))
    (%source-queries-succeed-p
     rulebase
     "phrase(sentence, [the, cat, sleeps])"
     "phrase(sentence, [the, dog, sleeps, today], [today])")
    (%source-queries-fail-p
     rulebase
     "phrase(sentence, [the, bird, sleeps])")))

(deftest source-loader-supports-dcg-arguments-and-braced-goals ()
  (let ((rulebase
          (consult-prolog
           "token(X) --> [X], { X = accepted }.")))
    (%source-queries-succeed-p
     rulebase
     "phrase(token(accepted), [accepted])")
    (%source-queries-fail-p
     rulebase
     "phrase(token(rejected), [rejected])")))

(deftest source-loader-expands-dcg-rules-with-empty-and-cut-bodies ()
  (let ((rulebase
          (consult-prolog
           (format nil
                   "epsilon_rule --> [].~%~
                    triple --> [a], [b], [c].~%~
                    cut_rule --> [a], !, [b].~%~
                    cut_rule --> [a], [x]."))))
    (%source-queries-succeed-p
     rulebase
     "phrase(epsilon_rule, [])"
     "phrase(triple, [a, b, c])"
     "phrase(cut_rule, [a, b])")
    (%source-queries-fail-p
     rulebase
     "phrase(epsilon_rule, [x])"
     "phrase(cut_rule, [a, x])")))

(deftest-table source-loader-rejects-non-consult-forms ()
  (:signals (consult-prolog "?- true.")
            "Queries are not consultable source forms")
  (:signals (consult-prolog ":- unsupported_directive(value).")
            "Unknown directives must not be ignored")
  (:signals (consult-prolog ":- op(1300, xfx, invalid).")
            "Invalid directives fail during validation"))

;; %apply-source-directive! validates each directive's shape before acting on
;; it; these cases hand-build malformed directive terms directly (bypassing
;; the parser) to exercise every arity/shape guard.
(deftest-table source-directive-validates-arity-and-shape ()
  (:signals (cl-prolog-kit::%apply-source-directive!
             'cl-prolog-kit::not-a-list-goal (make-rulebase) (list '()) 'cl-prolog-kit::user)
            "a non-cons goal must be rejected")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::op 500 cl-prolog-kit::yfx) (make-rulebase) (list '())
             'cl-prolog-kit::user)
            "op directive must supply priority, specifier, and name")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::op 500 42 cl-prolog-kit::the_operator) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "op specifier must be an atom, not a number")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::dynamic cl-prolog-kit::a/1 cl-prolog-kit::b/1) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "dynamic directive must supply exactly one predicate indicator")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::table cl-prolog-kit::a/1 cl-prolog-kit::b/1) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "table directive must supply exactly one predicate indicator")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::use_module cl-prolog-kit::a cl-prolog-kit::b cl-prolog-kit::c
               cl-prolog-kit::d)
             (make-rulebase) (list '()) 'cl-prolog-kit::user)
            "use_module directive must supply 1-2 trailing arguments")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::initialization cl-prolog-kit::goal cl-prolog-kit::extra)
             (make-rulebase) (list '()) 'cl-prolog-kit::user)
            "initialization directive must supply exactly one goal")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::consult cl-prolog-kit::a cl-prolog-kit::b) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "consult directive must supply exactly one source")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::ensure_loaded cl-prolog-kit::a cl-prolog-kit::b) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "ensure_loaded directive must supply exactly one source")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::load_files cl-prolog-kit::a cl-prolog-kit::b cl-prolog-kit::c
               cl-prolog-kit::d)
             (make-rulebase) (list '()) 'cl-prolog-kit::user)
            "load_files directive must supply sources and at most one options argument")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::set_prolog_flag cl-prolog-kit::flag) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "set_prolog_flag directive must supply exactly two arguments")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::char_conversion cl-prolog-kit::x) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "char_conversion directive must supply exactly two arguments")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::discontiguous) (make-rulebase) (list '())
             'cl-prolog-kit::user)
            "discontiguous directive must supply at least one predicate indicator")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::multifile) (make-rulebase) (list '()) 'cl-prolog-kit::user)
            "multifile directive must supply at least one predicate indicator")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::include cl-prolog-kit::a cl-prolog-kit::b) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "include directive must supply exactly one source")
  (:signals (cl-prolog-kit::%apply-source-directive!
             '(cl-prolog-kit::totally-unrecognized-directive) (make-rulebase)
             (list '()) 'cl-prolog-kit::user)
            "unrecognized directive functors must be rejected"))

;; %source-term-clause rejects terms that cannot name a clause: a bare
;; non-callable atom-value, or a rule whose head is neither an atom nor a
;; compound term.  Both terms are hand-built to bypass the parser.
(deftest-table source-term-clause-rejects-non-clause-terms ()
  (:signals (cl-prolog-kit::%source-term-clause 42)
            "a bare number is not a consultable clause")
  (:signals (cl-prolog-kit::%source-term-clause
             (list (cl-prolog-kit::%prolog-symbol ":-") 42 'cl-prolog-kit::body))
            "a rule head must be an atom or compound term"))

(deftest source-loader-load-files-directive-honors-if-not-loaded-option ()
  (with-temporary-prolog-files
      ((loaded "load_files_directive_clause.")
       (entry ""))
    (rewrite-prolog-file!
     entry (format nil ":- load_files(~A, [if(not_loaded)])."
                   (%prolog-path-list (list loaded))))
    (let ((rulebase (consult-prolog entry)))
      (is (%source-query-succeeds-p rulebase "load_files_directive_clause")))))

(deftest source-loader-include-splices-atom-facts ()
  (with-temporary-prolog-files
      ((included "included_atom_fact.")
       (entry ""))
    (rewrite-prolog-file!
     entry (format nil ":- include(~A)." (%prolog-path-atom included)))
    (let ((rulebase (consult-prolog entry)))
      (is (%source-query-succeeds-p rulebase "included_atom_fact")))))

(deftest source-loader-include-rejects-atom-directives ()
  (with-temporary-prolog-files
      ((included ":- bare_atom_directive.")
       (entry ""))
    (rewrite-prolog-file!
     entry (format nil ":- include(~A)." (%prolog-path-atom included)))
    (signals-error (consult-prolog entry))))

(deftest source-loader-validation-is-atomic ()
  (dolist (source '("kept. ?- kept."
                    "kept. :- op(1300, xfx, invalid)."
                    "kept. :- unsupported_directive(value)."))
    (let ((rulebase (make-rulebase)))
      (signals-error (consult-prolog source rulebase))
      (is-equal '() (rulebase-visible-clauses rulebase)))))

(defmacro deftest-source-loader-rolls-back (name source)
  "Define NAME as a rollback regression test for SOURCE."
  `(deftest ,name ()
     (let ((rulebase (consult-prolog "original.")))
       (signals-error (consult-prolog ,source rulebase))
       (is (%source-query-succeeds-p rulebase "original"))
       (is (not (%source-predicate-defined-p rulebase 'cl-prolog-kit::temporary)))
       (is (%source-query-succeeds-p rulebase "assertz(after_rollback)"))
       (is (%source-query-succeeds-p rulebase "after_rollback"))
       (is (eq (last (cl-prolog-kit::rulebase-entries rulebase))
               (cl-prolog-kit::rulebase-entries-tail rulebase)))
       (is (loop for predicate-key being the hash-keys
                   of (cl-prolog-kit::rulebase-predicate-index rulebase)
                   using (hash-value entries)
                 always
                 (eq (last entries)
                     (gethash predicate-key
                              (cl-prolog-kit::rulebase-predicate-tails
                               rulebase))))))))

(deftest-source-loader-rolls-back
  source-loader-preserves-an-existing-rulebase-on-failure
  "temporary. ?- true.")

(deftest-source-loader-rolls-back
  source-loader-rolls-back-failed-initialization
  "temporary. :- initialization(fail).")

(deftest source-loader-rolls-back-io-context-changes ()
  (let* ((original-output (make-string-output-stream))
         (transient-output (make-string-output-stream))
         (context (cl-prolog-kit::make-prolog-io-context
                   :output original-output
                   :error-output (make-string-output-stream)))
         (alias (cl-prolog-kit::%prolog-atom-symbol "transient_output"))
         (rulebase (make-rulebase :io-context context)))
    (with-closed-io-context (context)
      (cl-prolog-kit::%register-prolog-stream!
       context transient-output :output :alias alias)
      (signals-error
       (consult-prolog
        ":- initialization((set_output(transient_output), fail))."
        rulebase))
      (is (eq original-output
              (cl-prolog-kit::prolog-stream-stream
               (cl-prolog-kit::prolog-io-context-current-output
                (cl-prolog-kit::rulebase-io-context rulebase))))))))

(deftest source-loader-rolls-back-operator-and-predicate-properties ()
  (let ((rulebase (make-rulebase)))
    (signals-error
     (consult-prolog
      (format nil
              ":- op(500, yfx, transient).~%~
               :- dynamic(transient/1).~%~
               :- initialization(fail).")
      rulebase))
    (is (null (cl-prolog-kit::%operator-table-find
               (cl-prolog-kit::rulebase-operator-table rulebase)
               'cl-prolog-kit::transient)))
    (is (null (cl-prolog-kit::%rulebase-predicate-property
               rulebase 'cl-prolog-kit::transient 1)))))

