;;;; Engine surface, data model, and macro behavior tests.

(in-package #:cl-prolog-kit.tests)

(deftest-table package-surface ()
  (:is (find-package "CL-PROLOG-KIT"))
  (:exported "PROLOG" "CL-PROLOG-KIT")
  (:exported "QUERY-PROLOG" "CL-PROLOG-KIT")
  (:exported "MAP-PROLOG-SOLUTIONS" "CL-PROLOG-KIT")
  (:not-exported "DEFINE-BUILTIN" "CL-PROLOG-KIT")
  (:exported "DEF-RULE" "CL-PROLOG-KIT")
  (:exported "PHRASE" "CL-PROLOG-KIT")
  (:exported "UNIFY" "CL-PROLOG-KIT")
  (:exported "LOGIC-SUBSTITUTE" "CL-PROLOG-KIT")
  (:exported "FRESH-LOGIC-VARIABLE" "CL-PROLOG-KIT")
  (:exported "RULEBASE-VISIBLE-CLAUSES" "CL-PROLOG-KIT")
  (:exported "COPY-RULEBASE" "CL-PROLOG-KIT")
  (:exported "RULEBASE-EXTEND" "CL-PROLOG-KIT")
  (:not-exported "RULEBASE-CLAUSES" "CL-PROLOG-KIT")
  (:not-exported "RULEBASE-REMOVE-CLAUSE!" "CL-PROLOG-KIT")
  (:exported "TERM_VARIABLES" "CL-PROLOG-KIT")
  (:not-exported "TERM-VARIABLES" "CL-PROLOG-KIT")
  (:exported "COPY_TERM" "CL-PROLOG-KIT")
  (:exported "UNIFY_WITH_OCCURS_CHECK" "CL-PROLOG-KIT")
  (:not-exported "COPY-TERM" "CL-PROLOG-KIT")
  (:exported "INVALID-GOAL-ERROR" "CL-PROLOG-KIT")
  (:not-exported "SUBSTITUTE-TERM" "CL-PROLOG-KIT")
  (:not-exported "*MAX-PROOF-DEPTH*" "CL-PROLOG-KIT")
  (:not-exported "UNIFY-FAILED-P" "CL-PROLOG-KIT")
  (:not-exported "WHEN-UNIFY-SUCCEEDS" "CL-PROLOG-KIT")
  (:not-exported "WHEN-UNIFY-FAILS" "CL-PROLOG-KIT")
  (:not-exported "QUERY-PROLOG-CPS" "CL-PROLOG-KIT")
  (:not-exported "PROLOG-SUCCEEDS-P-CPS" "CL-PROLOG-KIT")
  (:is (logic-var-p '?x))
  (:is (logic-var-p (fresh-logic-variable)))
  (:is-not (logic-var-p :?keyword))
  (:is-not (logic-var-p 'plain))
  (:is-not (logic-var-p (make-symbol "")))
  (:is-not (logic-var-p 42)))

(deftest exported-term-builtin-names-dispatch ()
  (let ((rulebase (make-rulebase)))
    (is-equal '(((?x . ?x) (?y . ?y) (?variables ?x ?y)))
              (query-prolog rulebase
                            '(term_variables (pair ?x ?y ?x) ?variables)))
    (let* ((solution (query-prolog-first
                      rulebase '(copy_term (pair ?x ?x) ?copy)))
           (copy (solution-binding '?copy solution)))
      (is (logic-var-p (second copy)))
      (is (eq (second copy) (third copy))))))

(deftest rulebase-data-model ()
  (let ((rb (prolog
              ((parent tom bob))
              ((parent bob alice))
              ((ancestor ?x ?y) (parent ?x ?y))
              ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))))
    (is (rulebase-p rb))
    (is-equal 4 (length (rulebase-visible-clauses rb)))
    (let ((fact (first (rulebase-visible-clauses rb)))
          (rule (third (rulebase-visible-clauses rb))))
      (is-equal '(parent tom bob) (clause-head fact))
      (is-equal '() (clause-body fact))
      (is-equal '(ancestor ?x ?y) (clause-head rule))
      (is-equal '((parent ?x ?y)) (clause-body rule)))))

(deftest proof-analysis-cache-follows-rulebase-revisions ()
  (let* ((depth 2048)
         (predicates
           (loop repeat (1+ depth)
                 collect (gensym "DEPENDENCY-")))
         (root-predicate (first predicates))
         (last-predicate (car (last predicates)))
         (clauses
           (loop for remaining on predicates
                 while (rest remaining)
                 collect
                 (make-clause
                  (list (first remaining))
                  (list (list (second remaining))))))
         (rulebase (make-rulebase :clauses clauses))
         (session (cl-prolog-kit::%make-rulebase-table-session rulebase))
         (state (cl-prolog-kit::%make-proof-state
  rulebase
  (quote ())
  (cl-prolog-kit::%make-environment-index (quote ()))
  nil
  cl-prolog-kit::+default-prolog-module+
  session
  (cl-prolog-kit::%make-cut-tag)))
         (second-session
           (cl-prolog-kit::%make-rulebase-table-session rulebase))
         (second-state
           (cl-prolog-kit::%make-proof-state
  rulebase
  (quote ())
  (cl-prolog-kit::%make-environment-index (quote ()))
  nil
  cl-prolog-kit::+default-prolog-module+
  second-session
  (cl-prolog-kit::%make-cut-tag)))
         (root-goal (list root-predicate))
         (last-goal (list last-predicate))
         (first-snapshot (cl-prolog-kit::%proof-module-entries state)))
    (is (eq first-snapshot (cl-prolog-kit::%proof-module-entries state)))
    (is-equal 0
              (hash-table-count
               (cl-prolog-kit::rulebase-left-recursion-analysis rulebase)))
    (is (not (cl-prolog-kit::%left-recursive-p root-goal state)))
    (is-equal 1
              (hash-table-count
               (cl-prolog-kit::rulebase-left-recursion-analysis rulebase)))
    (is (not (cl-prolog-kit::%left-recursive-p last-goal second-state)))
    (is-equal 1
              (hash-table-count
               (cl-prolog-kit::rulebase-left-recursion-analysis rulebase)))
    (let* ((copy (copy-rulebase rulebase))
           (copy-session
             (cl-prolog-kit::%make-rulebase-table-session copy))
           (copy-state
             (cl-prolog-kit::%make-proof-state
  copy
  (quote ())
  (cl-prolog-kit::%make-environment-index (quote ()))
  nil
  cl-prolog-kit::+default-prolog-module+
  copy-session
  (cl-prolog-kit::%make-cut-tag))))
      (is (not (eq (cl-prolog-kit::rulebase-left-recursion-analysis rulebase)
                   (cl-prolog-kit::rulebase-left-recursion-analysis copy))))
      (is-equal 0
                (hash-table-count
                 (cl-prolog-kit::rulebase-left-recursion-analysis copy)))
      (is (not (cl-prolog-kit::%left-recursive-p root-goal copy-state)))
      (is-equal 1
                (hash-table-count
                 (cl-prolog-kit::rulebase-left-recursion-analysis copy)))
      (is-equal 1
                (hash-table-count
                 (cl-prolog-kit::rulebase-left-recursion-analysis rulebase))))
    (rulebase-insert-clause!
     rulebase
     (make-clause last-goal (list root-goal)))
    (is-equal 0
              (hash-table-count
               (cl-prolog-kit::rulebase-left-recursion-analysis rulebase)))
    (let ((next-snapshot (cl-prolog-kit::%proof-module-entries state)))
      (is (not (eq first-snapshot next-snapshot)))
      (is-equal (1+ (length first-snapshot)) (length next-snapshot)))
    (is (cl-prolog-kit::%left-recursive-p root-goal state))
    (is (cl-prolog-kit::%left-recursive-p last-goal second-state))
    (is-equal 1
              (hash-table-count
               (cl-prolog-kit::rulebase-left-recursion-analysis rulebase)))))

(deftest-table rulebase-default-constructors ()
  (:equal '() (clause-body (make-clause '(lonely))))
  (:equal '() (rulebase-visible-clauses (make-rulebase))))

(deftest rulebase-source-registry-is-transactional ()
  (let* ((first-source #p"/canonical/first.pl")
         (second-source #p"/canonical/second.pl")
         (rulebase (make-rulebase)))
    (cl-prolog-kit::%set-rulebase-source-state! rulebase first-source :loaded)
    (let ((transaction (cl-prolog-kit::%copy-rulebase rulebase)))
      (cl-prolog-kit::%set-rulebase-source-state! transaction first-source :loading)
      (cl-prolog-kit::%set-rulebase-source-state! transaction second-source :loaded)
      (is-equal :loaded
                (cl-prolog-kit::%rulebase-source-state rulebase first-source))
      (multiple-value-bind (state present-p)
          (cl-prolog-kit::%rulebase-source-state rulebase second-source)
        (is-equal nil state)
        (is (not present-p)))
      (cl-prolog-kit::%replace-rulebase! rulebase transaction)
      (is-equal :loading
                (cl-prolog-kit::%rulebase-source-state rulebase first-source))
      (is-equal :loaded
                (cl-prolog-kit::%rulebase-source-state rulebase second-source)))))

(deftest-table prolog-invalid-clauses-signal ()
  (:signals (macroexpand-1 '(prolog (:facts (parent tom bob))))
            "Invalid PROLOG clause should fail at expansion time")
  (:signals (macroexpand-1 '(prolog (42)))
            "A clause head must be a list")
  (:signals (macroexpand-1 '(prolog ready))
            "A clause must be a list"))

(deftest-tree-contains def-rule-expands-to-clause-construction
    ((macroexpand-1 '(def-rule (warm ?x) (planet ?x))))
  make-clause
  '(warm ?x)
  '(list '(planet ?x)))

(deftest explicit-rulebase-composition ()
  (let* ((warm-rule (def-rule (warm ?x) (planet ?x)))
         (rulebase (make-rulebase
                    :clauses (list (make-clause '(planet earth))
                                   (make-clause '(inhabited ?x) '((planet ?x)))
                                   warm-rule))))
    (is-equal '(warm ?x) (clause-head warm-rule))
    (is-equal '(nil) (query-prolog rulebase '(planet earth)))
    (is-equal '(nil) (query-prolog rulebase '(inhabited earth)))
    (is-equal '(nil) (query-prolog rulebase '(warm earth)))))

(deftest extend-rulebase-shadowing ()
  (let* ((base (prolog ((color apple red))))
         (extended (extend-rulebase base
                     ((color apple green)))))
    (is-equal '(((?shade . red)))
              (query-prolog base '(color apple ?shade)))
    (is-equal '((?shade . green))
              (query-prolog-first extended '(color apple ?shade)))
    (is-equal '(((?shade . green)) ((?shade . red)))
              (query-prolog extended '(color apple ?shade)))))

(deftest rulebase-extension-preserves-complete-state ()
  (let ((base (prolog ((color apple red)))))
    (setf (gethash '(user marked 1)
                   (cl-prolog-kit::rulebase-predicate-properties base))
          :dynamic
          (gethash 'dialect (cl-prolog-kit::rulebase-prolog-flag-values base))
          :cl-prolog-kit
          (gethash #\a (cl-prolog-kit::rulebase-char-conversions base))
          #\b
          (gethash #P"fixture.pl" (cl-prolog-kit::rulebase-source-registry base))
          (cl-prolog-kit::%make-source-record :loaded))
    (let ((extended (extend-rulebase base ((color apple green)))))
      (is-equal '(((?shade . green)) ((?shade . red)))
                (query-prolog extended '(color apple ?shade)))
      (is (eq (cl-prolog-kit::rulebase-operator-table base)
              (cl-prolog-kit::rulebase-operator-table extended)))
      (is (not (eq (cl-prolog-kit::rulebase-predicate-properties base)
                   (cl-prolog-kit::rulebase-predicate-properties extended))))
      (is-equal :dynamic
                (gethash '(user marked 1)
                         (cl-prolog-kit::rulebase-predicate-properties extended)))
      (is (not (eq (cl-prolog-kit::rulebase-io-context base)
                   (cl-prolog-kit::rulebase-io-context extended))))
      (is (not (eq (cl-prolog-kit::rulebase-module-registry base)
                   (cl-prolog-kit::rulebase-module-registry extended))))
      (is (not (eq (cl-prolog-kit::rulebase-source-registry base)
                   (cl-prolog-kit::rulebase-source-registry extended))))
      (is-equal :loaded
                (cl-prolog-kit::%source-record-state
                 (gethash #P"fixture.pl"
                          (cl-prolog-kit::rulebase-source-registry extended))))
      (is-equal :cl-prolog-kit
                (gethash 'dialect
                         (cl-prolog-kit::rulebase-prolog-flag-values extended)))
      (is-equal #\b
                (gethash #\a
                         (cl-prolog-kit::rulebase-char-conversions extended))))))

(deftest query-first-distinguishes-ground-success-from-failure ()
  (let ((rulebase (prolog ((ready))
                          ((status ok)))))
    (multiple-value-bind (solution succeeded-p)
        (query-prolog-first rulebase '(ready))
      (is-equal nil solution)
      (is succeeded-p))
    (multiple-value-bind (solution succeeded-p)
        (query-prolog-first rulebase '(status missing))
      (is-equal nil solution)
      (is (not succeeded-p)))
    (let ((ran nil))
      (with-prolog-query () (rulebase '(ready))
        (setf ran t))
      (is ran))))

(deftest assert-query-first-keeps-options ()
  (with-macroexpansion (expansion
                        '(assert-query rb (ancestor tom ?who)
                                       :first ((?who . bob))
                                       :max-depth 3
                                       :project nil))
    (is (%tree-contains-p expansion 'query-prolog-first))
    (is (%tree-contains-p expansion ':max-depth))
    (is (%tree-contains-p expansion ':project)))
  (with-macroexpansion (expansion
                        '(deftest-queries example ((make-rulebase))
                           ((ancestor tom ?who) :first ((?who . bob)) :max-depth 3)))
    (is (%tree-contains-p expansion 'query-prolog-first))
    (is (%tree-contains-p expansion ':max-depth))))

(deftest when-guards-compile-to-closures ()
  (let ((rb (prolog
              ((score alice 42))
              ((score bob 7))
              ((high-score ?who ?n) (score ?who ?n) (:when (> ?n 10)))
              ((constant-yes) (:when t))
              ((constant-no) (:when nil)))))
    (let* ((rule (find '(high-score ?who ?n) (rulebase-visible-clauses rb)
                       :key #'clause-head :test #'equal))
           (guard (second (clause-body rule))))
      (is (functionp (second guard)) "The :when guard must be compiled to a closure")
      (is-equal '(?n) (cddr guard)))
    (is-equal '(((?who . alice) (?n . 42)))
              (query-prolog rb '(high-score ?who ?n)))
    (is (prolog-succeeds-p rb '(constant-yes)))
    (is (not (prolog-succeeds-p rb '(constant-no))))))

(deftest when-guard-shape-is-strict ()
  (is (cl-prolog-kit::%when-guard-p '(:when (> ?n 1))))
  (is (not (cl-prolog-kit::%when-guard-p '(:when))))
  (is (not (cl-prolog-kit::%when-guard-p '(:when f ?x))))
  (is (not (cl-prolog-kit::%when-guard-p 'ready)))
  (let ((rb (prolog
              ((ready))
              ((launchable) ready))))
    (is-equal '(nil) (query-prolog rb '(launchable)))))

(deftest guards-nest-inside-control-goals ()
  (let ((rb (prolog
              ((n 1)) ((n 2)) ((n 3))
              ((odd-n ?x) (n ?x) (not (:when (evenp ?x))))
              ((tagged ?x ?tag)
               (n ?x)
               (or ((:when (= ?x 1)) (= ?tag one))
                   (= ?tag other))))))
    (is-equal '(((?x . 1)) ((?x . 3))) (query-prolog rb '(odd-n ?x)))
    (is-same-set '(((?tag . one)) ((?tag . other)))
                 (query-prolog rb '(tagged 1 ?tag)))
    (is-equal '(((?tag . other)))
              (query-prolog rb '(tagged 2 ?tag)))))

(deftest def-rule-compiles-when-guards ()
  (with-macroexpansion (expansion
                        '(def-rule (even ?x)
                           (n ?x)
                           (:when (evenp ?x))))
    (is (%tree-contains-p expansion 'make-clause))
    (is (%tree-contains-p expansion 'lambda))
    (is (%tree-contains-p expansion 'evenp))
    (is (not (%tree-contains-p expansion 'eval-when)))
    (is (not (%tree-contains-p expansion 'eval)))))

(deftest with-prolog-query-and-match ()
  (let ((rb (make-family-rulebase))
        (seen nil))
    (with-prolog-query (?x) (rb '(ancestor ?x bob))
      (setf seen ?x))
    (is-equal 'tom seen)
    (with-prolog-query (?x) (rb '(ancestor eve tom))
      (setf seen :should-not-run))
    (is-equal 'tom seen)
    (is-equal :direct
              (prolog-match rb
                ((parent tom bob) :direct)
                ((ancestor eve tom) :wrong)
                ((ancestor tom eve) :fallback)))
    (is-equal nil
              (prolog-match rb
                ((parent eve tom) :never)))))
