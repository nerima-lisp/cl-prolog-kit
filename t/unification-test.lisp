;;;; Unification protocol tests.
(in-package #:cl-prolog-kit.tests)

(defun %test-environment-index-entry (variable index)
  (multiple-value-bind (binding present-p)
      (cl-prolog-kit::%environment-index-binding variable index)
    (if present-p
        (values (cons (cdr binding)
                      (cl-prolog-kit::%environment-index-rank variable index))
                t)
        (values nil nil))))

(deftest-unification
  unification-protocol
  (:substitute (pair ?x ?y) (pair left right) :expected (pair left right))
  (:fails (tag a) (tag b))
  (:not-ok ?x (wrap ?x))
  (:not-ok (wrap ?x) ?x)
  (:ok "same" "same")
  (:ok 7 7)
  (:not-ok 7 8)
  (:substitute
    (:outer (:inner . ?side) ?side)
    (:outer (:inner . :buy) ?other)
    :expected
    (:outer (:inner . :buy) :buy))
  (:substitute ?x ?y :initial-env ((?y . done)) :expected done))

(deftest
    cyclic-unification-and-substitution-terminate
    ()
    (let ((left (cons 'node nil))
          (right (cons 'node nil))
          (different (cons 'other nil)))
      (setf (cdr left) left
            (cdr right) right
            (cdr different) different)
      (is (nth-value 1 (unify left right)))
      (is (not (nth-value 1 (unify left different))))
      (let ((copy (logic-substitute left nil)))
        (is (not (eq copy left)))
        (is (eq copy (cdr copy)))))
    (let* ((existing (fresh-logic-variable "?EXISTING"))
           (new (fresh-logic-variable "?NEW"))
           (environment (list (cons existing 'bound))))
      (multiple-value-bind (extended ok) (unify new 'value environment)
        (is ok)
        (is (eq 'value (cdr (assoc new extended :test (function eq)))))
        (is (eq 'bound (cdr (assoc existing extended :test (function eq))))))))
  (deftest
    large-isomorphic-cyclic-lists-unify-after-scratch-hash-migration
    (:timeout 2)
    (let ((left (loop repeat 33 collect 'node))
          (right (loop repeat 33 collect 'node)))
      (setf (cdr (last left)) left
            (cdr (last right)) right)
      (multiple-value-bind (environment ok) (unify left right)
        (is ok)
        (is (null environment)))))

(deftest
  cyclic-substitution-preserves-sharing-and-variables
  ()
  (let* ((variable (fresh-logic-variable "?CYCLE"))
         (shared (cons variable nil))
         (root (cons shared shared)))
    (setf (cdr shared) shared)
    (is (cl-prolog-kit::%term-has-variables-p root))
    (let ((copy (logic-substitute root nil)))
      (is (eq (car copy) (cdr copy)))
      (is (eq (car copy) (cdr (car copy))))
      (is (eq variable (car (car copy)))))))

(deftest
    cyclic-variable-scan-and-query-collection-terminate
    ()
  (let* ((variable (fresh-logic-variable "?TERM-HAS-VARIABLES"))
         (ground-cycle (cons 'ground nil))
         (variable-cycle (cons variable nil))
         (query-cycle (cons '?x nil)))
    (setf (cdr ground-cycle) ground-cycle
          (cdr variable-cycle) variable-cycle
          (cdr query-cycle) query-cycle)
    (is (not (cl-prolog-kit::%term-has-variables-p 'ground)))
    (is (cl-prolog-kit::%term-has-variables-p variable))
    (is (not (cl-prolog-kit::%term-has-variables-p ground-cycle)))
    (is (cl-prolog-kit::%term-has-variables-p variable-cycle))
    (is-equal '(?x) (cl-prolog-kit::%collect-query-variables query-cycle))))
  (deftest unify-identical-object-fast-path-preserves-environment () (let* ((variable (fresh-logic-variable "?IDENTICAL")) (binding (cons (fresh-logic-variable "?BOUND") :value)) (environment (list binding))) (dolist (occurs-check (list nil t)) (multiple-value-bind (extended ok) (unify variable variable environment occurs-check) (is ok) (is (eq environment extended)) (is (eq binding (car extended))))))) (deftest unify-distinct-equal-objects-use-general-path () (let* ((left (copy-seq "same")) (right (copy-seq "same")) (environment (list (cons (fresh-logic-variable "?EXISTING") :value)))) (is (not (eq left right))) (dolist (occurs-check (list nil t)) (multiple-value-bind (extended ok) (unify left right environment occurs-check) (is ok) (is (eq environment extended)))))) (deftest question-prefixed-atoms-do-not-unify-as-variables () (let ((atom (read-prolog-term "'?x'."))) (is (not (logic-var-p atom))) (is (not (nth-value 1 (unify atom (quote cl-prolog-kit::a))))) (is (nth-value 1 (unify atom atom)))))
  (deftest
    indexed-alias-chain-terminates-with-linear-lookup
    (:timeout 3)
    (let* ((variables
          (loop repeat 50001
                collect (fresh-logic-variable "?CHAIN")))
           (environment
          (loop for tail on variables
                for variable = (car tail)
                for value = (if (cdr tail) (cadr tail)
              :resolved)
                collect (cons variable value)))
           (start (car variables)))
      (is (eq :resolved (cl-prolog-kit::%walk-term start environment)))
      (is (eq :resolved (logic-substitute start environment)))
      (multiple-value-bind (extended ok) (unify start :resolved environment)
        (is ok)
        (is (eq extended environment)))))
  (deftest
    duplicate-bindings-preserve-first-binding
    ()
    (let* ((variable (fresh-logic-variable "?DUPLICATE"))
           (environment (list (cons variable nil) (cons variable :ignored))))
      (is (null (cl-prolog-kit::%walk-term variable environment)))
      (is (null (logic-substitute variable environment)))
      (multiple-value-bind (extended ok) (unify variable nil environment)
        (is ok)
        (is (eq extended environment)))))
  (deftest
    alias-cycle-operations-terminate
    (:timeout 2)
    (let* ((x (fresh-logic-variable "?CYCLE-X"))
           (y (fresh-logic-variable "?CYCLE-Y"))
           (z (fresh-logic-variable "?CYCLE-Z"))
           (entry (fresh-logic-variable "?CYCLE-ENTRY"))
           (unbound (fresh-logic-variable "?UNBOUND"))
           (self-environment (list (cons x x)))
           (cycle-environment (list (cons x y) (cons y x)))
           (three-cycle-environment (list (cons x y) (cons y z) (cons z x)))
           (chain-into-cycle-environment
          (list (cons entry y) (cons x y) (cons y z) (cons z x)))
           (nil-terminal-environment (list (cons entry nil))))
      (is (eq unbound (cl-prolog-kit::%walk-term unbound nil)))
      (is (null (cl-prolog-kit::%walk-term entry nil-terminal-environment)))
      (is (eq x (cl-prolog-kit::%walk-term x self-environment)))
      (is (eq x (cl-prolog-kit::%walk-term x cycle-environment)))
      (is (eq x (cl-prolog-kit::%walk-term y cycle-environment)))
      (is (eq x (cl-prolog-kit::%walk-term y three-cycle-environment)))
      (is (eq x (cl-prolog-kit::%walk-term entry chain-into-cycle-environment)))
      (is (eq x (logic-substitute y cycle-environment)))
      (is (cl-prolog-kit::%occurs-p x y cycle-environment))
      (multiple-value-bind (extended ok) (unify x y cycle-environment)
        (is ok)
        (is (eq extended cycle-environment)))
      (multiple-value-bind (extended ok) (unify x :resolved cycle-environment)
        (is ok)
        (is (eq (cdr extended) cycle-environment))
        (is (eq :resolved (logic-substitute x extended)))
        (is (eq :resolved (logic-substitute y extended))))
      (is (eq x (cl-prolog-kit::%walk-term x cycle-environment)))))
  (deftest
    indexed-unification-updates-environment-order
    ()
    (let ((left-variable (fresh-logic-variable "?LEFT"))
          (right-variable (fresh-logic-variable "?RIGHT")))
      (multiple-value-bind (environment ok) (unify (list left-variable left-variable) (list right-variable :done))
        (is ok)
        (is (eq right-variable (caar environment)))
        (is (eq :done (cdar environment)))
        (is (eq left-variable (caadr environment)))
        (is (eq right-variable (cdadr environment)))
        (is (eq :done (logic-substitute left-variable environment)))
        (is (eq :done (logic-substitute right-variable environment))))))
  (deftest
    occurs-check-and-input-environment-remain-persistent
    ()
    (let* ((x (fresh-logic-variable "?OCCURS-X"))
           (y (fresh-logic-variable "?OCCURS-Y"))
           (z (fresh-logic-variable "?PERSISTENT"))
           (binding (cons y x))
           (environment (list binding))
           (term (list 'node y)))
      (multiple-value-bind (extended ok) (unify x term environment)
        (is (not ok))
        (is (null extended)))
      (is (eq binding (car environment)))
      (is (eq x (cdar environment)))
      (multiple-value-bind (extended ok) (unify z :resolved environment)
        (is ok)
        (is (eq environment (cdr extended)))
        (is (eq binding (car environment))))
      (is (eq binding (car environment)))
      (is (eq x (cdar environment)))))
  (deftest
      indexed-substitution-preserves-cyclic-cons-sharing
      ()
      (let* ((variable (fresh-logic-variable "?SHARED"))
             (shared (cons variable nil))
             (root (cons shared shared))
             (environment (list (cons variable :resolved))))
        (setf (cdr shared) shared)
        (let ((copy (logic-substitute root environment)))
          (is (not (eq copy root)))
          (is (eq (car copy) (cdr copy)))
          (is (eq :resolved (car (car copy))))
          (is (eq (car copy) (cdr (car copy)))))))
  (deftest
      indexed-substitution-root-fast-path-preserves-semantics
      ()
      (let* ((unbound (fresh-logic-variable "?ROOT-UNBOUND"))
             (alias (fresh-logic-variable "?ROOT-ALIAS"))
             (atom-alias (fresh-logic-variable "?ROOT-ATOM"))
             (cycle-x (fresh-logic-variable "?ROOT-CYCLE-X"))
             (cycle-y (fresh-logic-variable "?ROOT-CYCLE-Y"))
             (empty-index (cl-prolog-kit::%make-environment-index nil))
             (alias-index
               (cl-prolog-kit::%make-environment-index
                 (list (cons alias unbound))))
             (atom-index
               (cl-prolog-kit::%make-environment-index
                 (list (cons atom-alias :resolved))))
             (cycle-index
               (cl-prolog-kit::%make-environment-index
                 (list (cons cycle-x cycle-y)
                       (cons cycle-y cycle-x)))))
        (is (eq unbound
                (cl-prolog-kit::%logic-substitute-indexed
                  unbound empty-index)))
        (is (eq unbound
                (cl-prolog-kit::%logic-substitute-indexed
                  alias alias-index)))
        (is (eq :resolved
                (cl-prolog-kit::%logic-substitute-indexed
                  atom-alias atom-index)))
        (is (eq cycle-x
                (cl-prolog-kit::%logic-substitute-indexed
                  cycle-y cycle-index)))
        (let* ((shared (cons :shared :tail))
               (root (cons shared shared))
               (copy
                 (cl-prolog-kit::%logic-substitute-indexed root empty-index)))
          (is (not (eq copy root)))
          (is (eq (car copy) (cdr copy)))
          (is (not (eq (car copy) shared))))
        (let* ((dotted (cons :left :right))
               (copy
                 (cl-prolog-kit::%logic-substitute-indexed dotted empty-index)))
          (is (not (eq copy dotted)))
          (is (eq :left (car copy)))
          (is (eq :right (cdr copy))))
        (let ((cyclic (cons :root nil)))
          (setf (cdr cyclic) cyclic)
          (let ((copy
                  (cl-prolog-kit::%logic-substitute-indexed
                    cyclic empty-index)))
            (is (not (eq copy cyclic)))
            (is (eq copy (cdr copy)))))
        (let* ((variable (fresh-logic-variable "?ROOT-BOUND-CYCLE"))
               (cyclic (cons :bound nil)))
          (setf (cdr cyclic) cyclic)
          (let* ((index
                   (cl-prolog-kit::%make-environment-index
                     (list (cons variable cyclic))))
                 (copy
                   (cl-prolog-kit::%logic-substitute-indexed variable index)))
            (is (not (eq copy cyclic)))
            (is (eq :bound (car copy)))
            (is (eq copy (cdr copy)))))))
(deftest environment-index-overlay-preserves-parent-and-siblings ()
    (let* ((parent-variable (fresh-logic-variable "?PARENT"))
           (left-variable (fresh-logic-variable "?LEFT-CHILD"))
           (right-variable (fresh-logic-variable "?RIGHT-CHILD"))
           (parent-environment (list (cons parent-variable :parent)))
           (parent-index
             (cl-prolog-kit::%make-environment-index parent-environment)))
      (multiple-value-bind (left-environment left-ok left-index)
          (cl-prolog-kit::%unify-indexed
            left-variable
            :left
            parent-environment
            parent-index)
        (declare (ignore left-environment))
        (is left-ok)
        (multiple-value-bind (right-environment right-ok right-index)
            (cl-prolog-kit::%unify-indexed
              right-variable
              :right
              parent-environment
              parent-index)
          (declare (ignore right-environment))
          (is right-ok)
          (is (not (eq parent-index left-index)))
          (is (not (eq parent-index right-index)))
          (is
            (eq (cl-prolog-kit::%environment-index-table parent-index)
                (cl-prolog-kit::%environment-index-table left-index)))
          (is
            (eq (cl-prolog-kit::%environment-index-table parent-index)
                (cl-prolog-kit::%environment-index-table right-index)))
          (is
            (= 0
               (cl-prolog-kit::%environment-index-overlay-length
                 parent-index)))
          (is
            (= 1
               (cl-prolog-kit::%environment-index-overlay-length left-index)))
          (is
            (= 1
               (cl-prolog-kit::%environment-index-overlay-length right-index)))
          (is
            (eq :left
                (car
                  (%test-environment-index-entry
                    left-variable
                    left-index))))
          (is
            (eq :right
                (car
                  (%test-environment-index-entry
                    right-variable
                    right-index))))
          (multiple-value-bind (entry present-p)
              (%test-environment-index-entry
                right-variable
                left-index)
            (declare (ignore entry))
            (is (not present-p)))
          (multiple-value-bind (entry present-p)
              (%test-environment-index-entry
                left-variable
                right-index)
            (declare (ignore entry))
            (is (not present-p)))
          (multiple-value-bind (entry present-p)
              (%test-environment-index-entry
                left-variable
                parent-index)
            (declare (ignore entry))
            (is (not present-p)))))))
  (deftest environment-index-overlay-rolls-back-failed-unification ()
    (let* ((variable (fresh-logic-variable "?ROLLBACK"))
           (parent-index (cl-prolog-kit::%make-environment-index nil)))
      (multiple-value-bind (environment ok result-index)
          (cl-prolog-kit::%unify-indexed
            (list variable variable)
            (list :first :second)
            nil
            parent-index)
        (is (null environment))
        (is (not ok))
        (is (eq parent-index result-index))
        (is
          (= 0
             (cl-prolog-kit::%environment-index-overlay-length parent-index)))
        (multiple-value-bind (entry present-p)
            (%test-environment-index-entry variable parent-index)
          (declare (ignore entry))
          (is (not present-p))))))
  (deftest environment-index-compaction-preserves-ranks-and-cycle-choice ()
  (let* ((x (fresh-logic-variable "?COMPACT-X"))
         (y (fresh-logic-variable "?COMPACT-Y"))
         (z (fresh-logic-variable "?COMPACT-Z"))
         (fillers
           (loop repeat 4
                 collect (fresh-logic-variable "?COMPACT-FILLER")))
         (oldest-to-newest
           (append
             (list (cons x y) (cons y z) (cons z x))
             (loop for filler in fillers
                   collect (cons filler :filler))))
         (newest-binding (cons (car fillers) :newest))
         (base-index (cl-prolog-kit::%make-environment-index nil))
         (before-compaction
           (cl-prolog-kit::%extend-environment-index
             base-index
             oldest-to-newest))
         (compacted
           (cl-prolog-kit::%extend-environment-index
             before-compaction
             (list newest-binding))))
    (is (= 7 (length oldest-to-newest)))
    (is
      (= 7
         (cl-prolog-kit::%environment-index-overlay-length before-compaction)))
    (is
      (equal (reverse oldest-to-newest)
             (cl-prolog-kit::%environment-index-overlay before-compaction)))
    (is
      (= -1
         (cdr (%test-environment-index-entry x before-compaction))))
    (is
      (= -2
         (cdr (%test-environment-index-entry y before-compaction))))
    (is
      (= -3
         (cdr (%test-environment-index-entry z before-compaction))))
    (is
      (= 0
         (cl-prolog-kit::%environment-index-overlay-length compacted)))
    (is
      (and (eq (cl-prolog-kit::%environment-index-table base-index) (cl-prolog-kit::%environment-index-table compacted)) (= 1 (length (cl-prolog-kit::%environment-index-chunks compacted)))))
    (is
      (eq :newest
          (car
            (%test-environment-index-entry
              (car fillers)
              compacted))))
    (is
      (= -8
         (cdr
           (%test-environment-index-entry
             (car fillers)
             compacted))))
    (is
      (= -1
         (cdr (%test-environment-index-entry x compacted))))
    (is
      (= -2
         (cdr (%test-environment-index-entry y compacted))))
    (is
      (= -3
         (cdr (%test-environment-index-entry z compacted))))
    (is (eq z (cl-prolog-kit::%walk-term-indexed x compacted)))))
  (deftest environment-index-after-bindings-distinguishes-prefix-and-rebuild ()
    (let* ((variable (fresh-logic-variable "?PREFIX"))
           (parent-environment (list (cons variable :base)))
           (parent-index
             (cl-prolog-kit::%make-environment-index parent-environment))
           (older-binding (cons variable :older))
           (newest-binding (cons variable :newest))
           (bindings
             (cons newest-binding
                   (cons older-binding parent-environment)))
           (extended
             (cl-prolog-kit::%environment-index-after-bindings
               bindings
               parent-environment
               parent-index))
           (rebuilt
             (cl-prolog-kit::%environment-index-after-bindings
               (copy-tree bindings)
               parent-environment
               parent-index)))
      (is
        (eq parent-index
            (cl-prolog-kit::%environment-index-after-bindings
              parent-environment
              parent-environment
              parent-index)))
      (is
        (= 2
           (cl-prolog-kit::%environment-index-overlay-length extended)))
      (is
        (eq :newest
            (car
              (%test-environment-index-entry variable extended))))
      (is
        (= -2
           (cdr
             (%test-environment-index-entry variable extended))))
      (is
        (eq :base
            (car
              (%test-environment-index-entry
                variable
                parent-index))))
      (is
        (= 0
           (cl-prolog-kit::%environment-index-overlay-length rebuilt)))
      (is
        (not
          (eq (cl-prolog-kit::%environment-index-table parent-index)
              (cl-prolog-kit::%environment-index-table rebuilt))))
      (is
        (eq :newest
            (car
              (%test-environment-index-entry variable rebuilt))))))
  (deftest environment-index-long-alias-chain-keeps-overlay-bounded
    (:timeout 5)
    (let* ((variables
             (loop repeat 50001
                   collect (fresh-logic-variable "?BOUNDED-CHAIN")))
           (environment
             (loop for tail on variables
                   for variable = (car tail)
                   for value = (if (cdr tail) (cadr tail) :resolved)
                   collect (cons variable value)))
           (index (cl-prolog-kit::%make-environment-index environment))
           (maximum-overlay-length 0))
      (is
        (eq :resolved
            (cl-prolog-kit::%walk-term-indexed (car variables) index)))
      (loop repeat 17
            for variable = (fresh-logic-variable "?OVERLAY-BOUND")
            do (setf index
                     (cl-prolog-kit::%extend-environment-index
                       index
                       (list (cons variable :bound))))
               (setf maximum-overlay-length
                     (max
                       maximum-overlay-length
                       (cl-prolog-kit::%environment-index-overlay-length
                         index)))
               (is
                 (< (cl-prolog-kit::%environment-index-overlay-length index)
                    cl-prolog-kit::+environment-index-overlay-threshold+)))
      (progn (is (= 7 maximum-overlay-length)) (let ((chunk-sizes (mapcar (function cl-prolog-kit::%environment-index-chunk-size) (cl-prolog-kit::%environment-index-chunks index)))) (is (= (length chunk-sizes) (length (remove-duplicates chunk-sizes)))) (is (<= (length chunk-sizes) (integer-length 17)))))
      (is
        (eq :resolved
            (cl-prolog-kit::%walk-term-indexed (car variables) index)))))

(deftest
    indexed-substitution-upgrades-copy-map-with-bound-alias
    ()
  (let* ((alias (fresh-logic-variable "?COPY-ALIAS"))
         (bound (fresh-logic-variable "?COPY-BOUND"))
         (shared (cons alias nil))
         (nodes (loop repeat (1+ cl-prolog-kit::+freshening-map-threshold+)
                     collect (cons :node :tail)))
         (root (cons shared (cons shared nodes)))
         (index (cl-prolog-kit::%make-environment-index
                  (list (cons alias bound)
                        (cons bound :resolved)))))
    (setf (cdr shared) shared)
    (let ((copy (cl-prolog-kit::%logic-substitute-indexed root index)))
      (is (not (eq copy root)))
      (is (eq (car copy) (cadr copy)))
      (is (not (eq (car copy) shared)))
      (is (eq :resolved (car (car copy))))
      (is (eq (car copy) (cdr (car copy))))
      (is (not (eq (caddr copy) (first nodes)))))))

(deftest
    freshen-term-preserves-cyclic-conses
    ()
    (let ((variable (fresh-logic-variable "?CYCLE"))
          (cycle (cons nil nil)))
      (setf (car cycle) variable
            (cdr cycle) cycle)
      (let ((copy
              (cl-prolog-kit::%freshen-term
                cycle
                (make-hash-table :test (function eq)))))
        (is (not (eq copy cycle)))
        (is (eq copy (cdr copy)))
        (is (logic-var-p (car copy)))
        (is (not (eq variable (car copy))))
        (is (eq variable (car cycle)))
        (is (eq cycle (cdr cycle))))))
  (deftest
    freshen-term-preserves-dotted-lists-and-two-argument-calls
    ()
    (let* ((variable (fresh-logic-variable "?DOTTED"))
           (term (cons variable :tail))
           (copy
             (cl-prolog-kit::%freshen-term
               term
               (make-hash-table :test (function eq)))))
      (is (not (eq copy term)))
      (is (logic-var-p (car copy)))
      (is (not (eq variable (car copy))))
      (is (eq :tail (cdr copy)))
      (is (eq variable (car term)))
      (is (eq :tail (cdr term)))))
  (deftest
    freshen-term-preserves-proper-list-variable-identity
    ()
    (let* ((variable (fresh-logic-variable "?PROPER"))
           (term (list :pair variable variable))
           (copy
             (cl-prolog-kit::%freshen-term
               term
               (make-hash-table :test (function eq)))))
      (is (not (eq copy term)))
      (is (logic-var-p (second copy)))
      (is (eq (second copy) (third copy)))
      (is (not (eq variable (second copy))))
      (is (eq variable (second term)))
      (is (eq variable (third term)))))
  (deftest
    freshen-term-honors-prebound-variable-in-cyclic-graph
    ()
    (let ((source-variable (fresh-logic-variable "?SOURCE"))
          (bound-variable (fresh-logic-variable "?BOUND"))
          (cycle (cons nil nil))
          (table (make-hash-table :test (function eq))))
      (setf (car cycle) source-variable
            (cdr cycle) cycle
            (gethash source-variable table) bound-variable)
      (let ((copy (cl-prolog-kit::%freshen-term cycle table)))
        (is (not (eq copy cycle)))
        (is (eq copy (cdr copy)))
        (is (eq bound-variable (car copy)))
        (is (eq source-variable (car cycle)))
        (is (eq cycle (cdr cycle))))))
  (deftest
    freshening-map-upgrades-to-eq-hash-table
    ()
    (let* ((mapping (cl-prolog-kit::%make-freshening-map))
           (keys
             (loop repeat (1+ cl-prolog-kit::+freshening-map-threshold+)
                   collect (cons nil nil))))
      (loop for key in keys
            for value from 0
            do (cl-prolog-kit::%freshening-map-insert key value mapping))
      (let ((table (cl-prolog-kit::%freshening-map-table mapping)))
        (is (hash-table-p table))
        (is (eq (hash-table-test table) (quote eq)))
        (is (null (cl-prolog-kit::%freshening-map-entries mapping)))
        (loop for key in keys
              for expected from 0
              do (multiple-value-bind (actual present-p)
                     (cl-prolog-kit::%freshening-map-lookup key mapping)
                   (is present-p)
                   (is (= expected actual)))))))
  (deftest
    freshen-clause-preserves-shared-cons-and-variable-identity
    ()
    (let* ((variable (fresh-logic-variable "?SHARED"))
           (shared (cons variable :shared-tail))
           (head (list (quote head) shared variable))
           (body-goal (list (quote body) shared variable))
           (body (list body-goal))
           (clause (make-clause head body))
           (fresh-clause (cl-prolog-kit::%freshen-clause clause))
           (fresh-head (clause-head fresh-clause))
           (fresh-goal (first (clause-body fresh-clause)))
           (fresh-shared (second fresh-head))
           (fresh-variable (third fresh-head)))
      (is (not (eq fresh-clause clause)))
      (is (not (eq fresh-shared shared)))
      (is (eq fresh-shared (second fresh-goal)))
      (is (eq fresh-variable (third fresh-goal)))
      (is (eq fresh-variable (car fresh-shared)))
      (is (not (eq fresh-variable variable)))
      (is (eq :shared-tail (cdr fresh-shared)))
      (is (eq head (clause-head clause)))
      (is (eq body (clause-body clause)))
      (is (eq shared (second head)))
      (is (eq shared (second body-goal)))
      (is (eq variable (third head)))
      (is (eq variable (car shared)))
      (is (eq :shared-tail (cdr shared)))))
  (deftest
    freshen-clause-isolates-attempts
    ()
    (let* ((variable (fresh-logic-variable "?ATTEMPT"))
           (shared (cons variable :shared-tail))
           (clause
             (make-clause
               (list (quote head) shared variable)
               (list (list (quote body) shared variable))))
           (first-clause (cl-prolog-kit::%freshen-clause clause))
           (second-clause (cl-prolog-kit::%freshen-clause clause))
           (first-head (clause-head first-clause))
           (second-head (clause-head second-clause))
           (first-goal (first (clause-body first-clause)))
           (second-goal (first (clause-body second-clause))))
      (is (eq (second first-head) (second first-goal)))
      (is (eq (third first-head) (third first-goal)))
      (is (eq (third first-head) (car (second first-head))))
      (is (eq (second second-head) (second second-goal)))
      (is (eq (third second-head) (third second-goal)))
      (is (eq (third second-head) (car (second second-head))))
      (is (not (eq (second first-head) (second second-head))))
      (is (not (eq (third first-head) (third second-head))))))

(deftest unification-scratch-promotes-at-linear-threshold () (let* ((primary-threshold cl-prolog-kit::+unification-pair-primary-threshold+) (threshold cl-prolog-kit::+unification-pair-linear-threshold+) (scratch (cl-prolog-kit::%make-unification-scratch)) (pairs (loop for index to threshold collect (cons (list :left index) (list :right index))))) (is (= 8 primary-threshold)) (is (= 16 threshold)) (is (null (cl-prolog-kit::%unification-scratch-pair-seen scratch))) (is (null (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch))) (loop for pair in (subseq pairs 0 primary-threshold) do (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch (car pair) (cdr pair))))) (is (= primary-threshold (cl-prolog-kit::%unification-scratch-pair-count scratch))) (is (null (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch))) (is (cl-prolog-kit::%unification-scratch-remember-pair scratch (caar pairs) (cdar pairs))) (let ((ninth (nth primary-threshold pairs))) (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch (car ninth) (cdr ninth))))) (is (= (1+ primary-threshold) (cl-prolog-kit::%unification-scratch-pair-count scratch))) (is (simple-vector-p (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch))) (is (cl-prolog-kit::%unification-scratch-remember-pair scratch (caar pairs) (cdar pairs))) (let ((ninth (nth primary-threshold pairs))) (is (cl-prolog-kit::%unification-scratch-remember-pair scratch (car ninth) (cdr ninth)))) (loop for pair in (subseq pairs (1+ primary-threshold) threshold) do (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch (car pair) (cdr pair))))) (is (= threshold (cl-prolog-kit::%unification-scratch-pair-count scratch))) (is (not (cl-prolog-kit::%unification-scratch-pair-hash-mode-p scratch))) (let ((seventeenth (nth threshold pairs))) (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch (car seventeenth) (cdr seventeenth))))) (is (= threshold (cl-prolog-kit::%unification-scratch-pair-count scratch))) (is (cl-prolog-kit::%unification-scratch-pair-hash-mode-p scratch)) (is (hash-table-p (cl-prolog-kit::%unification-scratch-first-index scratch))) (is (loop for pair in pairs always (cl-prolog-kit::%unification-scratch-remember-pair scratch (car pair) (cdr pair)))) (let ((primary (cl-prolog-kit::%unification-scratch-pair-seen scratch)) (secondary (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch))) (cl-prolog-kit::%reset-unification-scratch scratch) (is (zerop (cl-prolog-kit::%unification-scratch-pair-count scratch))) (is (not (cl-prolog-kit::%unification-scratch-pair-hash-mode-p scratch))) (is (loop for value across primary always (null value))) (is (loop for value across secondary always (null value))) (is (zerop (hash-table-count (cl-prolog-kit::%unification-scratch-first-index scratch)))) (loop for pair in (subseq pairs 0 (1+ primary-threshold)) do (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch (car pair) (cdr pair))))) (is (eq secondary (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch))) (is (not (cl-prolog-kit::%unification-scratch-pair-hash-mode-p scratch))) (cl-prolog-kit::%reset-unification-scratch scratch) (is (loop for value across primary always (null value))) (is (loop for value across secondary always (null value))))))
  (deftest unification-scratch-keeps-same-left-distinct-rights () (let ((scratch (cl-prolog-kit::%make-unification-scratch)) (left (list :left)) (rights (loop repeat 17 collect (list :right)))) (loop for right in rights do (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch left right)))) (is (loop for right in rights always (cl-prolog-kit::%unification-scratch-remember-pair scratch left right))) (is (cl-prolog-kit::%unification-scratch-pair-hash-mode-p scratch)) (is (= cl-prolog-kit::+unification-pair-linear-threshold+ (cl-prolog-kit::%unification-scratch-pair-count scratch))) (cl-prolog-kit::%reset-unification-scratch scratch)))
  (deftest unification-scratch-collision-list-recognizes-a-repeat ()
    "Once the pair memo is hashed, a left term seen with several rights keeps
the extra rights in a short list before it is worth a hash table.  A repeat has
to be recognized in that list shape too, or a shared sub-DAG would be walked
again instead of being short-circuited."
    (let* ((scratch (cl-prolog-kit::%make-unification-scratch))
           (left (list :left))
           (rights (loop repeat 3 collect (list :right))))
      (dolist (right rights)
        (is (not (cl-prolog-kit::%unification-scratch-remember-pair-hashed
                   scratch left right))))
      (let ((extras (gethash left
                             (cl-prolog-kit::%unification-scratch-collision-index
                               scratch))))
        (is (listp extras))
        (is (not (hash-table-p extras))))
      (is (loop for right in rights
                always (cl-prolog-kit::%unification-scratch-remember-pair-hashed
                         scratch left right)))
      (cl-prolog-kit::%reset-unification-scratch scratch)
      (is (zerop (hash-table-count
                   (cl-prolog-kit::%unification-scratch-collision-index scratch))))
      (is (not (cl-prolog-kit::%unification-scratch-remember-pair-hashed
                 scratch left (first rights))))))

  (deftest unification-scratch-keeps-directed-pairs ()(let* ((scratch (cl-prolog-kit::%make-unification-scratch)) (left (cons :left nil)) (right (cons :right nil))) (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch left right))) (is (cl-prolog-kit::%unification-scratch-remember-pair scratch left right)) (is (not (cl-prolog-kit::%unification-scratch-remember-pair scratch right left))) (is (cl-prolog-kit::%unification-scratch-remember-pair scratch right left)) (cl-prolog-kit::%reset-unification-scratch scratch)))

  (deftest
    cyclic-unification-scratch-success-and-failure
    ()
    (let ((left (cons :node nil))
          (right (cons :node nil))
          (different (cons :different nil)))
      (setf (cdr left) left
            (cdr right) right
            (cdr different) different)
      (is (nth-value 1 (unify left right)))
      (is (not (nth-value 1 (unify left different))))))
  (deftest
    shared-dag-unification-scratch-success-and-failure
    ()
    (let* ((left-leaf (list :leaf))
           (right-leaf (list :leaf))
           (different-leaf (list :different))
           (left (list left-leaf left-leaf))
           (right (list right-leaf right-leaf))
           (different (list different-leaf different-leaf)))
      (is (nth-value 1 (unify left right)))
      (is (not (nth-value 1 (unify left different))))))
  (defun make-nested-term (depth)
    (loop with term = :leaf
          repeat depth
          do (setf term (list term))
          finally (return term)))

  (deftest unification-scratch-cleans-up-after-condition-and-nonlocal-exit ()
    (let ((scratch (cl-prolog-kit::%make-unification-scratch)))
      (handler-case
          (cl-prolog-kit::%call-with-unification-scratch
            scratch
            (lambda ()
              (is (cl-prolog-kit::%unification-scratch-active-p scratch))
              (dotimes (index 9)
                (is (not (cl-prolog-kit::%unification-scratch-remember-pair
                           scratch (list :left index) (list :right index)))))
              (is (simple-vector-p
                    (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch)))
              (error "forced exit")))
        (error () nil))
      (is (not (cl-prolog-kit::%unification-scratch-active-p scratch)))
      (is (zerop (cl-prolog-kit::%unification-scratch-pair-count scratch)))
      (is (not (cl-prolog-kit::%unification-scratch-pair-hash-mode-p scratch)))
      (let ((primary (cl-prolog-kit::%unification-scratch-pair-seen scratch))
            (secondary (cl-prolog-kit::%unification-scratch-pair-seen-secondary scratch))
            (index (cl-prolog-kit::%unification-scratch-first-index scratch)))
        (is (loop for value across primary always (null value)))
        (is (loop for value across secondary always (null value)))
        (is (or (null index) (zerop (hash-table-count index)))))
      (is (not (cl-prolog-kit::%unification-scratch-remember-pair
                 scratch (list :marker-left) (list :marker-right))))))
  (deftest nested-query-roots-use-independent-unification-scratches ()
  (let ((outer-scratch nil)
        (inner-scratch nil))
    (map-prolog-solutions
     (lambda (outer-solution)
       (declare (ignore outer-solution))
       (setf outer-scratch cl-prolog-kit::*unification-scratch*)
       (map-prolog-solutions
        (lambda (inner-solution)
          (declare (ignore inner-solution))
          (setf inner-scratch cl-prolog-kit::*unification-scratch*)
          (is (not (eq outer-scratch inner-scratch))))
        (make-rulebase)
        (list (quote true)))
       (is (eq outer-scratch cl-prolog-kit::*unification-scratch*)))
     (make-rulebase)
     (list (quote true)))
    (is outer-scratch)
    (is inner-scratch)
    (is (not (eq outer-scratch inner-scratch)))
    (dolist (scratch (list outer-scratch inner-scratch))
      (is (not (cl-prolog-kit::%unification-scratch-active-p scratch)))
      (let ((index (cl-prolog-kit::%unification-scratch-first-index scratch)))
        (is (or (null index) (zerop (hash-table-count index))))))))
  (deftest active-unification-scratch-uses-temporary-reentrant-scratch ()
  (let ((scratch (cl-prolog-kit::%make-unification-scratch))
        (marker-left (quote (marker-left)))
        (marker-right (quote (marker-right))))
    (is (not (cl-prolog-kit::%unification-scratch-remember-pair
               scratch marker-left marker-right)))
    (setf (cl-prolog-kit::%unification-scratch-active-p scratch) t)
    (let ((cl-prolog-kit::*unification-scratch* scratch))
      (multiple-value-bind (bindings unified-p index)
          (cl-prolog-kit::%unify-indexed
            (list (make-nested-term 24))
            (list (make-nested-term 24))
            nil
            (cl-prolog-kit::%make-environment-index nil))
        (declare (ignore index))
        (is unified-p)
        (is (null bindings))))
    (is (cl-prolog-kit::%unification-scratch-active-p scratch))
    (is (cl-prolog-kit::%unification-scratch-remember-pair
          scratch marker-left marker-right))
    (setf (cl-prolog-kit::%unification-scratch-active-p scratch) nil)
    (cl-prolog-kit::%reset-unification-scratch scratch)
    (is (not (cl-prolog-kit::%unification-scratch-remember-pair
               scratch marker-left marker-right)))))
  (deftest active-grown-unification-scratch-preserves-parent-state ()
  (let ((scratch (cl-prolog-kit::%make-unification-scratch))
        (lefts (loop repeat 33 collect (list (quote left))))
        (rights (loop repeat 33 collect (list (quote right)))))
    (loop for left in lefts
          for right in rights
          do (is (not (cl-prolog-kit::%unification-scratch-remember-pair
                        scratch left right))))
    (setf (cl-prolog-kit::%unification-scratch-active-p scratch) t)
    (let ((cl-prolog-kit::*unification-scratch* scratch))
      (multiple-value-bind (bindings unified-p index)
          (cl-prolog-kit::%unify-indexed
            (list (make-nested-term 24))
            (list (make-nested-term 24))
            nil
            (cl-prolog-kit::%make-environment-index nil))
        (declare (ignore index))
        (is unified-p)
        (is (null bindings))))
    (is (cl-prolog-kit::%unification-scratch-active-p scratch))
    (loop for left in lefts
          for right in rights
          do (is (cl-prolog-kit::%unification-scratch-remember-pair
                   scratch left right)))
    (setf (cl-prolog-kit::%unification-scratch-active-p scratch) nil)
    (cl-prolog-kit::%reset-unification-scratch scratch)))
  #+sb-thread
(deftest concurrent-query-roots-use-thread-local-unification-scratches ()
  (let* ((thread-count 4)
         (ready (sb-thread:make-semaphore :count 0))
         (release (sb-thread:make-semaphore :count 0))
         (results (make-array thread-count :initial-element nil))
         (threads
           (loop for index below thread-count collect
                 (let ((slot index))
                   (sb-thread:make-thread
                    (lambda ()
                      (let ((observed nil)
                            (consistent-p t))
                        (map-prolog-solutions
                         (lambda (solution)
                           (declare (ignore solution))
                           (setf observed cl-prolog-kit::*unification-scratch*)
                           (sb-thread:signal-semaphore ready)
                           (sb-thread:wait-on-semaphore release)
                           (setf consistent-p
                                 (eq observed cl-prolog-kit::*unification-scratch*)))
                         (make-rulebase)
                         '(true))
                        (setf (aref results slot)
                              (list observed
                                    consistent-p
                                    (not
                                     (cl-prolog-kit::%unification-scratch-active-p
                                      observed))
                                    (let ((index (cl-prolog-kit::%unification-scratch-first-index observed))) (or (null index) (zerop (hash-table-count index)))))))))))))
    (is (sb-thread:wait-on-semaphore ready :n thread-count :timeout 5))
    (sb-thread:signal-semaphore release thread-count)
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (let ((scratches
            (loop for result across results collect (first result))))
      (is (every #'identity scratches))
      (is (= thread-count
             (length (remove-duplicates scratches :test #'eq))))
      (loop for result across results
            do (is (second result))
               (is (third result))
               (is (fourth result))))))
  #-sb-thread
  (deftest concurrent-query-roots-are-skipped-without-sb-thread ()
    (is (not (member :sb-thread *features*))))
  (deftest
    deep-cyclic-unification-crosses-scratch-inline-capacity
    ()
    (labels ((make-cycle (different-index)
               (let ((nodes (make-array 40)))
            (dotimes (index 40)
              (setf (aref nodes index) (cons
                  (if (eql index different-index) :different
                    :node)
                  nil)))
            (dotimes (index 40)
              (setf (cdr (aref nodes index)) (aref nodes (mod (1+ index) 40))))
            (aref nodes 0))))
      (let ((left (make-cycle nil))
            (matching (make-cycle nil))
            (different (make-cycle 39)))
        (is (nth-value 1 (unify left matching)))
        (is (not (nth-value 1 (unify left different))))))) (deftest compiled-clause-template-preserves-alias-dag-cycle-and-improper-terms () (let* ((variable (fresh-logic-variable)) (shared (cons variable nil)) (root (cons shared shared)) (dotted (cons variable :tail)) (clause (make-clause (list (quote template-graph) variable variable root dotted) (list (list (quote template-body) variable root))))) (setf (cdr shared) shared) (let* ((template (cl-prolog-kit::%compile-clause-template clause)) (first (cl-prolog-kit::%materialize-clause-template template)) (second (cl-prolog-kit::%materialize-clause-template template)) (first-head (clause-head first)) (second-head (clause-head second)) (first-root (fourth first-head)) (second-root (fourth second-head)) (first-shared (car first-root)) (second-shared (car second-root)) (first-body-goal (first (clause-body first))) (second-body-goal (first (clause-body second)))) (is (eq (second first-head) (third first-head))) (is (eq (second first-head) (car first-shared))) (is (eq (car first-root) (cdr first-root))) (is (eq first-shared (cdr first-shared))) (is (eq (second first-head) (car (fifth first-head)))) (is (eq :tail (cdr (fifth first-head)))) (is (eq (second first-head) (second first-body-goal))) (is (eq first-root (third first-body-goal))) (is (eq (second second-head) (third second-head))) (is (eq (second second-head) (car second-shared))) (is (eq (car second-root) (cdr second-root))) (is (eq second-shared (cdr second-shared))) (is (eq (second second-head) (second second-body-goal))) (is (eq second-root (third second-body-goal))) (is (not (eq (second first-head) (second second-head)))) (is (not (eq first-root second-root))) (is (not (eq first-shared second-shared))) (is (not (eq (fifth first-head) (fifth second-head))))))) (deftest stored-clause-template-isolates-input-public-views-and-proof-iterations () (let* ((payload (list :original)) (input (make-clause (list (quote stored-template) payload))) (rulebase (make-rulebase :clauses (list input)))) (setf (car payload) :input-mutated) (let* ((first-view (first (rulebase-visible-clauses rulebase))) (view-payload (second (clause-head first-view)))) (setf (car view-payload) :view-mutated)) (is (= 1 (length (query-prolog rulebase (quote (stored-template (:original))))))) (is (= 1 (length (query-prolog rulebase (quote (stored-template (:original))))))) (is (null (query-prolog rulebase (quote (stored-template (:input-mutated)))))) (is (null (query-prolog rulebase (quote (stored-template (:view-mutated)))))) (let* ((first-view (first (rulebase-visible-clauses rulebase))) (second-view (first (rulebase-visible-clauses rulebase))) (first-head (clause-head first-view)) (second-head (clause-head second-view))) (is (not (eq first-view second-view))) (is (not (eq first-head second-head))) (is (not (eq (second first-head) (second second-head)))) (is-equal (quote (:original)) (second first-head)) (is-equal (quote (:original)) (second second-head)))) (let* ((variable (fresh-logic-variable)) (alias-rulebase (make-rulebase :clauses (list (make-clause (list (quote template-alias) variable variable)))))) (is (= 1 (length (query-prolog alias-rulebase (quote (template-alias alpha alpha)))))) (is (null (query-prolog alias-rulebase (quote (template-alias alpha beta))))) (is (= 1 (length (query-prolog alias-rulebase (quote (template-alias beta beta))))))))
(deftest occurs-scratch-promotes-after-inline-threshold ()
  (let* ((scratch (cl-prolog-kit::%make-unification-scratch))
         (nodes (loop repeat 9 collect (cons :node nil))))
    (is (null (cl-prolog-kit::%unification-scratch-occurs-seen scratch)))
    (cl-prolog-kit::%clear-occurs-scratch scratch)
    (is (null (cl-prolog-kit::%unification-scratch-occurs-seen scratch)))
    (loop for node in (subseq nodes 0 8)
          do (is (not (cl-prolog-kit::%occurs-scratch-remember-p scratch node))))
    (is (simple-vector-p
          (cl-prolog-kit::%unification-scratch-occurs-seen scratch)))
    (is (null (cl-prolog-kit::%unification-scratch-occurs-index scratch)))
    (is (not (cl-prolog-kit::%occurs-scratch-remember-p scratch (ninth nodes))))
    (is (hash-table-p
          (cl-prolog-kit::%unification-scratch-occurs-index scratch)))
    (is (cl-prolog-kit::%occurs-scratch-remember-p scratch (first nodes)))
    (cl-prolog-kit::%clear-occurs-scratch scratch)
    (is (zerop (cl-prolog-kit::%unification-scratch-occurs-count scratch)))
    (let ((seen (cl-prolog-kit::%unification-scratch-occurs-seen scratch)))
      (is (or (null seen) (every (function null) seen))))
    (is (zerop
          (hash-table-count
            (cl-prolog-kit::%unification-scratch-occurs-index scratch))))))
  (deftest occurs-scratch-traversal-semantics-and-cleanup ()
  (let ((scratch (cl-prolog-kit::%make-unification-scratch))
        (index (cl-prolog-kit::%make-environment-index nil))
        (variable (fresh-logic-variable "?OCCURS-SCRATCH")))
    (labels ((clean-p ()
               (let ((seen
                       (cl-prolog-kit::%unification-scratch-occurs-seen scratch))
                     (seen-index
                       (cl-prolog-kit::%unification-scratch-occurs-index scratch)))
                 (and
                   (zerop
                     (cl-prolog-kit::%unification-scratch-occurs-count scratch))
                   (or (null seen) (every (function null) seen))
                   (or (null seen-index)
                       (zerop (hash-table-count seen-index)))))))
      (is (not (cl-prolog-kit::%occurs-p-indexed variable :atomic index scratch)))
      (is (null (cl-prolog-kit::%unification-scratch-occurs-seen scratch)))
      (is (clean-p))
      (let ((cl-prolog-kit::*unification-scratch* scratch))
        (is (nth-value 1 (unify variable (list variable) nil nil))))
      (is (null (cl-prolog-kit::%unification-scratch-occurs-seen scratch)))
      (is (clean-p))
      (is (not (cl-prolog-kit::%occurs-p-indexed
                 variable (list :small :term) index scratch)))
      (is (clean-p))
      (is (not (cl-prolog-kit::%occurs-p-indexed
                 variable (loop repeat 20 collect :large) index scratch)))
      (is (hash-table-p
            (cl-prolog-kit::%unification-scratch-occurs-index scratch)))
      (is (clean-p))
      (let* ((shared (list :shared :leaf))
             (dag (list shared shared)))
        (is (not (cl-prolog-kit::%occurs-p-indexed variable dag index scratch)))
        (is (clean-p)))
      (let ((cycle (cons :cycle nil)))
        (setf (cdr cycle) cycle)
        (is (not (cl-prolog-kit::%occurs-p-indexed variable cycle index scratch)))
        (is (clean-p)))
      (is (cl-prolog-kit::%occurs-p-indexed
            variable (list :contains variable) index scratch))
      (is (clean-p)))))
