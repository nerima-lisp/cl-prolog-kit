;;;; Predicate-index maintenance and call/rule-resolution depth-limit tests.

(in-package #:cl-prolog-kit.tests)

(defun stored-clause-heads (entries)
  "Return the clause head of each stored-clause entry in ENTRIES, in order."
  (mapcar (lambda (entry)
            (clause-head (cl-prolog-kit::%stored-clause-clause entry)))
          entries))

(deftest predicate-index-excludes-unrelated-clauses-and-preserves-order ()
  (let* ((rulebase (make-rulebase))
         (index (cl-prolog-kit::rulebase-predicate-index rulebase))
         (tails (cl-prolog-kit::rulebase-predicate-tails rulebase))
         (key (list cl-prolog-kit::+default-prolog-module+ 'indexed 1)))
    (is (null (cl-prolog-kit::rulebase-entries rulebase)))
    (is (null (cl-prolog-kit::rulebase-entries-tail rulebase)))
    (is-equal 0 (hash-table-count index))
    (is-equal 0 (hash-table-count tails))
    (rulebase-insert-clause! rulebase (make-clause '(indexed first))
                             :position :first)
    (is (eq (cl-prolog-kit::rulebase-entries rulebase)
            (cl-prolog-kit::rulebase-entries-tail rulebase)))
    (is (eq (gethash key index) (gethash key tails)))
    (let ((first-indexed-tail (gethash key tails)))
      (rulebase-insert-clause! rulebase (make-clause '(other between)))
      (is (eq first-indexed-tail (gethash key tails))))
    (rulebase-insert-clause! rulebase (make-clause '(indexed second)))
    (let ((global-tail (cl-prolog-kit::rulebase-entries-tail rulebase))
          (indexed-tail (gethash key tails)))
      (rulebase-insert-clause! rulebase (make-clause '(indexed zeroth))
                               :position :first)
      (is (eq global-tail (cl-prolog-kit::rulebase-entries-tail rulebase)))
      (is (eq indexed-tail (gethash key tails))))
    (is-equal '((indexed zeroth)
                (indexed first)
                (other between)
                (indexed second))
              (stored-clause-heads (cl-prolog-kit::rulebase-entries rulebase)))
    (multiple-value-bind (revision entries)
        (cl-prolog-kit::%rulebase-predicate-entries
         rulebase cl-prolog-kit::+default-prolog-module+ 'indexed 1)
      (declare (cl:ignore revision))
      (is-equal '((indexed zeroth) (indexed first) (indexed second))
                (stored-clause-heads entries)))
    (is (eq (last (cl-prolog-kit::rulebase-entries rulebase))
            (cl-prolog-kit::rulebase-entries-tail rulebase)))
    (is (loop for predicate-key being the hash-keys of index
                using (hash-value entries)
              always
              (and (eq (last entries) (gethash predicate-key tails))
                   (equal entries
                          (remove-if-not
                           (lambda (entry)
                             (equal predicate-key
                                    (cl-prolog-kit::%stored-clause-predicate-key
                                     entry)))
                           (cl-prolog-kit::rulebase-entries rulebase))))))))

(deftest predicate-index-keeps-logical-update-history ()
  (let* ((rulebase (make-rulebase))
         (key (list cl-prolog-kit::+default-prolog-module+ 'indexed 1)))
    (assert-query rulebase (assertz (indexed first)) :succeeds)
    (assert-query rulebase (assertz (indexed second)) :succeeds)
    (let ((snapshot (cl-prolog-kit::rulebase-revision rulebase)))
      (assert-query rulebase (asserta (indexed zeroth)) :succeeds)
      (assert-query rulebase (retract (indexed first)) :succeeds)
      (is-equal '(((?x . zeroth)) ((?x . second)))
                (query-prolog rulebase '(indexed ?x)))
      (assert-query rulebase (assertz (indexed third)) :succeeds)
      (is-equal '(((?x . zeroth)) ((?x . second)) ((?x . third)))
                (query-prolog rulebase '(indexed ?x)))
      (let ((entries
              (gethash key
                       (cl-prolog-kit::rulebase-predicate-index rulebase))))
        (is-equal '((indexed zeroth)
                    (indexed first)
                    (indexed second)
                    (indexed third))
                  (stored-clause-heads entries))
        (is (eq (last entries)
                (gethash key
                         (cl-prolog-kit::rulebase-predicate-tails rulebase)))))
      (is-equal '((indexed first) (indexed second))
                (stored-clause-heads
                 (cl-prolog-kit::%rulebase-predicate-entries-at-revision
                  rulebase cl-prolog-kit::+default-prolog-module+
                  'indexed 1 snapshot)))
      (assert-query rulebase (abolish (/ indexed 1)) :succeeds)
      (is-equal '()
                (cl-prolog-kit::%rulebase-predicate-entries-at-revision
                 rulebase cl-prolog-kit::+default-prolog-module+ 'indexed 1
                 (cl-prolog-kit::rulebase-revision rulebase)))
      (is-equal '((indexed first) (indexed second))
                (stored-clause-heads
                 (cl-prolog-kit::%rulebase-predicate-entries-at-revision
                  rulebase cl-prolog-kit::+default-prolog-module+
                  'indexed 1 snapshot))))))

(deftest predicate-visible-check-respects-logical-update-history ()
  (let* ((rulebase (make-rulebase))
         (module cl-prolog-kit::+default-prolog-module+)
         (predicate 'visible-indexed)
         (arity 1)
         (key (list module predicate arity)))
    (rulebase-insert-clause!
     rulebase (make-clause '(visible-indexed dead-first)))
    (rulebase-insert-clause!
     rulebase (make-clause '(visible-indexed live-later)))
    (let* ((entries (gethash key
                             (cl-prolog-kit::rulebase-predicate-index rulebase)))
           (dead-first (first entries))
           (live-later (second entries)))
      (is (cl-prolog-kit::%rulebase-retract-entry! rulebase dead-first))
      (is (cl-prolog-kit::%rulebase-predicate-visible-p
           rulebase module predicate arity
           (cl-prolog-kit::rulebase-revision rulebase)))
      (is (cl-prolog-kit::%rulebase-retract-entry! rulebase live-later))
      (let ((all-dead-revision (cl-prolog-kit::rulebase-revision rulebase)))
        (is (not (cl-prolog-kit::%rulebase-predicate-visible-p
                  rulebase module predicate arity all-dead-revision)))
        (rulebase-insert-clause!
         rulebase (make-clause '(visible-indexed born-after-snapshot)))
        (is (not (cl-prolog-kit::%rulebase-predicate-visible-p
                  rulebase module predicate arity all-dead-revision)))
        (is (cl-prolog-kit::%rulebase-predicate-visible-p
             rulebase module predicate arity
             (cl-prolog-kit::rulebase-revision rulebase)))))))
 (deftest predicate-index-isolates-modules ()
  (let ((rulebase (make-rulebase)))
    (rulebase-insert-clause! rulebase (make-clause '(indexed alpha))
                             :module 'alpha)
    (rulebase-insert-clause! rulebase (make-clause '(indexed beta))
                             :module 'beta)
    (is-equal '((indexed alpha))
              (stored-clause-heads
               (cl-prolog-kit::%rulebase-predicate-entries-at-revision
                rulebase 'alpha 'indexed 1
                (cl-prolog-kit::rulebase-revision rulebase))))
    (is-equal '((indexed beta))
              (stored-clause-heads
               (cl-prolog-kit::%rulebase-predicate-entries-at-revision
                rulebase 'beta 'indexed 1
                (cl-prolog-kit::rulebase-revision rulebase))))))


(deftest predicate-index-copy-is-independent ()
  (let* ((rulebase (prolog ((indexed original))))
         (copy (cl-prolog-kit::%copy-rulebase rulebase))
         (key (list cl-prolog-kit::+default-prolog-module+ 'indexed 1)))
    (is (not (eq (cl-prolog-kit::rulebase-entries rulebase)
                 (cl-prolog-kit::rulebase-entries copy))))
    (is (not (eq (cl-prolog-kit::rulebase-entries-tail rulebase)
                 (cl-prolog-kit::rulebase-entries-tail copy))))
    (is (not (eq (cl-prolog-kit::rulebase-predicate-index rulebase)
                 (cl-prolog-kit::rulebase-predicate-index copy))))
    (is (not (eq (gethash key
                          (cl-prolog-kit::rulebase-predicate-index rulebase))
                 (gethash key
                          (cl-prolog-kit::rulebase-predicate-index copy)))))
    (is (not (eq (gethash key
                          (cl-prolog-kit::rulebase-predicate-tails rulebase))
                 (gethash key
                          (cl-prolog-kit::rulebase-predicate-tails copy)))))
    (rulebase-insert-clause! copy (make-clause '(indexed copied)))
    (is-equal '((indexed original))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog-kit::%rulebase-predicate-entries
                   rulebase cl-prolog-kit::+default-prolog-module+
                   'indexed 1))))
    (is-equal '((indexed original) (indexed copied))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog-kit::%rulebase-predicate-entries
                   copy cl-prolog-kit::+default-prolog-module+
                   'indexed 1))))
    (rulebase-insert-clause! rulebase
                             (make-clause '(indexed original-added)))
    (is-equal '((indexed original) (indexed original-added))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog-kit::%rulebase-predicate-entries
                   rulebase cl-prolog-kit::+default-prolog-module+
                   'indexed 1))))
    (is-equal '((indexed original) (indexed copied))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog-kit::%rulebase-predicate-entries
                   copy cl-prolog-kit::+default-prolog-module+
                   'indexed 1))))
    (is (eq (last (cl-prolog-kit::rulebase-entries rulebase))
            (cl-prolog-kit::rulebase-entries-tail rulebase)))
    (is (eq (last (cl-prolog-kit::rulebase-entries copy))
            (cl-prolog-kit::rulebase-entries-tail copy)))))

(deftest predicate-index-replace-reflects-transaction ()
  (let* ((rulebase (prolog ((indexed original))))
         (transaction (cl-prolog-kit::%copy-rulebase rulebase))
         (key (list cl-prolog-kit::+default-prolog-module+ 'indexed 1)))
    (rulebase-insert-clause! transaction (make-clause '(indexed committed)))
    (cl-prolog-kit::%replace-rulebase! rulebase transaction)
    (is-equal '((indexed original) (indexed committed))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog-kit::%rulebase-predicate-entries
                   rulebase cl-prolog-kit::+default-prolog-module+
                   'indexed 1))))
    (let ((discarded (cl-prolog-kit::%copy-rulebase rulebase)))
      (rulebase-insert-clause! discarded
                               (make-clause '(indexed rolled-back))))
    (rulebase-insert-clause! rulebase
                             (make-clause '(indexed after-rollback)))
    (is-equal '((indexed original)
                (indexed committed)
                (indexed after-rollback))
              (stored-clause-heads
               (nth-value
                1 (cl-prolog-kit::%rulebase-predicate-entries
                   rulebase cl-prolog-kit::+default-prolog-module+
                   'indexed 1))))
    (is (eq (last (cl-prolog-kit::rulebase-entries rulebase))
            (cl-prolog-kit::rulebase-entries-tail rulebase)))
    (is (eq (last (gethash key
                           (cl-prolog-kit::rulebase-predicate-index rulebase)))
            (gethash key
                     (cl-prolog-kit::rulebase-predicate-tails rulebase))))))

(deftest predicate-index-proof-loop-preserves-arity-and-solution-order ()
  (let ((rulebase
          (prolog
           ((dispatch alpha))
           ((dispatch ?value) (helper ?value))
           ((dispatch beta))
           ((dispatch alpha extra))
           ((helper gamma)))))
    (is-equal (quote (nil))
              (query-prolog rulebase (quote (dispatch alpha))))
    (is-equal (quote (((?value . alpha))
                      ((?value . gamma))
                      ((?value . beta))))
              (query-prolog rulebase (quote (dispatch ?value))))
    (is-equal (quote (nil))
              (query-prolog rulebase (quote (dispatch alpha extra))))))

(deftest predicate-index-bound-cyclic-root-keeps-full-snapshot-order ()
  (let* ((cycle (list (quote root)))
         (environment (list (cons (quote ?value) cycle)))
         (rulebase
           (prolog
            ((cyclic-index first))
            ((cyclic-index ?item))
            ((cyclic-index last))))
         (session (cl-prolog-kit::%make-rulebase-table-session rulebase))
         (state
           (cl-prolog-kit::%make-proof-state
            rulebase
            environment
            (cl-prolog-kit::%make-environment-index environment)
            nil
            cl-prolog-kit::+default-prolog-module+
            session
            (cl-prolog-kit::%make-cut-tag)))
         (descriptor
           (cl-prolog-kit::%rulebase-predicate-descriptor
            rulebase cl-prolog-kit::+default-prolog-module+
            (quote cyclic-index) 1))
         (entries (cl-prolog-kit::%predicate-descriptor-entries descriptor)))
    (setf (cdr cycle) cycle)
    (let ((snapshot
            (cl-prolog-kit::%proof-predicate-entries
             (quote (cyclic-index ?value)) state)))
      (is (eq entries snapshot))
      (is-equal
       (quote ((cyclic-index first)
               (cyclic-index ?item)
               (cyclic-index last)))
       (stored-clause-heads snapshot))
      (setf (car cycle) (quote changed))
      (is (eq snapshot
              (cl-prolog-kit::%proof-predicate-entries
               (quote (cyclic-index ?value)) state))))))

  (deftest tabled-cyclic-goal-retains-cyclic-answer ()
    (let* ((cycle (list (quote root)))
           (environment (list (cons (quote ?value) cycle)))
           (rulebase (prolog ((tabled-cycle ?item)))))
      (setf (cdr cycle) cycle)
      (cl-prolog-kit::%add-rulebase-table-declaration!
       rulebase (quote tabled-cycle) 1 :test)
      (let ((answers
              (query-prolog rulebase (quote (tabled-cycle ?value))
                            :environment environment)))
        (is (= 1 (length answers)))
        (let ((resolved
                (cl-prolog-kit:logic-substitute (quote ?value) (first answers))))
          (is (consp resolved))
          (is (eq resolved (cdr resolved)))))))

  (deftest predicate-index-proof-cache-follows-rulebase-revisions ()
    (let* ((rulebase (prolog ((indexed original))))
           (session (cl-prolog-kit::%make-rulebase-table-session rulebase))
           (state
             (cl-prolog-kit::%make-proof-state
              rulebase
              (quote ())
              (cl-prolog-kit::%make-environment-index (quote ()))
              nil
              cl-prolog-kit::+default-prolog-module+
              session
              (cl-prolog-kit::%make-cut-tag)))
           (first-snapshot
             (cl-prolog-kit::%proof-predicate-entries (quote (indexed ?value)) state)))
      (is (eq first-snapshot
              (cl-prolog-kit::%proof-predicate-entries (quote (indexed ?value)) state)))
      (rulebase-insert-clause! rulebase (make-clause (quote (indexed added))))
      (let ((next-snapshot
              (cl-prolog-kit::%proof-predicate-entries (quote (indexed ?value)) state)))
        (is (not (eq first-snapshot next-snapshot)))
        (is-equal (quote ((indexed original) (indexed added)))
                  (stored-clause-heads next-snapshot)))))

(deftest ordinary-predicates-are-not-replayed-for-tabling ()
  (let ((rulebase (prolog
                    ((run-once) (assertz marker)))))
    (assert-query rulebase (run-once) :succeeds)
    (is-equal 1 (length (query-prolog rulebase 'marker)))))

(deftest depth-counts-only-user-rule-resolution ()
  (let ((rb (prolog
              ((ready))
              ((through-call) (call ready))
              ((through-not) (not false)))))
    (is-equal '(nil) (query-prolog rb '(ready) :max-depth 0))
    (is-equal '(nil) (query-prolog rb '(through-call) :max-depth 1))
    (is-equal '(nil) (query-prolog rb '(through-not) :max-depth 1))
    (handler-case
        (progn
          (query-prolog rb '(through-call) :max-depth 0)
          (error "Expected a PROLOG-DEPTH-LIMIT-EXCEEDED"))
      (prolog-depth-limit-exceeded (condition)
        (is-equal '(through-call)
                  (prolog-depth-limit-exceeded-goal condition))))))

(deftest call-with-depth-limit-counts-rules-and-preserves-global-limit ()
  (let ((rb (prolog
              ((ready))
              ((one-deep) (ready))
              ((two-deep) (one-deep)))))
    (assert-query rb (call_with_depth_limit true 1 ?depth)
                  :ordered (((?depth . 1))))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit true 0 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog-kit::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (assert-query rb (call_with_depth_limit (ready) 1 ?depth)
                  :ordered (((?depth . 1))))
    (assert-query rb (call_with_depth_limit (one-deep) 2 ?depth)
                  :ordered (((?depth . 2))))
    (assert-query rb (call_with_depth_limit (two-deep) 3 ?depth)
                  :ordered (((?depth . 3))))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit (ready) 0 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog-kit::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (let* ((solutions
             (query-prolog rb '(call_with_depth_limit (two-deep) 2 ?result)))
           (result (logic-substitute '?result (first solutions))))
      (is (eq (cl-prolog-kit::%iso-atom "DEPTH_LIMIT_EXCEEDED") result)))
    (signals-prolog-condition prolog-depth-limit-exceeded
      (query-prolog rb '(call_with_depth_limit (one-deep) 5 ?result)
                    :max-depth 0))))

(deftest call-with-depth-limit-is-cut-opaque ()
  (let ((rb (make-rulebase)))
    (is-equal
     '(((?depth . cl-prolog-kit::depth_limit_exceeded) (?side . ?side))
       ((?depth . ?depth) (?side . fallback)))
     (query-prolog
      rb '(or (call_with_depth_limit (and ! fail) 0 ?depth)
              (= ?side fallback))))))

(deftest call-with-depth-limit-is-uncatchable-by-goal ()
  (let ((rb (prolog
              ((looping) (looping)))))
    (let* ((solutions
             (query-prolog
              rb
              '(call_with_depth_limit
                (catch (looping) ?caught true) 0 ?result)))
           (solution (first solutions)))
      ;; Unbound query variables are represented by self-bindings in solutions;
      ;; substituting through one would recurse indefinitely.
      (is (logic-var-p (cdr (assoc '?caught solution))))
      (is (eq (cl-prolog-kit::%iso-atom "DEPTH_LIMIT_EXCEEDED")
              (logic-substitute '?result solution))))))

(deftest nested-call-with-depth-limit-overrides-only-the-inner-scope ()
  (let ((rb (prolog
              ((ready))
              ((one-deep) (ready)))))
    (assert-query
     rb
     (call_with_depth_limit
      (call_with_depth_limit (one-deep) 2 ?inner-depth)
      1 ?outer-depth)
     :ordered (((?inner-depth . 2) (?outer-depth . 1))))))

(deftest call-with-depth-limit-does-not-scope-over-the-caller-continuation ()
  (let ((rb (make-rulebase)))
    (assert-query
     rb
     (and (call_with_depth_limit true 1 ?depth)
          (= ?side ok))
     :ordered (((?depth . 1) (?side . ok))))))

(deftest finite-proofs-are-unbounded-by-default ()
    (let ((rb (make-rulebase))
          (chain-length 20))
      (labels ((predicate-at (index)
                 (intern (format nil "DEPTH-~D" index) *package*)))
        (rulebase-insert-clause! rb (make-clause (list (predicate-at 0))))
        (loop for index from 1 to chain-length
              do (rulebase-insert-clause!
                  rb
                  (make-clause (list (predicate-at index))
                               (list (list (predicate-at (1- index)))))))
        (is-equal (quote (nil))
                  (query-prolog rb (list (predicate-at chain-length))))
        (is (handler-case
                (progn
                  (query-prolog rb (list (predicate-at chain-length))
                                :max-depth (1- chain-length))
                  nil)
              (prolog-depth-limit-exceeded () t))))))

  (deftest proof-state-environment-index-follows-binding-updates ()
    (let* ((rulebase (make-rulebase))
           (bindings (quote ((?seed . initial))))
           (state
             (cl-prolog-kit::%make-proof-state
              rulebase
              bindings
              (cl-prolog-kit::%make-environment-index bindings)
              nil
              cl-prolog-kit::+default-prolog-module+
              (cl-prolog-kit::%make-rulebase-table-session rulebase)
              (cl-prolog-kit::%make-cut-tag)))
           (extended-bindings
             (acons (quote ?derived) (quote ?seed) bindings))
           (extended
             (cl-prolog-kit::%state-with state :bindings extended-bindings)))
      (is-equal (quote initial)
                (cl-prolog-kit::%logic-substitute-indexed
                 (quote ?derived)
                 (cl-prolog-kit::proof-state-environment-index extended)))
      (is (not (eq (cl-prolog-kit::proof-state-environment-index state)
                   (cl-prolog-kit::proof-state-environment-index extended))))))

  (deftest proof-state-explicit-environment-index-takes-precedence ()
    (let* ((rulebase (make-rulebase))
           (bindings (quote ((?seed . initial))))
           (state
             (cl-prolog-kit::%make-proof-state
              rulebase
              bindings
              (cl-prolog-kit::%make-environment-index bindings)
              nil
              cl-prolog-kit::+default-prolog-module+
              (cl-prolog-kit::%make-rulebase-table-session rulebase)
              (cl-prolog-kit::%make-cut-tag)))
           (updated-bindings (quote ((?seed . updated))))
           (supplied-index
             (cl-prolog-kit::%make-environment-index
              (quote ((?seed . supplied)))))
           (updated
             (cl-prolog-kit::%state-with
              state
              :bindings updated-bindings
              :environment-index supplied-index)))
      (is (eq supplied-index
              (cl-prolog-kit::proof-state-environment-index updated)))
      (is-equal (quote supplied)
                (cl-prolog-kit::%logic-substitute-indexed
                 (quote ?seed)
                 (cl-prolog-kit::proof-state-environment-index updated)))))

  (deftest proof-state-unchanged-cut-tag-preserves-identity ()
    (let* ((rulebase (make-rulebase))
           (state
             (cl-prolog-kit::%make-proof-state
              rulebase
              nil
              (cl-prolog-kit::%make-environment-index nil)
              nil
              cl-prolog-kit::+default-prolog-module+
              (cl-prolog-kit::%make-rulebase-table-session rulebase)
              (cl-prolog-kit::%make-cut-tag))))
      (is (eq state
              (cl-prolog-kit::%state-with
               state
               :cut-tag (cl-prolog-kit::proof-state-cut-tag state))))))

    (deftest proof-state-module-update-bypasses-unchanged-cut-tag-identity ()
    (let* ((rulebase (make-rulebase))
           (state
             (cl-prolog-kit::%make-proof-state
              rulebase
              nil
              (cl-prolog-kit::%make-environment-index nil)
              nil
              cl-prolog-kit::+default-prolog-module+
              (cl-prolog-kit::%make-rulebase-table-session rulebase)
              (cl-prolog-kit::%make-cut-tag)))
           (updated
             (cl-prolog-kit::%state-with
              state
              :cut-tag (cl-prolog-kit::proof-state-cut-tag state)
              :module (quote other))))
      (is (not (eq state updated)))
      (is-equal (quote other)
                (cl-prolog-kit::proof-state-module updated))))

  (deftest indexed-query-state-handles-initial-bindings-builtins-and-projection ()
    (is-equal
     (quote (((?left . ready) (?seed . ready) (?right . ready))))
     (query-prolog
      (make-rulebase)
      (quote ((= ?left ?seed) (= ?right ?left)))
      :environment (quote ((?seed . ready))))))

  (deftest constraint-hook-propagated-bindings-update-the-state-index ()
    (let ((hook-ran-p nil)
          (rulebase (prolog ((trigger)))))
      (let ((cl-prolog-kit::*constraint-post-unify-hook*
              (lambda (environment emit)
                (funcall
                 emit
                 (if hook-ran-p
                     environment
                     (progn
                       (setf hook-ran-p t)
                       (acons (quote ?hooked)
                              (quote propagated)
                              environment)))))))
        (is-equal
         (quote (((?hooked . propagated))))
         (query-prolog
          rulebase
          (quote ((trigger) (= ?hooked propagated))))))))

  (deftest table-answer-replay-preserves-parent-index-for-projection ()
  (let ((rulebase
          (prolog
           ((tabled-source alpha))
           ((tabled-source beta)))))
    (cl-prolog-kit::%add-rulebase-table-declaration!
     rulebase (quote tabled-source) 1 :test)
    (is-equal
     (quote
      (((?first . alpha) (?pair alpha alpha))
       ((?first . beta) (?pair beta beta))))
     (query-prolog
      rulebase
      (quote
       ((tabled-source ?first)
        (tabled-source ?first)
        (= ?pair (?first ?first))))))))

(deftest predicate-descriptors-copy-on-write-every-mutation ()
  (let ((rulebase (make-rulebase))
        (module cl-prolog-kit::+default-prolog-module+))
    (rulebase-insert-clause! rulebase (make-clause (quote (cow middle))))
    (let* ((middle-descriptor
             (cl-prolog-kit::%rulebase-predicate-descriptor
              rulebase module (quote cow) 1))
           (middle-snapshot
             (cl-prolog-kit::%predicate-descriptor-entries middle-descriptor)))
      (rulebase-insert-clause!
       rulebase (make-clause (quote (cow first))) :position :first)
      (let* ((first-descriptor
               (cl-prolog-kit::%rulebase-predicate-descriptor
                rulebase module (quote cow) 1))
             (first-snapshot
               (cl-prolog-kit::%predicate-descriptor-entries first-descriptor)))
        (is (not (eq middle-descriptor first-descriptor)))
        (is-equal
         (quote ((cow middle)))
         (stored-clause-heads middle-snapshot))
        (is-equal
         (quote ((cow first) (cow middle)))
         (stored-clause-heads first-snapshot))
        (rulebase-insert-clause!
         rulebase (make-clause (quote (cow last))) :position :last)
        (let* ((last-descriptor
                 (cl-prolog-kit::%rulebase-predicate-descriptor
                  rulebase module (quote cow) 1))
               (last-snapshot
                 (cl-prolog-kit::%predicate-descriptor-entries last-descriptor))
               (middle-entry (second last-snapshot)))
          (is (not (eq first-descriptor last-descriptor)))
          (is-equal
           (quote ((cow first) (cow middle) (cow last)))
           (stored-clause-heads last-snapshot))
          (is (cl-prolog-kit::%rulebase-retract-entry! rulebase middle-entry))
          (let* ((retract-descriptor
                   (cl-prolog-kit::%rulebase-predicate-descriptor
                    rulebase module (quote cow) 1))
                 (retract-snapshot
                   (cl-prolog-kit::%predicate-descriptor-entries
                    retract-descriptor)))
            (is (not (eq last-descriptor retract-descriptor)))
            (is-equal
             (quote ((cow first) (cow last)))
             (stored-clause-heads retract-snapshot))
            (is-equal
             (quote ((cow first) (cow middle) (cow last)))
             (stored-clause-heads last-snapshot))
            (is (cl-prolog-kit::%rulebase-retract-entries!
                 rulebase retract-snapshot))
            (is (null
                 (cl-prolog-kit::%rulebase-predicate-descriptor
                  rulebase module (quote cow) 1)))
            (is (zerop
                 (hash-table-count
                  (cl-prolog-kit::rulebase-predicate-descriptors rulebase))))))))))

(deftest predicate-descriptor-candidates-preserve-order-without-lookup-writes ()
  (let* ((rulebase
           (prolog
            ((indexed target exact-first))
            ((indexed ?value wildcard))
            ((indexed other excluded))
            ((indexed target exact-last))))
         (module cl-prolog-kit::+default-prolog-module+)
         (descriptor
           (cl-prolog-kit::%rulebase-predicate-descriptor
            rulebase module (quote indexed) 2))
         (root (cl-prolog-kit::rulebase-predicate-descriptors rulebase))
         (predicates (gethash module root))
         (arities (gethash (quote indexed) predicates))
         (symbols
           (cl-prolog-kit::%predicate-descriptor-symbol-first-argument-index
            descriptor))
         (atoms
           (cl-prolog-kit::%predicate-descriptor-atom-first-argument-index
            descriptor))
         (counts
           (list (hash-table-count root)
                 (hash-table-count predicates)
                 (hash-table-count arities)
                 (hash-table-count symbols)
                 (hash-table-count atoms))))
    (is-equal
     (quote
      ((indexed target exact-first)
       (indexed ?value wildcard)
       (indexed target exact-last)))
     (stored-clause-heads
      (cl-prolog-kit::%predicate-descriptor-first-argument-entries
       descriptor (quote target))))
    (is-equal
     (quote ((indexed ?value wildcard)))
     (stored-clause-heads
      (cl-prolog-kit::%predicate-descriptor-first-argument-entries
       descriptor (quote absent))))
    (is-equal
     (quote
      ((indexed target exact-first)
       (indexed ?value wildcard)
       (indexed other excluded)
       (indexed target exact-last)))
     (stored-clause-heads
      (cl-prolog-kit::%predicate-descriptor-first-argument-entries
       descriptor (quote ?query))))
    (is (null
         (cl-prolog-kit::%rulebase-predicate-descriptor
          rulebase (make-symbol "MISSING-MODULE") (quote indexed) 2)))
    (is (null
         (cl-prolog-kit::%rulebase-predicate-descriptor
          rulebase module (make-symbol "MISSING-PREDICATE") 2)))
    (is (null
         (cl-prolog-kit::%rulebase-predicate-descriptor
          rulebase module (quote indexed) 3)))
    (cl-prolog-kit::%predicate-descriptor-first-argument-entries
     descriptor (make-symbol "MISSING-FIRST-ARGUMENT"))
    (is-equal
     counts
     (list (hash-table-count root)
           (hash-table-count predicates)
           (hash-table-count arities)
           (hash-table-count symbols)
           (hash-table-count atoms)))))

(deftest predicate-descriptor-high-cardinality-keys-preserve-order ()
    (let* ((key-count 512)
           (target 257)
           (rulebase
             (make-rulebase
              :clauses
              (append
               (loop for index below key-count
                     collect
                     (make-clause (list 'high-cardinality index 'exact-before)))
               (list (make-clause '(high-cardinality ?value wildcard)))
               (loop for index below key-count
                     collect
                     (make-clause (list 'high-cardinality index 'exact-after))))))
           (descriptor
             (cl-prolog-kit::%rulebase-predicate-descriptor
              rulebase cl-prolog-kit::+default-prolog-module+
              'high-cardinality 2)))
      (is-equal
       `((high-cardinality ,target exact-before)
         (high-cardinality ?value wildcard)
         (high-cardinality ,target exact-after))
       (stored-clause-heads
        (cl-prolog-kit::%predicate-descriptor-first-argument-entries
         descriptor target)))))

  (deftest predicate-index-resolves-first-argument-aliases-before-selection ()
    (let* ((environment '((?outer . ?inner) (?inner . exact-after)))
           (rulebase
             (prolog
              ((alias-index exact-before exact-before))
              ((alias-index ?value wildcard))
              ((alias-index exact-after exact-after))))
           (session (cl-prolog-kit::%make-rulebase-table-session rulebase))
           (state
             (cl-prolog-kit::%make-proof-state
              rulebase
              environment
              (cl-prolog-kit::%make-environment-index environment)
              nil
              cl-prolog-kit::+default-prolog-module+
              session
              (cl-prolog-kit::%make-cut-tag))))
      (is-equal
       '((alias-index ?value wildcard)
         (alias-index exact-after exact-after))
       (stored-clause-heads
        (cl-prolog-kit::%proof-predicate-entries
         '(alias-index ?outer ?result) state)))))

(deftest predicate-descriptor-canonicalizes-symbol-atoms-and-indexes-eql-atoms ()
  (let* ((package-a
           (make-package
            (symbol-name (gensym "DESCRIPTOR-PACKAGE-A-")) :use (quote ())))
         (package-b
           (make-package
            (symbol-name (gensym "DESCRIPTOR-PACKAGE-B-")) :use (quote ()))))
    (unwind-protect
         (let* ((package-atom-a (intern "SAME" package-a))
                (package-atom-b (intern "SAME" package-b))
                (interned-verbatim-atom (prolog-atom "SAME"))
                (uninterned-verbatim-atom (make-symbol "SAME"))
                (rulebase
                  (make-rulebase
                   :clauses
                   (list
                    (make-clause
                     (list (quote atom-key) package-atom-a (quote package-a)))
                    (make-clause (quote (atom-key ?value wildcard)))
                    (make-clause
                     (list (quote atom-key) package-atom-b (quote package-b)))
                    (make-clause
                     (list
                      (quote atom-key)
                      interned-verbatim-atom
                      (quote interned)))
                    (make-clause
                     (list
                      (quote atom-key)
                      uninterned-verbatim-atom
                      (quote uninterned))))))
                (descriptor
                  (cl-prolog-kit::%rulebase-predicate-descriptor
                   rulebase cl-prolog-kit::+default-prolog-module+
                   (quote atom-key) 2)))
           (is (not (eq package-atom-a package-atom-b)))
           (is (not (eq interned-verbatim-atom uninterned-verbatim-atom)))
           (is-equal
            (list
             (list (quote atom-key) package-atom-a (quote package-a))
             (quote (atom-key ?value wildcard))
             (list (quote atom-key) package-atom-b (quote package-b)))
            (stored-clause-heads
             (cl-prolog-kit::%predicate-descriptor-first-argument-entries
              descriptor package-atom-b)))
           (is-equal
            (list
             (quote (atom-key ?value wildcard))
             (list
              (quote atom-key)
              interned-verbatim-atom
              (quote interned))
             (list
              (quote atom-key)
              uninterned-verbatim-atom
              (quote uninterned)))
            (stored-clause-heads
             (cl-prolog-kit::%predicate-descriptor-first-argument-entries
              descriptor uninterned-verbatim-atom))))
      (delete-package package-a)
      (delete-package package-b)))
  (let* ((rulebase
           (prolog
            ((atomic-key 42 number))
            ((atomic-key ?value wildcard))
            ((atomic-key #\x character))))
         (descriptor
           (cl-prolog-kit::%rulebase-predicate-descriptor
            rulebase cl-prolog-kit::+default-prolog-module+
            (quote atomic-key) 2)))
    (is-equal
     (quote ((atomic-key 42 number) (atomic-key ?value wildcard)))
     (stored-clause-heads
      (cl-prolog-kit::%predicate-descriptor-first-argument-entries
       descriptor 42)))
    (is-equal
     (quote ((atomic-key ?value wildcard) (atomic-key #\x character)))
     (stored-clause-heads
      (cl-prolog-kit::%predicate-descriptor-first-argument-entries
       descriptor #\x)))
    (is-equal
     (quote ((atomic-key ?value wildcard)))
     (stored-clause-heads
      (cl-prolog-kit::%predicate-descriptor-first-argument-entries
       descriptor 99)))))

(deftest predicate-descriptor-leaves-mutable-compound-and-cyclic-terms-unindexed ()
  (let* ((string (copy-seq "string"))
         (bits (copy-seq #*101))
         (compound (list (quote compound) (quote value)))
         (cycle (cons (quote loop) nil))
         (rulebase (make-rulebase)))
    (setf (cdr cycle) cycle)
    (dolist (head
             (list (list (quote unindexed) string (quote string))
                   (list (quote unindexed) bits (quote bits))
                   (list (quote unindexed) compound (quote compound))
                   (list (quote unindexed) cycle (quote cycle))))
      (rulebase-insert-clause! rulebase (make-clause head)))
    (let* ((descriptor
             (cl-prolog-kit::%rulebase-predicate-descriptor
              rulebase cl-prolog-kit::+default-prolog-module+
              (quote unindexed) 2))
           (entries (cl-prolog-kit::%predicate-descriptor-entries descriptor)))
      (is (zerop
           (hash-table-count
            (cl-prolog-kit::%predicate-descriptor-symbol-first-argument-index
             descriptor))))
      (is (zerop
           (hash-table-count
            (cl-prolog-kit::%predicate-descriptor-atom-first-argument-index
             descriptor))))
      (dolist (first-argument (list string bits compound cycle))
        (is (eq entries
                (cl-prolog-kit::%predicate-descriptor-first-argument-entries
                 descriptor first-argument))))
      (setf (char string 0) #\X
            (aref bits 0) 0
            (car compound) (quote changed)
            (car cycle) (quote changed))
      (dolist (first-argument (list string bits compound cycle))
        (is (eq entries
                (cl-prolog-kit::%predicate-descriptor-first-argument-entries
                 descriptor first-argument)))))))

(deftest predicate-descriptors-rebuild-on-copy-and-same-revision-replace ()
  (let* ((source
           (make-rulebase
            :clauses (list (make-clause (quote (replace-source value))))))
         (target
           (make-rulebase
            :clauses (list (make-clause (quote (replace-target value))))))
         (source-descriptor
           (cl-prolog-kit::%rulebase-predicate-descriptor
            source cl-prolog-kit::+default-prolog-module+
            (quote replace-source) 1))
         (target-descriptor
           (cl-prolog-kit::%rulebase-predicate-descriptor
            target cl-prolog-kit::+default-prolog-module+
            (quote replace-target) 1))
         (revision (cl-prolog-kit::rulebase-revision target)))
    (is (= revision (cl-prolog-kit::rulebase-revision source)))
    (cl-prolog-kit::%replace-rulebase! target source)
    (let ((replacement-descriptor
            (cl-prolog-kit::%rulebase-predicate-descriptor
             target cl-prolog-kit::+default-prolog-module+
             (quote replace-source) 1)))
      (is (null
           (cl-prolog-kit::%rulebase-predicate-descriptor
            target cl-prolog-kit::+default-prolog-module+
            (quote replace-target) 1)))
      (is (not (eq target-descriptor replacement-descriptor)))
      (is (not (eq source-descriptor replacement-descriptor)))
      (is-equal
       (quote ((replace-source value)))
       (stored-clause-heads
        (cl-prolog-kit::%predicate-descriptor-entries replacement-descriptor)))
      (let* ((copy (cl-prolog-kit::%copy-rulebase target))
             (copy-descriptor
               (cl-prolog-kit::%rulebase-predicate-descriptor
                copy cl-prolog-kit::+default-prolog-module+
                (quote replace-source) 1)))
        (is (not (eq replacement-descriptor copy-descriptor)))
        (is (not
             (eq
              (cl-prolog-kit::%predicate-descriptor-entries replacement-descriptor)
              (cl-prolog-kit::%predicate-descriptor-entries copy-descriptor))))
        (is-equal
         (quote ((replace-source value)))
         (stored-clause-heads
          (cl-prolog-kit::%predicate-descriptor-entries copy-descriptor)))))))

(deftest table-answer-replay-reuses-ground-answer-across-consumers ()
    (let ((rulebase (prolog ((tabled-ground alpha)))))
      (cl-prolog-kit::%add-rulebase-table-declaration!
       rulebase (quote tabled-ground) 1 :test)
      (let ((solutions
              (query-prolog
               rulebase
               (quote
                ((tabled-ground ?first)
                 (tabled-ground ?second)
                 (tabled-ground ?third))))))
        (is (= 1 (length solutions)))
        (let ((environment (first solutions)))
          (is-equal (quote alpha)
                    (logic-substitute (quote ?first) environment))
          (is-equal (quote alpha)
                    (logic-substitute (quote ?second) environment))
          (is-equal (quote alpha)
                    (logic-substitute (quote ?third) environment))))))

  (deftest table-answer-replay-freshens-nonground-answer-per-consumer ()
    (let ((rulebase (prolog ((tabled-variable ?item)))))
      (cl-prolog-kit::%add-rulebase-table-declaration!
       rulebase (quote tabled-variable) 1 :test)
      (let ((solutions
              (query-prolog
               rulebase
               (quote
                ((tabled-variable ?first)
                 (tabled-variable ?second)
                 (= ?first alpha)
                 (= ?second beta))))))
        (is (= 1 (length solutions)))
        (let ((environment (first solutions)))
          (is-equal (quote alpha)
                    (logic-substitute (quote ?first) environment))
          (is-equal (quote beta)
                    (logic-substitute (quote ?second) environment))))))

(defun retract-assert-cycle (rulebase count)
  "Retract counter/1 and reassert it with the next value, COUNT times.

Each half is its own top-level call, so *PROLOG-ACTIVE-TOP-LEVEL-CALLS*
returns to 0 between them and dead-entry compaction gets its chance to run."
  (dotimes (index count)
    (query-prolog rulebase
                  (list (quote cl-prolog-kit::retract)
                        (list (quote cl-prolog-kit::counter) index)))
    (query-prolog rulebase
                  (list (quote cl-prolog-kit::assertz)
                        (list (quote cl-prolog-kit::counter) (1+ index))))))

(deftest dead-entries-survive-below-the-compaction-threshold (:timeout 60)
  "Retracted clauses stay physically present until the backlog reaches
*RULEBASE-COMPACTION-THRESHOLD*, which is what keeps
%RULEBASE-PREDICATE-ENTRIES-AT-REVISION able to answer for an old revision."
  (let* ((rulebase (make-rulebase))
         (threshold cl-prolog-kit::*rulebase-compaction-threshold*))
    (assert-query rulebase (assertz (cl-prolog-kit::counter 0)) :succeeds)
    (retract-assert-cycle rulebase (1- threshold))
    (is-equal (1- threshold) (cl-prolog-kit::rulebase-dead-entries rulebase))
    (is-equal threshold (length (cl-prolog-kit::rulebase-entries rulebase)))
    (is-equal (list (list (cons (quote ?value) (1- threshold))))
              (query-prolog rulebase (quote (cl-prolog-kit::counter ?value))))))

(deftest compaction-rebuilds-the-index-from-the-surviving-entries (:timeout 60)
  "Reaching the threshold physically drops every dead entry from
RULEBASE-ENTRIES and rebuilds RULEBASE-PREDICATE-INDEX/-TAILS from what is
left: the churning predicate loses its bucket outright while the untouched one
keeps its clause, its bucket and a tail that still points into that bucket."
  (let* ((rulebase (make-rulebase))
         (threshold cl-prolog-kit::*rulebase-compaction-threshold*)
         (counter-key
           (list cl-prolog-kit::+default-prolog-module+ (quote cl-prolog-kit::counter) 1))
         (stable-key
           (list cl-prolog-kit::+default-prolog-module+ (quote cl-prolog-kit::stable) 1))
         (stable-head (list (quote cl-prolog-kit::stable) (quote cl-prolog-kit::kept))))
    (assert-query rulebase (assertz (cl-prolog-kit::stable cl-prolog-kit::kept)) :succeeds)
    (assert-query rulebase (assertz (cl-prolog-kit::counter 0)) :succeeds)
    (retract-assert-cycle rulebase (1- threshold))
    ;; One more retract takes the backlog to the threshold, so compaction runs
    ;; while counter/1 has no live clause at all.
    (query-prolog rulebase
                  (list (quote cl-prolog-kit::retract)
                        (list (quote cl-prolog-kit::counter) (1- threshold))))
    (is-equal 0 (cl-prolog-kit::rulebase-dead-entries rulebase))
    (let ((entries (cl-prolog-kit::rulebase-entries rulebase)))
      (is-equal (list stable-head) (stored-clause-heads entries))
      (is (eq (last entries) (cl-prolog-kit::rulebase-entries-tail rulebase))))
    (let ((bucket (gethash stable-key
                           (cl-prolog-kit::rulebase-predicate-index rulebase))))
      (is-equal (list stable-head) (stored-clause-heads bucket))
      (is (eq (last bucket)
              (gethash stable-key
                       (cl-prolog-kit::rulebase-predicate-tails rulebase)))))
    (is (null (gethash counter-key
                       (cl-prolog-kit::rulebase-predicate-index rulebase))))
    (is-equal (list (list (cons (quote ?value) (quote cl-prolog-kit::kept))))
              (query-prolog rulebase (quote (cl-prolog-kit::stable ?value))))
    (is (null (query-prolog rulebase (quote (cl-prolog-kit::counter ?value)))))))

(deftest compaction-is-a-no-op-without-dead-entries ()
  (let* ((rulebase (make-rulebase)))
    (assert-query rulebase (assertz (cl-prolog-kit::counter 0)) :succeeds)
    (let ((entries (cl-prolog-kit::rulebase-entries rulebase))
          (index (cl-prolog-kit::rulebase-predicate-index rulebase)))
      (is-equal 0 (cl-prolog-kit::rulebase-dead-entries rulebase))
      (is (eq rulebase (cl-prolog-kit::%compact-rulebase! rulebase)))
      (is (eq entries (cl-prolog-kit::rulebase-entries rulebase)))
      (is (eq index (cl-prolog-kit::rulebase-predicate-index rulebase))))))
