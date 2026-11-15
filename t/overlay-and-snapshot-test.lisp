;;;; RULEBASE-SNAPSHOT and the overlay-fact API (CALL-WITH-OVERLAY-FACTS,
;;;; WITH-OVERLAY-FACTS): snapshot independence, overlay lifetime across
;;;; normal and non-local exits, and interplay with compaction, loading
;;;; transactions, and a second thread.

(in-package #:cl-prolog-kit.tests)

(defun %overlay-values (rulebase goal variable)
  "Return the bindings of VARIABLE across every solution of GOAL, in order."
  (mapcar (lambda (solution) (cdr (assoc variable solution)))
          (query-prolog rulebase goal)))

(defun %overlay-base-rulebase ()
  "Return a rulebase whose dynamic ITEM/1 holds BASE1 and BASE2."
  (let ((rulebase (make-rulebase)))
    ;; assertz, unlike MAKE-RULEBASE's :CLAUSES, declares ITEM/1 dynamic, so
    ;; the tests may assert and retract it from Prolog.
    (prolog-succeeds-p rulebase '(assertz (item base1)))
    (prolog-succeeds-p rulebase '(assertz (item base2)))
    rulebase))

(defun %overlay-facts (predicate values)
  (mapcar (lambda (value) (make-clause (list predicate value))) values))

;;; Snapshot

(deftest snapshot-isolates-changes-made-to-the-original ()
  (let* ((original (%overlay-base-rulebase))
         (snapshot (rulebase-snapshot original)))
    (is (prolog-succeeds-p original '(assertz (counter 1))))
    (is (prolog-succeeds-p original '(retract (item base1))))
    (rulebase-insert-clause! original (make-clause '(item base3)))
    (is-equal '(base2 base3) (%overlay-values original '(item ?x) '?x))
    (is-equal '(base1 base2) (%overlay-values snapshot '(item ?x) '?x))
    (is (not (prolog-succeeds-p snapshot '(clause (counter ?n) true))))))

(deftest snapshot-isolates-changes-made-to-the-snapshot ()
  (let* ((original (%overlay-base-rulebase))
         (snapshot nil))
    (is (prolog-succeeds-p original '(assertz (counter 1))))
    (setf snapshot (rulebase-snapshot original))
    (is (prolog-succeeds-p snapshot '(retract (counter 1))))
    (is (prolog-succeeds-p snapshot '(assertz (counter 2))))
    (is (prolog-succeeds-p snapshot '(retract (item base2))))
    (is-equal '(2) (%overlay-values snapshot '(counter ?n) '?n))
    (is-equal '(1) (%overlay-values original '(counter ?n) '?n))
    (is-equal '(base1 base2) (%overlay-values original '(item ?x) '?x))
    (is-equal '(base1) (%overlay-values snapshot '(item ?x) '?x))))

(deftest snapshot-retract-of-a-shared-clause-affects-one-side-only ()
  (let* ((original (%overlay-base-rulebase))
         (snapshot (rulebase-snapshot original)))
    (is (prolog-succeeds-p original '(retract (item base1))))
    (is (prolog-succeeds-p snapshot '(retract (item base2))))
    (is-equal '(base2) (%overlay-values original '(item ?x) '?x))
    (is-equal '(base1) (%overlay-values snapshot '(item ?x) '?x))))

(deftest snapshot-preserves-flags-operators-and-properties ()
  (let ((original (make-rulebase)))
    (is (prolog-succeeds-p
         original
         '(cl-prolog-kit::set_prolog_flag cl-prolog-kit::double_quotes
           cl-prolog-kit::atom)))
    (is (prolog-succeeds-p
         original '(cl-prolog-kit::op 700 cl-prolog-kit::xfx snapshot_op)))
    (is (consult-prolog ":- dynamic(declared_only/1)." original))
    (let ((snapshot (rulebase-snapshot original)))
      (is (prolog-succeeds-p
           snapshot
           '(cl-prolog-kit::current_prolog_flag cl-prolog-kit::double_quotes
             cl-prolog-kit::atom)))
      (is (prolog-succeeds-p
           snapshot '(cl-prolog-kit::current_op 700 cl-prolog-kit::xfx
                      snapshot_op)))
      ;; Parsed atoms live in their own package, so the query is parsed too.
      (is (prolog-succeeds-p
           snapshot
           (read-prolog-term "predicate_property(declared_only(_), dynamic)")))
      ;; Declarations made afterwards stay on the side that made them.
      (is (prolog-succeeds-p
           snapshot '(cl-prolog-kit::op 0 cl-prolog-kit::xfx snapshot_op)))
      (is (prolog-succeeds-p
           original '(cl-prolog-kit::current_op 700 cl-prolog-kit::xfx
                      snapshot_op)))
      (is (prolog-succeeds-p
           original
           '(cl-prolog-kit::set_prolog_flag cl-prolog-kit::double_quotes
             cl-prolog-kit::codes)))
      (is (prolog-succeeds-p
           snapshot
           '(cl-prolog-kit::current_prolog_flag cl-prolog-kit::double_quotes
             cl-prolog-kit::atom))))))

;;; Overlay facts

(deftest overlay-facts-are-visible-only-inside-the-body ()
  (let ((rulebase (%overlay-base-rulebase)))
    (is-equal '(base1 base2 overlay1 overlay2)
              (with-overlay-facts
                  (rulebase (%overlay-facts 'item '(overlay1 overlay2)))
                (%overlay-values rulebase '(item ?x) '?x)))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))))

(deftest call-with-overlay-facts-returns-the-function-values ()
  (let ((rulebase (%overlay-base-rulebase)))
    (is-equal '(1 2)
              (multiple-value-list
               (call-with-overlay-facts rulebase
                                        (%overlay-facts 'item '(extra))
                                        (lambda () (values 1 2)))))))

(deftest overlay-facts-are-removed-after-non-local-exits ()
  (let ((rulebase (%overlay-base-rulebase)))
    (is (eq :caught
            (handler-case
                (with-overlay-facts (rulebase (%overlay-facts 'item '(e)))
                  (error "escape"))
              (error () :caught))))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))
    (is (eq :thrown
            (cl:catch 'overlay-tag
              (with-overlay-facts (rulebase (%overlay-facts 'item '(t1)))
                (cl:throw 'overlay-tag :thrown)))))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))
    (is (eq :returned
            (block escape
              (with-overlay-facts (rulebase (%overlay-facts 'item '(r1)))
                (return-from escape :returned)))))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))))

(deftest overlay-rules-take-part-in-resolution ()
  (let ((rulebase (make-rulebase
                   :clauses (list (make-clause '(edge a b))
                                  (make-clause '(edge b c))))))
    (is-equal '(b c)
              (with-overlay-facts
                  (rulebase (list (make-clause '(path ?x ?y) '((edge ?x ?y)))
                                  (make-clause '(path ?x ?z)
                                               '((edge ?x ?y) (path ?y ?z)))))
                (%overlay-values rulebase '(path a ?to) '?to)))
    (is (not (prolog-succeeds-p
              rulebase '(cl-prolog-kit::current_predicate (/ path 2)))))))

(deftest nested-overlays-unwind-in-order ()
  (let ((rulebase (%overlay-base-rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item '(outer)))
      (with-overlay-facts (rulebase (%overlay-facts 'item '(inner)))
        (is-equal '(base1 base2 outer inner)
                  (%overlay-values rulebase '(item ?x) '?x)))
      (is-equal '(base1 base2 outer)
                (%overlay-values rulebase '(item ?x) '?x)))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))))

(deftest overlay-leaves-the-bodys-own-assertions-in-place ()
  (let ((rulebase (%overlay-base-rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item '(temporary)))
      (is (prolog-succeeds-p rulebase '(assertz (item asserted))))
      (is (prolog-succeeds-p rulebase '(assertz (note kept)))))
    (is-equal '(base1 base2 asserted)
              (%overlay-values rulebase '(item ?x) '?x))
    (is-equal '(kept) (%overlay-values rulebase '(note ?x) '?x))))

(deftest overlay-leaves-an-identical-asserted-clause-in-place ()
  (let ((rulebase (%overlay-base-rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item '(twin)))
      (is (prolog-succeeds-p rulebase '(assertz (item twin)))))
    (is-equal '(base1 base2 twin)
              (%overlay-values rulebase '(item ?x) '?x))))

(deftest overlay-fact-retracted-by-the-body-is-not-retracted-twice ()
  (let ((rulebase (%overlay-base-rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item '(gone stays)))
      (is (prolog-succeeds-p rulebase '(retract (item gone))))
      (is-equal 1 (cl-prolog-kit::rulebase-dead-entries rulebase)))
    (is-equal 2 (cl-prolog-kit::rulebase-dead-entries rulebase))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))))

(deftest overlay-is-removed-after-a-loading-transaction-in-the-body ()
  ;; consult-prolog publishes copied stored-clause records through
  ;; %REPLACE-RULEBASE!, so the overlay must find its clauses again.
  (let ((rulebase (%overlay-base-rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item '(overlay)))
      (consult-prolog "loaded(fact)." rulebase)
      (is-equal '(base1 base2 overlay)
                (%overlay-values rulebase '(item ?x) '?x)))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))
    (is (prolog-succeeds-p rulebase (read-prolog-term "loaded(fact)")))))

(deftest overlay-survives-compaction-of-a-large-batch ()
  (let* ((count (+ 88 cl-prolog-kit::*rulebase-compaction-threshold*))
         (values (loop for index below count collect index))
         (rulebase (%overlay-base-rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item values))
      (is-equal (+ 2 count) (length (query-prolog rulebase '(item ?x))))
      ;; Retracting every overlay fact inside the body crosses the
      ;; threshold, so the next top-level query compacts them away before
      ;; the overlay exits.
      (is (prolog-succeeds-p rulebase '(retractall (item ?x))))
      (is (not (prolog-succeeds-p rulebase '(item ?x))))
      (is-equal 0 (cl-prolog-kit::rulebase-dead-entries rulebase))
      (is (prolog-succeeds-p rulebase '(assertz (item fresh)))))
    (is-equal '(fresh) (%overlay-values rulebase '(item ?x) '?x))
    (with-overlay-facts (rulebase (%overlay-facts 'item values))
      (is-equal (+ 1 count) (length (query-prolog rulebase '(item ?x)))))
    (is-equal count (cl-prolog-kit::rulebase-dead-entries rulebase))
    (is-equal '(fresh) (%overlay-values rulebase '(item ?x) '?x))
    (is-equal 0 (cl-prolog-kit::rulebase-dead-entries rulebase))
    (is-equal 1 (length (cl-prolog-kit::rulebase-entries rulebase)))))

;;; Threads

#+sb-thread
(deftest snapshot-is-usable-from-another-thread-while-the-original-changes ()
  (let* ((original (%overlay-base-rulebase))
         (snapshot (rulebase-snapshot original))
         (rounds 200)
         (worker
           (sb-thread:make-thread
            (lambda ()
              (loop repeat rounds
                    do (let ((answers
                               (%overlay-values snapshot '(item ?x) '?x)))
                         (unless (equal answers '(base1 base2))
                           (return answers)))
                    finally (return :consistent)))
            :name "overlay-snapshot-reader")))
    (loop for index below rounds
          do (rulebase-insert-clause! original (make-clause (list 'item index)))
             (prolog-succeeds-p original '(retract (item base1)))
             (prolog-succeeds-p original '(assertz (item base1))))
    (is (eq :consistent (sb-thread:join-thread worker)))
    (is-equal (+ 2 rounds)
              (length (query-prolog original '(item ?x))))))

;;; Interplay with the other v1.6.0 additions

(deftest snapshot-carries-the-capability-policy ()
  (let* ((policy (make-prolog-capability-policy))
         (original (make-rulebase :capability-policy policy))
         (snapshot (rulebase-snapshot original)))
    (is (eq policy (rulebase-capability-policy snapshot)))
    (is (eq :denied
            (handler-case
                (progn (query-prolog snapshot
                              (list 'open (prolog-atom "kb-snapshot.txt")
                                    (prolog-atom "write") '?s))
                       :opened)
              (prolog-permission-error () :denied))))))

(deftest overlay-inserts-a-batch-under-one-revision ()
  (let* ((rulebase (%overlay-base-rulebase))
         (before (cl-prolog-kit::rulebase-revision rulebase)))
    (with-overlay-facts (rulebase (%overlay-facts 'item '(o1 o2 o3)))
      (is-equal (1+ before) (cl-prolog-kit::rulebase-revision rulebase))
      (is-equal '(base1 base2 o1 o2 o3)
                (%overlay-values rulebase '(item ?x) '?x)))
    (is-equal (+ 2 before) (cl-prolog-kit::rulebase-revision rulebase))
    (is-equal '(base1 base2) (%overlay-values rulebase '(item ?x) '?x))))

(deftest overlay-with-no-clauses-leaves-the-revision-alone ()
  (let* ((rulebase (%overlay-base-rulebase))
         (before (cl-prolog-kit::rulebase-revision rulebase)))
    (is-equal :done (with-overlay-facts (rulebase '()) :done))
    (is-equal before (cl-prolog-kit::rulebase-revision rulebase))))

(deftest overlay-on-an-empty-rulebase-is-fully-removed ()
  (let ((rulebase (make-rulebase)))
    (is-equal '(o1 o2)
              (with-overlay-facts (rulebase (%overlay-facts 'item '(o1 o2)))
                (%overlay-values rulebase '(item ?x) '?x)))
    (is (null (rulebase-visible-clauses rulebase)))))
