;;;; First-argument eq, string, and compound indexing, and the
;;;; rulebase-insert-clauses! bulk insert.

(in-package #:cl-prolog-kit.tests)

(defun %entry-heads (entries)
  (mapcar (lambda (entry)
            (clause-head (cl-prolog-kit::%stored-clause-clause entry)))
          entries))

(defun %default-descriptor (rulebase predicate arity)
  (cl-prolog-kit::%rulebase-predicate-descriptor
   rulebase cl-prolog-kit::+default-prolog-module+ predicate arity))

(defun %candidate-heads (descriptor first-argument)
  (%entry-heads
   (cl-prolog-kit::%predicate-descriptor-first-argument-entries
    descriptor first-argument)))

(defun %clauses-from-heads (heads)
  (mapcar #'make-clause heads))

(deftest symbol-eq-index-shares-the-text-bucket ()
  (let* ((rulebase
           (prolog
            ((eq-key alpha first))
            ((eq-key ?value wildcard))
            ((eq-key beta other))
            ((eq-key alpha last))))
         (descriptor (%default-descriptor rulebase 'eq-key 2))
         (eq-index
           (cl-prolog-kit::%predicate-descriptor-symbol-eq-first-argument-index
            descriptor))
         (text-index
           (cl-prolog-kit::%predicate-descriptor-symbol-first-argument-index
            descriptor)))
    (is-equal 2 (hash-table-count eq-index))
    (is (eq (gethash 'alpha eq-index) (gethash "alpha" text-index)))
    (is (eq (gethash 'alpha eq-index)
            (cl-prolog-kit::%predicate-descriptor-first-argument-entries
             descriptor 'alpha)))
    (is-equal '((eq-key alpha first) (eq-key ?value wildcard) (eq-key alpha last))
              (%candidate-heads descriptor 'alpha))
    (is-equal '((eq-key ?value wildcard))
              (%candidate-heads descriptor 'absent))))

(deftest symbol-eq-miss-falls-back-to-atom-text ()
  ;; CL:LIST and the parser's USER-ATOMS::LIST are distinct symbols for the one
  ;; atom `list'; a goal naming the symbol absent from the clause heads must
  ;; still select the clause through the text table.
  (let* ((parser-list (prolog-atom "list"))
         (verbatim (prolog-atom "Foo"))
         (uninterned-verbatim (make-symbol "Foo"))
         (rulebase
           (make-rulebase
            :clauses
            (%clauses-from-heads
             (list (list 'same-text 'list 'lisp-list)
                   (list 'same-text '?value 'wildcard)
                   (list 'same-text verbatim 'verbatim)
                   (list 'same-text 'foo 'canonical)))))
         (descriptor (%default-descriptor rulebase 'same-text 2))
         (eq-index
           (cl-prolog-kit::%predicate-descriptor-symbol-eq-first-argument-index
            descriptor)))
    (is (not (eq parser-list 'list)))
    (is-equal "list" (prolog-atom-text parser-list))
    (is (null (nth-value 1 (gethash parser-list eq-index))))
    (is-equal (list (list 'same-text 'list 'lisp-list)
                    (list 'same-text '?value 'wildcard))
              (%candidate-heads descriptor parser-list))
    (is-equal '(((?who . lisp-list)) ((?who . wildcard)))
              (query-prolog rulebase (list 'same-text parser-list '?who)))
    ;; Verbatim `'Foo'' and canonical `foo' are different atoms; the
    ;; uninterned verbatim spelling shares the verbatim text only.
    (is (null (nth-value 1 (gethash uninterned-verbatim eq-index))))
    (is-equal (list (list 'same-text '?value 'wildcard)
                    (list 'same-text verbatim 'verbatim))
              (%candidate-heads descriptor uninterned-verbatim))
    (is-equal (list (list 'same-text '?value 'wildcard)
                    (list 'same-text 'foo 'canonical))
              (%candidate-heads descriptor 'foo))
    (is-equal '(((?who . wildcard)) ((?who . verbatim)))
              (query-prolog rulebase
                            (list 'same-text uninterned-verbatim '?who)))))

(deftest string-index-never-collides-with-atoms ()
  (let* ((caller-string (copy-seq "abc"))
         (rulebase
           (make-rulebase
            :clauses
            (%clauses-from-heads
             (list (list 'text-key caller-string 'string)
                   (list 'text-key 'abc 'atom)
                   (list 'text-key '?value 'wildcard)
                   (list 'text-key "ABC" 'upper-string)))))
         (descriptor (%default-descriptor rulebase 'text-key 2)))
    (is-equal '((text-key "abc" string) (text-key ?value wildcard))
              (%candidate-heads descriptor "abc"))
    (is-equal '((text-key abc atom) (text-key ?value wildcard))
              (%candidate-heads descriptor 'abc))
    (is-equal '((text-key ?value wildcard))
              (%candidate-heads descriptor "missing"))
    (is-equal '(((?kind . string)) ((?kind . wildcard)))
              (query-prolog rulebase (list 'text-key "abc" '?kind)))
    (is-equal '(((?kind . atom)) ((?kind . wildcard)))
              (query-prolog rulebase '(text-key abc ?kind)))
    ;; The rulebase owns its strings, so the caller mutating its string
    ;; changes neither the stored clause nor its index key.
    (setf (char caller-string 0) #\x)
    (is-equal '(((?kind . string)) ((?kind . wildcard)))
              (query-prolog rulebase (list 'text-key "abc" '?kind)))
    (is-equal '(((?kind . wildcard)))
              (query-prolog rulebase (list 'text-key "xbc" '?kind)))))

(deftest compound-index-keys-functor-and-arity-preserving-order ()
  (let* ((heads '((shape (point 1 2) point-2)
                  (shape ?value wildcard)
                  (shape (point 1) point-1)
                  (shape (a b c) list-abc)
                  (shape (?functor 1 2) variable-functor)
                  (shape (1 2) number-list)
                  (shape (point 3 4) point-2-later)
                  (shape point atom)))
         (rulebase (make-rulebase :clauses (%clauses-from-heads heads)))
         (descriptor (%default-descriptor rulebase 'shape 2)))
    (is-equal 2 (cl-prolog-kit::%predicate-descriptor-compound-max-arity
                 descriptor))
    (is-equal '((shape (point 1 2) point-2)
                (shape ?value wildcard)
                (shape (?functor 1 2) variable-functor)
                (shape (1 2) number-list)
                (shape (point 3 4) point-2-later))
              (%candidate-heads descriptor '(point ?x ?y)))
    (is-equal '((shape ?value wildcard)
                (shape (point 1) point-1)
                (shape (?functor 1 2) variable-functor)
                (shape (1 2) number-list))
              (%candidate-heads descriptor '(point ?x)))
    ;; The list [a, b, c] is the cons (A B C), indexed as a/2.
    (is-equal '((shape ?value wildcard)
                (shape (a b c) list-abc)
                (shape (?functor 1 2) variable-functor)
                (shape (1 2) number-list))
              (%candidate-heads descriptor '(a b c)))
    (let ((fallback '((shape ?value wildcard)
                      (shape (?functor 1 2) variable-functor)
                      (shape (1 2) number-list))))
      (is-equal fallback (%candidate-heads descriptor '(point 1 2 3)))
      (is-equal fallback (%candidate-heads descriptor '(unknown 1)))
      (is-equal fallback (%candidate-heads descriptor '(5 6)))
      (is-equal fallback (%candidate-heads descriptor '(point 1 . 2)))
      ;; The spine walk stops after the longest indexed arity, so a cyclic
      ;; goal terminates.
      (let ((cycle (list 'point)))
        (setf (cdr cycle) cycle)
        (is-equal fallback (%candidate-heads descriptor cycle))))
    (is-equal heads (%candidate-heads descriptor '(point 1 . ?tail)))
    (is-equal heads (%candidate-heads descriptor '(?functor 1 2)))
    (is-equal '((shape ?value wildcard) (shape point atom))
              (%candidate-heads descriptor 'point))
    (let ((solutions (query-prolog rulebase '(shape (point ?x ?y) ?kind))))
      (is-equal '(point-2 wildcard variable-functor point-2-later)
                (mapcar (lambda (solution) (cdr (assoc '?kind solution)))
                        solutions))
      (is-equal '(3 4)
                (list (cdr (assoc '?x (fourth solutions)))
                      (cdr (assoc '?y (fourth solutions))))))))

(deftest compound-eq-miss-falls-back-to-functor-text ()
  (let* ((parser-list (prolog-atom "list"))
         (rulebase
           (make-rulebase
            :clauses
            (%clauses-from-heads
             (list (list 'functor-text (list 'list 1) 'lisp-list)
                   (list 'functor-text (list 'other 1) 'other)))))
         (descriptor (%default-descriptor rulebase 'functor-text 2)))
    (is (null (nth-value
               1 (gethash parser-list
                          (cl-prolog-kit::%predicate-descriptor-compound-eq-first-argument-index
                           descriptor)))))
    (is-equal (list (list 'functor-text (list 'list 1) 'lisp-list))
              (%candidate-heads descriptor (list parser-list 1)))
    (is-equal '(((?kind . lisp-list)))
              (query-prolog rulebase
                            (list 'functor-text (list parser-list 1) '?kind)))))

(deftest unbound-first-argument-selects-every-clause ()
  (let* ((heads '((any-key alpha one)
                  (any-key "alpha" two)
                  (any-key (alpha 1) three)
                  (any-key 7 four)
                  (any-key ?value five)))
         (rulebase (make-rulebase :clauses (%clauses-from-heads heads)))
         (descriptor (%default-descriptor rulebase 'any-key 2)))
    (is (eq (cl-prolog-kit::%predicate-descriptor-entries descriptor)
            (cl-prolog-kit::%predicate-descriptor-first-argument-entries
             descriptor '?query)))
    (is-equal '(one two three four five)
              (mapcar (lambda (solution) (cdr (assoc '?n solution)))
                      (query-prolog rulebase '(any-key ?key ?n))))))

(defun %bulk-fixture-clauses ()
  (%clauses-from-heads
   '((bulk alpha 1) (other x) (bulk ?any 2) (bulk "s" 3) (bulk (f 1) 4)
     (other y) (bulk alpha 5))))

(defun %bulk-base-rulebase ()
  (make-rulebase
   :clauses (%clauses-from-heads '((bulk alpha 0) (other w) (bulk beta 0)))))

(defun %rulebase-observation (rulebase)
  "Return what a caller can observe of RULEBASE's clause order and indexes."
  (let ((index (cl-prolog-kit::rulebase-predicate-index rulebase)))
    (list
     (mapcar #'clause-head (rulebase-visible-clauses rulebase))
     (%entry-heads
      (gethash (list cl-prolog-kit::+default-prolog-module+ 'bulk 2) index))
     (%entry-heads
      (gethash (list cl-prolog-kit::+default-prolog-module+ 'other 1) index))
     (query-prolog rulebase '(bulk alpha ?n))
     (query-prolog rulebase '(bulk "s" ?n))
     (query-prolog rulebase '(bulk (f ?x) ?n))
     (query-prolog rulebase '(other ?x)))))

(deftest bulk-insert-matches-repeated-single-inserts ()
  (let ((bulk (%bulk-base-rulebase))
        (single (%bulk-base-rulebase)))
    (is (eq bulk (rulebase-insert-clauses! bulk (%bulk-fixture-clauses))))
    (dolist (clause (%bulk-fixture-clauses))
      (rulebase-insert-clause! single clause))
    (is-equal (%rulebase-observation single) (%rulebase-observation bulk))
    (is-equal '(((?n . 0)) ((?n . 1)) ((?n . 2)) ((?n . 5)))
              (query-prolog bulk '(bulk alpha ?n))))
  ;; :FIRST places CLAUSES in their own order before the existing clauses,
  ;; which repeated single :FIRST inserts reach by inserting back to front.
  (let ((bulk (%bulk-base-rulebase))
        (single (%bulk-base-rulebase)))
    (rulebase-insert-clauses! bulk (%bulk-fixture-clauses) :position :first)
    (dolist (clause (reverse (%bulk-fixture-clauses)))
      (rulebase-insert-clause! single clause :position :first))
    (is-equal (%rulebase-observation single) (%rulebase-observation bulk))
    (is-equal '(((?n . 1)) ((?n . 2)) ((?n . 5)) ((?n . 0)))
              (query-prolog bulk '(bulk alpha ?n)))
    (is-equal '(bulk alpha 1)
              (clause-head (first (rulebase-visible-clauses bulk))))))

(deftest bulk-insert-into-empty-rulebase-maintains-tails ()
  (dolist (position '(:first :last))
    (let ((rulebase (make-rulebase)))
      (rulebase-insert-clauses! rulebase (%bulk-fixture-clauses)
                                :position position)
      (rulebase-insert-clause! rulebase (make-clause '(bulk alpha 6)))
      (rulebase-insert-clause! rulebase (make-clause '(bulk alpha -1))
                               :position :first)
      (is-equal '(((?n . -1)) ((?n . 1)) ((?n . 2)) ((?n . 5)) ((?n . 6)))
                (query-prolog rulebase '(bulk alpha ?n)))
      (is (eq (last (cl-prolog-kit::rulebase-entries rulebase))
              (cl-prolog-kit::rulebase-entries-tail rulebase))))))

(deftest bulk-insert-bumps-the-revision-once ()
  (let* ((rulebase (%bulk-base-rulebase))
         (before (cl-prolog-kit::rulebase-revision rulebase)))
    (rulebase-insert-clauses! rulebase (%bulk-fixture-clauses))
    (is-equal (1+ before) (cl-prolog-kit::rulebase-revision rulebase))
    (is-equal '(1)
              (remove-duplicates
               (mapcar (lambda (entry)
                         (- (cl-prolog-kit::%stored-clause-born-revision entry)
                            before))
                       (nthcdr 3 (cl-prolog-kit::rulebase-entries rulebase)))))))

(deftest bulk-insert-of-no-clauses-is-a-no-op ()
  (let* ((rulebase (%bulk-base-rulebase))
         (before (cl-prolog-kit::rulebase-revision rulebase))
         (entries (cl-prolog-kit::rulebase-entries rulebase))
         (descriptor (%default-descriptor rulebase 'bulk 2)))
    (is (eq rulebase (rulebase-insert-clauses! rulebase '() :position :first)))
    (is-equal before (cl-prolog-kit::rulebase-revision rulebase))
    (is (eq entries (cl-prolog-kit::rulebase-entries rulebase)))
    (is (eq descriptor (%default-descriptor rulebase 'bulk 2)))))

(deftest bulk-insert-leaves-rulebase-unchanged-on-a-malformed-clause ()
  (let* ((rulebase (%bulk-base-rulebase))
         (before (cl-prolog-kit::rulebase-revision rulebase))
         (visible (rulebase-visible-clauses rulebase)))
    (signals-error
     (rulebase-insert-clauses! rulebase
                               (list (make-clause '(bulk alpha 9)) 'not-a-clause)))
    (is-equal before (cl-prolog-kit::rulebase-revision rulebase))
    (is-equal (mapcar #'clause-head visible)
              (mapcar #'clause-head (rulebase-visible-clauses rulebase)))))

;;; A head whose argument list is improper snapshots fine but has no arity, so
;;; computing its predicate key signals; it must do so before any mutation.
(defun %improper-head-clause ()
  (make-clause '(bulk . 42)))

(defun %insert-failure-observation (rulebase)
  "Return RULEBASE's revision, entry spine, tail, buckets, descriptor and queries."
  (list (cl-prolog-kit::rulebase-revision rulebase)
        (copy-list (cl-prolog-kit::rulebase-entries rulebase))
        (cl-prolog-kit::rulebase-entries-tail rulebase)
        (%rulebase-observation rulebase)
        (%candidate-heads (%default-descriptor rulebase 'bulk 2) 'alpha)
        (%candidate-heads (%default-descriptor rulebase 'bulk 2) '?any)))

(deftest bulk-insert-leaves-rulebase-unchanged-on-an-improper-head ()
  (dolist (position '(:first :last))
    (let* ((rulebase (%bulk-base-rulebase))
           (before (%insert-failure-observation rulebase)))
      (signals-error
       (rulebase-insert-clauses! rulebase
                                 (list (make-clause '(bulk alpha 9))
                                       (make-clause '(other z))
                                       (%improper-head-clause))
                                 :position position))
      (is-equal before (%insert-failure-observation rulebase))
      ;; The rulebase still accepts inserts and indexes them normally.
      (rulebase-insert-clauses! rulebase (list (make-clause '(bulk alpha 9)))
                                :position position)
      (is-equal (if (eq position :first)
                    '(((?n . 9)) ((?n . 0)))
                    '(((?n . 0)) ((?n . 9))))
                (query-prolog rulebase '(bulk alpha ?n))))))

(deftest single-insert-leaves-rulebase-unchanged-on-an-improper-head ()
  (dolist (position '(:first :last))
    (let* ((rulebase (%bulk-base-rulebase))
           (before (%insert-failure-observation rulebase)))
      (signals-error
       (rulebase-insert-clause! rulebase (%improper-head-clause)
                                :position position))
      (is-equal before (%insert-failure-observation rulebase)))))

(deftest overlay-leaves-rulebase-unchanged-when-insertion-fails ()
  (let* ((rulebase (%bulk-base-rulebase))
         (before (%insert-failure-observation rulebase))
         (called nil))
    (signals-error
     (call-with-overlay-facts
      rulebase
      (list (make-clause '(bulk alpha 9)) (%improper-head-clause))
      (lambda () (setf called t))))
    (is (not called))
    (is-equal before (%insert-failure-observation rulebase))
    (signals-error
     (with-overlay-facts (rulebase (list (make-clause '(other z))
                                         (%improper-head-clause)))
       (setf called t)))
    (is (not called))
    (is-equal before (%insert-failure-observation rulebase))))

;;; A regression would make LENGTH loop on the cycle, so each cyclic case runs
;;; under a timeout, and assertions compare only acyclic values: a failure
;;; report printing a cyclic term would not terminate either.
(defun %outcome-within-timeout (thunk)
  "Return :SIGNALLED, :RETURNED, or :TIMED-OUT for calling THUNK."
  (handler-case (sb-ext:with-timeout 5 (funcall thunk) :returned)
    (sb-ext:timeout () :timed-out)
    (error () :signalled)))

(defun %circular-head-clause ()
  (let ((head (list 'bulk 'alpha)))
    (setf (cddr head) (cdr head))
    (make-clause head)))

(deftest inserts-reject-circular-clause-heads ()
  (dolist (position '(:first :last))
    (let* ((rulebase (%bulk-base-rulebase))
           (before (%insert-failure-observation rulebase)))
      (is-equal :signalled
                (%outcome-within-timeout
                 (lambda ()
                   (rulebase-insert-clause! rulebase (%circular-head-clause)
                                            :position position))))
      (is-equal before (%insert-failure-observation rulebase))
      (is-equal :signalled
                (%outcome-within-timeout
                 (lambda ()
                   (rulebase-insert-clauses!
                    rulebase
                    (list (make-clause '(bulk alpha 9)) (%circular-head-clause))
                    :position position))))
      (is-equal before (%insert-failure-observation rulebase)))))

(deftest assertz-of-a-circular-head-raises-a-type-error ()
  (dolist (source '("H = [p|H], catch(assertz(H), error(type_error(T, _), _), true)"
                    "H = [p|H], catch(assertz((H :- true)), error(type_error(T, _), _), true)"
                    "L = [a|L], catch(asserta([p|L]), error(type_error(T, _), _), true)"))
    (let* ((rulebase (make-rulebase))
           (kind :unset))
      (is (prolog-succeeds-p
           rulebase (read-prolog-term "set_prolog_flag(occurs_check, false)")))
      (is-equal :returned
                (%outcome-within-timeout
                 (lambda ()
                   (setf kind
                         (cdr (find "?T"
                                    (first (query-prolog
                                            rulebase (read-prolog-term source)))
                                    :key (lambda (binding)
                                           (symbol-name (car binding)))
                                    :test #'string=))))))
      (is-equal "callable" (and (symbolp kind) (prolog-atom-text kind)))
      (is-equal 0 (length (rulebase-visible-clauses rulebase))))))

(deftest bulk-insert-keeps-the-logical-update-view ()
  (dolist (position '(:first :last))
    (let ((rulebase
            (make-rulebase
             :clauses (%clauses-from-heads '((running 1) (running 2)))))
          (seen '()))
      (map-prolog-solutions
       (lambda (solution)
         (when (null seen)
           (rulebase-insert-clauses!
            rulebase
            (%clauses-from-heads '((running 3) (running 4)))
            :position position))
         (push (cdr (assoc '?n solution)) seen))
       rulebase '(running ?n))
      (is-equal '(1 2) (reverse seen))
      (is-equal (if (eq position :first) '(3 4 1 2) '(1 2 3 4))
                (mapcar (lambda (solution) (cdr (assoc '?n solution)))
                        (query-prolog rulebase '(running ?n)))))))

(deftest bulk-insert-honours-module-and-source ()
  (let* ((rulebase (make-rulebase))
         (module :bulk-module)
         (source #p"/virtual/bulk.pl"))
    (rulebase-insert-clauses! rulebase
                              (%clauses-from-heads '((scoped a) (scoped b)))
                              :module module :source source)
    (is (null (%default-descriptor rulebase 'scoped 1)))
    (let ((descriptor
            (cl-prolog-kit::%rulebase-predicate-descriptor
             rulebase module 'scoped 1)))
      (is-equal '((scoped b)) (%candidate-heads descriptor 'b)))
    (dolist (entry (cl-prolog-kit::rulebase-entries rulebase))
      (is (eq module (cl-prolog-kit::%stored-clause-module entry)))
      (is (eq source (cl-prolog-kit::%stored-clause-source entry))))))

(deftest rulebase-extend-prepends-clauses-in-order-with-one-revision ()
  (let* ((base (%bulk-base-rulebase))
         (extended
           (rulebase-extend base (%clauses-from-heads '((bulk alpha 1)
                                                        (bulk alpha 2))))))
    (is-equal '(((?n . 1)) ((?n . 2)) ((?n . 0)))
              (query-prolog extended '(bulk alpha ?n)))
    (is-equal '(((?n . 0)))
              (query-prolog base '(bulk alpha ?n)))
    (is-equal (1+ (cl-prolog-kit::rulebase-revision base))
              (cl-prolog-kit::rulebase-revision extended))))
