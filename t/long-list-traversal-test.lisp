;;;; Term traversals walk list spines iteratively: a long list must not
;;;; exhaust the control stack, and sharing, cycles, dotted tails and
;;;; numbering order must match the recursive traversal they replaced.

(in-package #:cl-prolog-kit.tests)

(defparameter *long-list-length* 100000
  "Twice the highest length at which a recursive traversal crashed (50000).")

(defun %long-list-goal-holds-p (program goal)
  (prolog-succeeds-p
   (if program (consult-prolog program) (make-rulebase))
   (read-prolog-term (format nil goal *long-list-length*))))

(deftest long-list-passes-through-msort ()
  ;; Copies the sorted list through LOGIC-SUBSTITUTE.
  (is (%long-list-goal-holds-p
       nil "numlist(1, ~D, L), msort(L, S), S = [1, 2|_]")))

(deftest long-list-passes-through-assertz-and-call ()
  ;; %COPY-CLAUSE and %COMPILE-CLAUSE-TEMPLATE on insert, then
  ;; %MATERIALIZE-CLAUSE-TEMPLATE-REFERENCE for the non-ground clause.
  (is (%long-list-goal-holds-p
       ":- dynamic(big/1)."
       "numlist(1, ~D, L), assertz(big([_|L])), big([_, 1, 2|_])")))

(deftest long-list-passes-through-tabling ()
  ;; %CANONICALIZE-VARIANT and %INSTANTIATE-VARIANT.
  (is (%long-list-goal-holds-p
       ":- table(t/1). t(_)."
       "numlist(1, ~D, L), t([_|L])")))

(deftest long-list-passes-through-copy-term-and-findall ()
  ;; %FRESHEN-TERM and LOGIC-SUBSTITUTE.
  (is (%long-list-goal-holds-p
       nil "numlist(1, ~D, L), copy_term([_|L], [_, 1|_])"))
  (is (%long-list-goal-holds-p
       nil "numlist(1, ~D, L), findall([_|L], true, [[_, 1|_]])")))

(deftest long-list-passes-through-acyclic-term ()
  (is (%long-list-goal-holds-p nil "numlist(1, ~D, L), acyclic_term(L)")))

(defun %long-list (&optional (tail '()))
  (let ((list (loop for i from 1 to *long-list-length* collect i)))
    (setf (cdr (last list)) tail)
    list))

(deftest long-lists-pass-through-internal-traversals ()
  (let* ((variable (fresh-logic-variable "?TAIL"))
         (list (%long-list variable)))
    (is (eq :cons (first (cl-prolog-kit::%variant-graph-key (%long-list)))))
    (let ((freshened (cl-prolog-kit::%freshen-term
                      list (make-hash-table :test #'eq))))
      (is (logic-var-p (cdr (last freshened))))
      (is (not (eq variable (cdr (last freshened))))))
    (let ((marked (%long-list (list cl-prolog-kit::+variant-variable-marker+ 0))))
      (is (logic-var-p (cdr (last (cl-prolog-kit::%instantiate-variant marked nil)))))
      (is (logic-var-p (cdr (last (cl-prolog-kit::%instantiate-variant marked t))))))))

(deftest spine-copies-preserve-sharing-cycles-and-dotted-tails ()
  (let* ((suffix (list 'c 'd))
         (shared (list (cons 'a suffix) (cons 'b suffix)))
         (cycle (list 'x 'y 'z))
         (dotted (list* 'p 'q 'r)))
    (setf (cdr (last cycle)) (cdr cycle))
    (flet ((check-copy (copy-function)
             (let ((copied (funcall copy-function shared)))
               (is-equal shared copied)
               (is (not (eq (cdr (first shared)) (cdr (first copied)))))
               (is (eq (cdr (first copied)) (cdr (second copied)))))
             (let ((copied (funcall copy-function cycle)))
               (is (not (eq cycle copied)))
               (is (eq (cdr copied) (cdddr copied)))
               (is-equal '(x y z y) (list (first copied) (second copied)
                                          (third copied) (fourth copied))))
             (let ((copied (funcall copy-function dotted)))
               (is-equal dotted copied)
               (is (not (eq dotted copied))))))
      (check-copy (lambda (term)
                    (clause-head (cl-prolog-kit::%copy-clause (make-clause term)))))
      (check-copy (lambda (term)
                    (cl-prolog-kit::%freshen-term term (make-hash-table :test #'eq))))
      (check-copy (lambda (term)
                    ;; A bound variable forces LOGIC-SUBSTITUTE to copy.
                    (rest (logic-substitute (cons '?bound term) '((?bound . k))))))
      (check-copy (lambda (term)
                    (rest
                     (clause-head
                      (cl-prolog-kit::%materialize-clause-template
                      (cl-prolog-kit::%compile-clause-template
                       (make-clause (cons '?fresh term)))))))))))

(deftest logic-substitute-resolves-a-bound-dotted-tail ()
  (is-equal '(a b c d)
            (logic-substitute '(a b . ?rest) '((?rest . (c . ?last)) (?last . (d))))))

(deftest freshen-term-numbers-variables-in-first-occurrence-order ()
  (let* ((table (make-hash-table :test #'eq))
         (freshened (cl-prolog-kit::%freshen-term '(?a (?b ?a) ?c . ?b) table)))
    (is (eq (first freshened) (second (second freshened))))
    (is (eq (first (second freshened)) (cdddr freshened)))
    ;; FRESH-LOGIC-VARIABLE numbers its names, so the counters show the order.
    (flet ((counter (variable)
             (parse-integer (symbol-name variable) :start (length "?FRESH"))))
      (is (< (counter (first freshened))
             (counter (first (second freshened)))
             (counter (third freshened)))))))

(deftest clause-template-numbers-conses-car-before-cdr ()
  (let* ((template (cl-prolog-kit::%compile-clause-template
                    (make-clause '(p (x ?v) ?w . z))))
         (describe
           (lambda (reference)
             (list (cl-prolog-kit::%clause-template-reference-kind reference)
                   (cl-prolog-kit::%clause-template-reference-value reference)))))
    (is-equal '((:literal p) (:cons 2) (:literal x) (:variable 0) (:variable 1))
              (map 'list describe (cl-prolog-kit::%clause-template-cons-cars template)))
    (is-equal '((:cons 1) (:cons 4) (:cons 3) (:literal nil) (:literal z))
              (map 'list describe (cl-prolog-kit::%clause-template-cons-cdrs template)))
    (is-equal 2 (cl-prolog-kit::%clause-template-variable-count template))))

(deftest variant-graph-key-encodes-cdr-cycles-as-references ()
  (let ((cycle (list 'a 'b)))
    (setf (cddr cycle) cycle)
    (is-equal '(:cons 0 (:atom a) (:cons 1 (:atom b) (:reference 0)))
              (cl-prolog-kit::%variant-graph-key cycle))
    (is-equal '(:cons 0 (:atom a) (:cons 1 (:atom b) (:atom c)))
              (cl-prolog-kit::%variant-graph-key '(a b . c)))))

(deftest instantiate-variant-shares-unchanged-suffixes ()
  (let* ((marker (list cl-prolog-kit::+variant-variable-marker+ 0))
         (ground-suffix (list 'c 'd))
         (term (list* marker 'b ground-suffix)))
    (dolist (cyclic-p '(nil t))
      (let ((instance (cl-prolog-kit::%instantiate-variant term cyclic-p)))
        (is (logic-var-p (first instance)))
        (is (eq (cdr term) (cdr instance)))
        (is (eq ground-suffix (cddr instance)))))
    ;; A marker in tail position is a variable, not a two-element list.
    (let ((instance (cl-prolog-kit::%instantiate-variant (list* 'a marker) nil)))
      (is (logic-var-p (cdr instance))))
    (let ((cycle (list marker 'b)))
      (setf (cddr cycle) cycle)
      (let ((instance (cl-prolog-kit::%instantiate-variant cycle t)))
        (is (logic-var-p (first instance)))
        (is (eq instance (cddr instance)))))))

(deftest acyclic-and-canonical-checks-see-spine-cycles-only ()
  (let ((cycle (list 'a 'b 'c))
        (suffix (list 'x 'y)))
    (setf (cdr (last cycle)) (cdr cycle))
    (is (not (cl-prolog-kit::%term-acyclic-p cycle)))
    (is (cl-prolog-kit::%term-acyclic-p (list suffix suffix (cons 'z suffix))))
    (let ((car-cycle (list 'f nil)))
      (setf (second car-cycle) car-cycle)
      (is (not (cl-prolog-kit::%term-acyclic-p car-cycle))))
    (is (nth-value 1 (cl-prolog-kit::%canonicalize-variant
                      (cons '?v cycle)
                      (cl-prolog-kit::%make-environment-index '()))))
    (is (not (nth-value 1 (cl-prolog-kit::%canonicalize-variant
                           (list '?v suffix suffix)
                           (cl-prolog-kit::%make-environment-index '())))))))
