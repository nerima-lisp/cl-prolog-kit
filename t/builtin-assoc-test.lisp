;;;; library(assoc) tests.  Wrapper predicates keep the opaque assoc term out
;;;; of the query so only plain result terms are asserted.

(in-package #:cl-prolog-kit.tests)

(defun make-assoc-rulebase ()
  (prolog
    ;; build {a->1, b->2} and look a key up
    ((lookup ?k ?v)
     (empty_assoc ?e) (put_assoc b ?e 2 ?a1) (put_assoc a ?a1 1 ?a2)
     (get_assoc ?k ?a2 ?v))
    ;; replace an existing key's value
    ((replaced ?v)
     (empty_assoc ?e) (put_assoc a ?e 1 ?a1) (put_assoc a ?a1 9 ?a2)
     (get_assoc a ?a2 ?v))
    ((keys-of ?k)
     (list_to_assoc ((- c 3) (- a 1) (- b 2)) ?a) (assoc_to_keys ?a ?k))
    ((values-of ?v)
     (list_to_assoc ((- c 3) (- a 1) (- b 2)) ?a) (assoc_to_values ?a ?v))
    ((list-of ?l)
     (list_to_assoc ((- c 3) (- a 1) (- b 2)) ?a) (assoc_to_list ?a ?l))
    ((del-keys ?dv ?k)
     (list_to_assoc ((- a 1) (- b 2)) ?a) (del_assoc a ?a ?dv ?a2)
     (assoc_to_keys ?a2 ?k))
    ((del-missing) (list_to_assoc ((- a 1)) ?a) (del_assoc z ?a ?v ?a2))
    ;; incremental puts (out of order) keep keys sorted
    ((incremental-keys ?k)
     (empty_assoc ?e) (put_assoc c ?e 3 ?a1) (put_assoc a ?a1 1 ?a2)
     (put_assoc b ?a2 2 ?a3) (assoc_to_keys ?a3 ?k))
    ;; non-atom (compound / number) keys sort by standard order
    ((mixed-keys ?k)
     (list_to_assoc ((- (p 2) x) (- 1 y) (- foo z)) ?a) (assoc_to_keys ?a ?k))
    ((empty-list ?l) (empty_assoc ?a) (assoc_to_list ?a ?l))))

(deftest-queries assoc-builtins ((make-assoc-rulebase))
  ((lookup a ?v)     :ordered (((?v . 1))))
  ((lookup b ?v)     :ordered (((?v . 2))))
  ((lookup z ?v)     :fails)
  ((replaced ?v)     :ordered (((?v . 9))))
  ((keys-of ?k)      :ordered (((?k a b c))))
  ((values-of ?v)    :ordered (((?v 1 2 3))))
  ((list-of ?l)      :ordered (((?l (cl-prolog-kit.user-atoms::- a 1) (cl-prolog-kit.user-atoms::- b 2) (cl-prolog-kit.user-atoms::- c 3)))))
  ((del-keys ?dv ?k) :ordered (((?dv . 1) (?k b))))
  ((del-missing)     :fails)
  ((incremental-keys ?k) :ordered (((?k a b c))))
  ((mixed-keys ?k)   :ordered (((?k 1 foo (p 2)))))
  ((empty-list ?l)   :ordered (((?l)))))

(deftest-queries assoc-error-contracts ((make-rulebase))
  ((get_assoc a ?unbound ?v)   :signals)
  ((get_assoc a not-an-assoc ?v) :signals)
  ;; a forged assoc with a non-pair-list argument is a catchable type error,
  ;; not a host crash / hang.
  ((get_assoc a (cl-prolog-kit.user-atoms::assoc 1) ?v) :signals)
  ((get_assoc a (cl-prolog-kit.user-atoms::assoc ((x))) ?v) :signals)
  ((list_to_assoc (not-a-pair) ?a) :signals)
  ((list_to_assoc ?unbound ?a)  :signals)
  ;; SWI rejects duplicate keys in list_to_assoc/2.
  ((list_to_assoc ((- a 1) (- a 2)) ?a) :signals))

