;;;; library(pairs): relate a list of Key-Value pairs to its keys and values.

(in-package #:cl-prolog-kit)

(defun %pairs-pair (key value)
  (list (%text-atom "-") key value))

(defun %require-pair-list (term environment operation)
  "Resolve TERM to a proper list of Key-Value pairs.  Returns (VALUES PAIRS
BOUND-P): BOUND-P is NIL only when TERM is unbound (so an empty list is
distinguished from an absent one)."
  (let ((resolved (logic-substitute term environment)))
    (cond
      ((logic-var-p resolved) (values nil nil))
      ((%proper-list-p resolved)
       (dolist (pair resolved)
         (unless (and (%proper-list-p pair) (= (length pair) 3)
                      (symbolp (first pair))
                      (string= (symbol-name (first pair)) "-"))
           (%raise-type-error "PAIR" pair environment operation
                              "expected a list of Key-Value pairs")))
       (values resolved t))
      (t (%raise-type-error "LIST" resolved environment operation
                            "expected a proper list of pairs")))))

(define-builtin (pairs_keys_values pairs keys values)
    (rulebase environment depth emit)
  (let ((operation (%iso-atom "PAIRS_KEYS_VALUES")))
    (multiple-value-bind (pair-list bound-p)
        (%require-pair-list pairs environment operation)
      (if bound-p
          ;; forward: pairs → keys and values
          (%term-unify-sequence
           (list (cons keys (mapcar #'second pair-list))
                 (cons values (mapcar #'third pair-list)))
           environment emit)
          ;; reverse: keys and values → pairs
          (let ((key-list (logic-substitute keys environment))
                (value-list (logic-substitute values environment)))
            (unless (and (%proper-list-p key-list) (%proper-list-p value-list))
              (%raise-instantiation-error
               environment operation
               "pairs_keys_values/3 needs the pairs, or both keys and values"))
            (unless (= (length key-list) (length value-list))
              (%raise-type-error "LIST" values environment operation
                                 "keys and values must have equal length"))
            (%unify-emit pairs
                         (mapcar #'%pairs-pair key-list value-list)
                         environment emit))))))

(defun %pairs-projection-goal (pairs target selector environment emit operation)
  "Shared body for pairs_keys/2 and pairs_values/2."
  (multiple-value-bind (pair-list bound-p)
      (%require-pair-list pairs environment operation)
    (unless bound-p
      (%raise-instantiation-error environment operation
                                  "requires an instantiated pair list"))
    (%unify-emit target (mapcar selector pair-list) environment emit)))

(define-builtin (pairs_keys pairs keys) (rulebase environment depth emit)
  (%pairs-projection-goal pairs keys #'second environment emit
                          (%iso-atom "PAIRS_KEYS")))

(define-builtin (pairs_values pairs values) (rulebase environment depth emit)
  (%pairs-projection-goal pairs values #'third environment emit
                          (%iso-atom "PAIRS_VALUES")))
