;;;; library(assoc): association lists keyed by the standard order of terms.
;;;;
;;;; An assoc is the arity-1 compound `assoc(Pairs)' whose single argument is a
;;;; proper list of `Key-Value' pairs kept sorted by the standard order of
;;;; their keys; the empty assoc is `assoc([])'.  (SWI uses an opaque AVL tree;
;;;; this list-backed form is O(n) per op but has the same observable relation.
;;;; It is an ordinary inspectable/unifiable compound, unlike SWI's opaque
;;;; type -- see docs/src/reference/semantics.md.)  Keys are compared with %COMPARE-TERMS,
;;;; so any ground term may be a key.

(in-package #:cl-prolog-kit)

(defun %assoc-functor () (%text-atom "assoc"))
(defun %pair-functor () (%text-atom "-"))
(defun %make-pair (key value) (list (%pair-functor) key value))
(defun %pair-key (pair) (second pair))
(defun %pair-value (pair) (third pair))

(defun %pair-term-p (term)
  (and (%proper-list-p term) (= (length term) 3)
       (symbolp (first term))
       (%same-atom-text-p (first term) (%pair-functor))))

(defun %make-assoc (sorted-pairs)
  (list (%assoc-functor) sorted-pairs))

(defun %assoc-pairs (assoc environment operation)
  "Validate ASSOC as `assoc(ProperPairList)' and return its sorted pair list.
A malformed or cyclic assoc raises a catchable ISO type_error rather than
letting a host walk diverge."
  (let ((resolved (logic-substitute assoc environment)))
    (cond
      ((logic-var-p resolved)
       (%raise-instantiation-error environment operation
                                   "assoc must be instantiated"))
      ((and (%proper-list-p resolved)
            (= (length resolved) 2)
            (symbolp (first resolved))
            (%same-atom-text-p (first resolved) (%assoc-functor))
            (%proper-list-p (second resolved)))
       (let ((pairs (second resolved)))
         (dolist (pair pairs pairs)
           (unless (%pair-term-p pair)
             (%raise-type-error "ASSOC" resolved environment operation
                                "assoc must hold Key-Value pairs")))))
      (t (%raise-type-error "ASSOC" resolved environment operation
                            "not a valid association")))))

(defun %assoc-key= (a b)
  (zerop (%compare-terms a b)))

(defun %assoc-insert (pair pairs)
  "Insert PAIR into the key-sorted PAIRS, keeping the order (iteratively, so no
O(n) stack depth on large maps)."
  (let ((key (%pair-key pair)) (before '()) (rest pairs))
    (loop while (and rest (minusp (%compare-terms (%pair-key (car rest)) key)))
          do (push (car rest) before)
             (setf rest (cdr rest)))
    (nreconc before (cons pair rest))))

(defun %require-assoc-input-pairs (list-term environment operation)
  "Resolve LIST-TERM to a proper list of Key-Value pairs (fresh pair terms),
raising on duplicate keys as SWI's list_to_assoc/2 does."
  (let ((resolved (logic-substitute list-term environment)))
    (unless (%proper-list-p resolved)
      (if (logic-var-p resolved)
          (%raise-instantiation-error environment operation
                                      "pair list must be instantiated")
          (%raise-type-error "LIST" resolved environment operation
                             "expected a proper list of Key-Value pairs")))
    (let ((sorted (stable-sort
                   (mapcar (lambda (pair)
                             (unless (%pair-term-p pair)
                               (%raise-type-error "PAIR" pair environment operation
                                                  "list_to_assoc/2 expects Key-Value pairs"))
                             (%make-pair (%pair-key pair) (%pair-value pair)))
                           resolved)
                   #'%prolog-term< :key #'%pair-key)))
      (loop for (a b) on sorted
            when (and b (%assoc-key= (%pair-key a) (%pair-key b)))
              do (%raise-domain-error "UNIQUE_KEY_PAIRS" (%pair-key a)
                                      environment operation
                                      "list_to_assoc/2 keys must be unique"))
      sorted)))

(define-builtin (empty_assoc assoc) (rulebase environment depth emit)
  (%unify-emit assoc (%make-assoc '()) environment emit))

(define-builtin (get_assoc key assoc value) (rulebase environment depth emit)
  (let* ((operation (%iso-atom "GET_ASSOC"))
         (key-value (logic-substitute key environment))
         (pairs (%assoc-pairs assoc environment operation))
         (match (find key-value pairs :key #'%pair-key :test #'%assoc-key=)))
    (when match
      (%unify-emit value (%pair-value match) environment emit))))

(define-builtin (put_assoc key assoc value new-assoc)
    (rulebase environment depth emit)
  (let* ((operation (%iso-atom "PUT_ASSOC"))
         (key-value (logic-substitute key environment))
         (value-value (logic-substitute value environment))
         (pairs (%assoc-pairs assoc environment operation))
         (without (remove key-value pairs :key #'%pair-key :test #'%assoc-key=)))
    (%unify-emit new-assoc
                 (%make-assoc (%assoc-insert (%make-pair key-value value-value)
                                             without))
                 environment emit)))

(define-builtin (del_assoc key assoc value new-assoc)
    (rulebase environment depth emit)
  (let* ((operation (%iso-atom "DEL_ASSOC"))
         (key-value (logic-substitute key environment))
         (pairs (%assoc-pairs assoc environment operation))
         (match (find key-value pairs :key #'%pair-key :test #'%assoc-key=)))
    (when match
      ;; REMOVE preserves order, so the result stays sorted -- no re-sort.
      (%term-unify-sequence
       (list (cons value (%pair-value match))
             (cons new-assoc (%make-assoc (remove match pairs :test #'eq))))
       environment emit))))

(define-builtin (list_to_assoc pairs assoc) (rulebase environment depth emit)
  (%unify-emit assoc
               (%make-assoc (%require-assoc-input-pairs
                             pairs environment (%iso-atom "LIST_TO_ASSOC")))
               environment emit))

(macrolet ((define-assoc-projection-builtins (&body specifications)
             ;; Each specification is (NAME RESULT OPERATION PROJECTION); a NIL
             ;; PROJECTION yields the sorted pairs themselves.
             `(progn
                ,@(loop for (name result operation projection) in specifications
                        collect
                        `(define-builtin (,name assoc ,result)
                             (rulebase environment depth emit)
                           (let ((entries (%assoc-pairs assoc environment
                                                        (%iso-atom ,operation))))
                             (%unify-emit ,result
                                          ,(if projection
                                               `(mapcar ,projection entries)
                                               'entries)
                                          environment emit)))))))
  (define-assoc-projection-builtins
    (assoc_to_list pairs "ASSOC_TO_LIST" nil)
    (assoc_to_keys keys "ASSOC_TO_KEYS" #'%pair-key)
    (assoc_to_values values "ASSOC_TO_VALUES" #'%pair-value)))
