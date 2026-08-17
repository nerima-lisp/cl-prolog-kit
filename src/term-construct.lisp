;;;; Term construction and decomposition builtins: functor/3, arg/3,
;;;; copy_term/2, numbervars/3, =../2, term_variables/2,3.  They share
;;;; %term-unify-sequence (src/builtins/core.lisp) for multi-argument
;;;; unification.

(in-package #:cl-prolog-kit)

(define-builtin (term_variables term variables) (rulebase environment depth emit)
  (%unify-emit variables
               (%collect-variables (%term-resolve term environment))
               environment emit))

(define-builtin (term_variables term variables tail)
    (rulebase environment depth emit)
  (%unify-emit variables
               (append (%collect-variables (%term-resolve term environment))
                       tail)
               environment emit))

(define-iso-builtin (functor term name arity) "FUNCTOR"
  (cond
    ((logic-var-p resolved-term)
     (cond
       ((logic-var-p resolved-name)
        (%raise-instantiation-error
         environment operation "functor/3 requires an instantiated name"))
       ((logic-var-p resolved-arity)
        (%raise-instantiation-error
         environment operation "functor/3 requires an instantiated arity"))
       ((not (or (%term-atom-p resolved-name)
                 (%prolog-number-p resolved-name)
                 (stringp resolved-name)))
        (%raise-type-error
         "ATOMIC" resolved-name environment operation
         "functor/3 name must be atomic"))
       ((not (integerp resolved-arity))
        (%raise-type-error
         "INTEGER" resolved-arity environment operation
         "functor/3 arity must be an integer"))
       ((> resolved-arity *max-prolog-term-arity*)
        ;; ISO 13211-1 8.5.1.3: an arity past what the implementation can build
        ;; is a representation_error naming the flag that reports the bound.
        (%raise-representation-error
         "MAX_ARITY" environment operation
         "functor/3 arity exceeds the max_arity flag"))
       ((minusp resolved-arity)
        (%raise-domain-error
         "NOT_LESS_THAN_ZERO" resolved-arity environment operation
         "functor/3 arity must not be negative"))
       ((and (plusp resolved-arity) (not (%term-atom-p resolved-name)))
        (%raise-type-error
         "ATOM" resolved-name environment operation
         "functor/3 compound name must be an atom"))
       (t
        (let ((constructed
                (if (zerop resolved-arity)
                    resolved-name
                    (cons resolved-name
                          (loop repeat resolved-arity
                                collect (fresh-logic-variable))))))
          (%term-unify-sequence
           (list (cons term constructed)
                 (cons name resolved-name)
                 (cons arity resolved-arity))
           environment emit)))))
    ((or (%term-atom-p resolved-term) (%prolog-number-p resolved-term)
         (stringp resolved-term))
     (%term-unify-sequence
      (list (cons name resolved-term) (cons arity 0)) environment emit))
    ((%term-compound-p resolved-term)
     (%term-unify-sequence
      (list (cons name (first resolved-term))
            (cons arity (length (rest resolved-term))))
      environment emit))
    ;; A cons that is not a proper list is a partial list, and only that: a
    ;; compound is always proper here, so there is no ambiguity to resolve.
    ;; ISO 13211-1 makes a list cell the compound `'.'(Head, Tail)'.
    ((consp resolved-term)
     (%term-unify-sequence
      (list (cons name (%intern-prolog-atom "."))
            (cons arity 2))
      environment emit))
    (t
     (%raise-type-error
      "CALLABLE" resolved-term environment operation
      "functor/3 term must be a Prolog term"))))

(define-iso-builtin (arg index term (argument :raw)) "ARG"
  (%require-bounded-integer resolved-index environment operation "arg/3 index")
  (cond
      ((logic-var-p resolved-term)
       (%raise-instantiation-error
        environment operation "arg/3 requires an instantiated term"))
      ((not (%term-compound-p resolved-term))
       (%raise-type-error
        "COMPOUND" resolved-term environment operation
        "arg/3 term must be compound"))
      ((and (plusp resolved-index)
            (<= resolved-index (length (rest resolved-term))))
       (%unify-emit argument (nth resolved-index resolved-term) environment emit))))

(define-iso-builtin (copy_term source (copy :raw)) "COPY_TERM"
  (declare (cl:ignore operation))
  (%unify-emit copy
               (%freshen-term resolved-source (make-hash-table :test #'eq))
               environment emit))

(define-iso-builtin (numbervars term start (end :raw)) "NUMBERVARS"
  (%require-bounded-integer resolved-start environment operation "numbervars/3 start")
  (let ((variables (%collect-variables resolved-term)))
    (%term-unify-sequence
     (append
      (loop for variable in variables
            for index from resolved-start
            collect (cons variable (list +numbervars-functor+ index)))
      (list (cons end (+ resolved-start (length variables)))))
     environment emit)))

(define-iso-builtin (|=..| term list) "UNIV"
  (cond
      ((not (logic-var-p resolved-term))
       (let ((decomposition
               (cond
                 ((or (%term-atom-p resolved-term)
                      (%prolog-number-p resolved-term)
                      (stringp resolved-term))
                  (list resolved-term))
                 ((%term-compound-p resolved-term) resolved-term))))
         (if decomposition
             (%unify-emit list decomposition environment emit)
             (%raise-type-error
              "CALLABLE" resolved-term environment operation
              "=../2 term must be a Prolog term"))))
      ((logic-var-p resolved-list)
       (%raise-instantiation-error
        environment operation "=../2 requires at least one instantiated argument"))
      ((not (%proper-list-p resolved-list))
       (%raise-type-error
        "LIST" resolved-list environment operation
        "=../2 second argument must be a proper list"))
      ((null resolved-list)
       (%raise-domain-error
        "NON_EMPTY_LIST" resolved-list environment operation
        "=../2 second argument must not be empty"))
      (t
       (let ((head (first resolved-list))
             (arguments (rest resolved-list)))
         (cond
           ((logic-var-p head)
            (%raise-instantiation-error
             environment operation "=../2 requires an instantiated functor"))
           ((and arguments (not (%term-atom-p head)))
            (%raise-type-error
             "ATOM" head environment operation
             "=../2 compound functor must be an atom"))
           ((and (null arguments)
                 (not (or (%term-atom-p head) (%prolog-number-p head)
                          (stringp head))))
            (%raise-type-error
             "ATOMIC" head environment operation
             "=../2 singleton list must contain an atomic term"))
           (t
            (%unify-emit term
                         (if arguments resolved-list head)
                         environment emit)))))))
