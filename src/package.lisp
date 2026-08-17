(defpackage #:cl-prolog-kit.user-atoms
  (:use)
  (:documentation
   "Interned Prolog atoms kept separate from CL-PROLOG-KIT symbols, including
inherited Common Lisp names and quoted names reserved for logic-variable
syntax."))

(defpackage #:cl-prolog-kit.verbatim-atoms
  (:use)
  (:documentation
   "Prolog atoms whose text holds an upper-case letter, interned under that
exact text rather than the upcased spelling CL-PROLOG-KIT uses for a lower-case
name.  The home package is what tells the two encodings apart, so `'ABC'' here
and CL-PROLOG-KIT::ABC (the atom `abc') stay distinct.  See src/atom-name.lisp."))

(defpackage #:cl-prolog-kit
  (:use #:cl)
  (:shadow #:! #:assert #:catch #:throw)
  (:documentation
   "A small, dependency-free Prolog engine.

Rulebases are plain data (see PROLOG, DEFINE-RULEBASE), proof search is
continuation-passing (see MAP-PROLOG-SOLUTIONS), and the builtin goal set
is extensible (see DEFINE-FOREIGN-PREDICATE).")
  (:export
   ;; data
   #:clause
   #:clause-p
   #:clause-head
   #:clause-body
   #:make-clause
   #:rulebase
   #:rulebase-p
   #:rulebase-visible-clauses
   #:make-rulebase
   #:copy-rulebase
   #:rulebase-extend
   #:rulebase-insert-clause!
   ;; atoms
   #:prolog-atom
   #:prolog-atom-text
   ;; unification
   #:logic-var-p
   #:fresh-logic-variable
   #:unify
   #:logic-substitute
   ;; engine
   #:*max-prolog-depth*
   #:invalid-max-depth-error
   #:invalid-max-depth-error-value
   #:prolog-depth-limit-exceeded
   #:prolog-depth-limit-exceeded-goal
   #:invalid-goal-error
   #:invalid-goal-error-goal
   #:prolog-exception
   #:prolog-exception-term
   #:prolog-runtime-error
   #:prolog-instantiation-error
   #:prolog-type-error
   #:prolog-domain-error
   #:prolog-permission-error
   #:prolog-existence-error
   #:prolog-evaluation-error
   #:prolog-resource-error
   #:prolog-representation-error
   #:prolog-syntax-error
   #:prolog-halt
   #:prolog-halt-code
   #:arithmetic-evaluation-error
   #:arithmetic-error-expression
   #:arithmetic-error-reason
   #:define-foreign-predicate
   ;; queries
   #:map-prolog-solutions
   #:query-prolog
   #:query-prolog-first
   #:prolog-succeeds-p
   #:solution-binding
   ;; text parser
   #:prolog-parser-resource-error
   #:prolog-parser-resource-error-resource
   #:prolog-parser-resource-error-limit
   #:prolog-parser-resource-error-observed
   #:prolog-parser-resource-error-position
   #:*max-prolog-source-characters*
   #:*max-prolog-delimiter-depth*
   #:*max-prolog-parser-depth*
   #:*max-prolog-tokens*
   #:*max-prolog-identifier-length*
   #:*max-prolog-quoted-lexeme-length*
   #:*max-prolog-numeric-lexeme-length*
   #:*max-prolog-interned-symbols*
   #:*max-prolog-builtin-output-length*
   #:read-prolog-term
   #:read-prolog-clause
   #:write-prolog-term
   #:prolog-term-string
   #:parse-prolog
   #:consult-prolog
   #:ensure-prolog-loaded
   #:consult
   #:ensure_loaded
   #:load_files
   ;; rule DSL
   #:prolog
   #:define-rulebase
   #:extend-rulebase
   #:def-rule
   #:with-prolog-query
   #:prolog-match
   ;; builtin goal names
   #:!
   #:call
   #:call_nth
   #:call_with_depth_limit
   #:once
   #:setup_call_cleanup
   #:call_cleanup
   #:forall
   #:if-then-else
   #:soft-if-then-else
   #:catch
   #:throw
   #:unify_with_occurs_check
   #:repeat
   #:findall
   #:bagof
   #:setof
   #:sort
   #:msort
   #:keysort
   #:true
   #:fail
   #:false
   #:|\\+|
   #:asserta
   #:assert
   #:assertz
   #:retract
   #:retractall
   #:current_predicate
   #:predicate_property
   #:abolish
   #:clause
   #:|\\=|
   #:is
   #:in
   #:ins
   #:|..|
   #:|#=|
   #:|#\\=|
   #:|#<|
   #:|#=<|
   #:|#>|
   #:|#>=|
   #:all_different
   #:labeling
   #:indomain
   #:|=:=|
   #:|=\\=|
   #:<
   #:=<
   #:>
   #:>=
   #:var
   #:nonvar
   #:atom
   #:atomic
   #:number
   #:integer
   #:float
   #:==
   #:|\\==|
   #:@<
   #:@=<
   #:@>
   #:@>=
   #:compare
   #:unifiable
   #:term_variables
   #:compound
   #:callable
   #:ground
   #:acyclic_term
   #:cyclic_term
   #:functor
   #:arg
   #:copy_term
   #:numbervars
   #:|=..|
   ;; library(apply) / library(lists) extensions
   #:maplist
   #:foldl
   #:include
   #:exclude
   #:partition
   #:permutation
   #:subtract
   #:union
   #:intersection
   #:list_to_set
   #:numlist
   #:sum_list
   #:sumlist
   ;; arithmetic relations
   #:plus
   #:succ
   ;; predsort / aggregate_all
   #:predsort
   #:aggregate_all
   ;; library(assoc)
   #:empty_assoc
   #:get_assoc
   #:put_assoc
   #:del_assoc
   #:list_to_assoc
   #:assoc_to_list
   #:assoc_to_keys
   #:assoc_to_values
   ;; library(pairs)
   #:pairs_keys_values
   #:pairs_keys
   #:pairs_values
   ;; character classification / case folding
   #:char_type
   #:code_type
   #:upcase_atom
   #:downcase_atom
   ;; string type and conversions
   #:string_length
   #:string_concat
   #:atom_string
   #:string_to_atom
   #:number_string
   #:string_chars
   #:string_codes
   #:term_string
   #:text_concat
   #:sub_string
   #:split_string
   ;; term <-> atom I/O and output
   #:term_to_atom
   #:read_term_from_atom
   #:print
   #:format
   #:tab
   ;; DCG
   #:def-dcg-rule
   #:phrase
   #:phrase-all
   #:dcg-alt
   #:dcg-opt
   #:dcg-star
   #:dcg-plus
   #:dcg-error-recovery
   #:dcg-token-match
   #:dcg-token-match-value))
