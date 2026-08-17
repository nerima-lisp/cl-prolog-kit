;;;; ISO 13211-1 conformance cases, written as the standard states them.
;;;;
;;;; Every case is a goal in Prolog *source text* paired with the outcome the
;;;; standard requires -- a proof, a failure, or a specific error term. Writing
;;;; them as text rather than as engine data structures is the point: it is the
;;;; only way to exercise the reader, the operator table, and the builtin error
;;;; contracts together, and it is how a user meets them.
;;;;
;;;; Each case cites its subclause, so a failure says which requirement broke.
;;;; This file is deliberately broad rather than deep; the per-builtin suites
;;;; carry the exhaustive behavioural coverage.

(in-package #:cl-prolog-kit.tests)

;;; 6.3-6.4 -- syntax: what the reader must accept and how tokens are formed.

(deftest-iso iso-syntax
  ;; 6.3.3.1 / 6.3.4.3: an atom that is an operator is a term as an argument,
  ;; and bracketed.
  ("6.3.3.1" "functor(T, +, 2), T = 1 + 2" :true)
  ("6.3.3.1" "T =.. [+, 1, 2], T == 1 + 2" :true)
  ("6.3.3.1" "atom_length(-, 1)" :true)
  ("6.3.3.1" "sort(0, @<, [b,a], [a,b])" :true)
  ("6.3.3.1" "compare(<, 1, 2)" :true)
  ("6.3.4.3" "X = (+), X == '+'" :true)
  ("6.3.4.3" "X = (:-), X == ':-'" :true)
  ;; 6.3.5: [] is an atom, and quoting does not change which atom.
  ("6.3.5" "atom([])" :true)
  ("6.3.5" "[] == '[]'" :true)
  ("6.3.5" "atom_length([], 2)" :true)
  ;; 6.3.6: a bare {} is the atom of that name.
  ("6.3.6" "X = {}, atom(X)" :true)
  ("6.3.6" "{} == '{}'" :true)
  ;; 6.4.2: quoting is invisible; case is not.
  ("6.4.2" "hello == 'hello'" :true)
  ("6.4.2" "'FooBar' \\== foobar" :true)
  ;; 6.4.2: a maximal run of graphic characters is one token, and an undeclared
  ;; one is an atom -- so an operator can be declared before it exists.
  ("6.4.2" "op(700, xfx, ===)" :true)
  ("6.4.2" "atom_length(===, 3)" :true)
  ;; 6.4.2.1: escape sequences denote the character they name.
  ("6.4.2.1" "atom_codes('a\\nb', [0'a, 10, 0'b])" :true)
  ("6.4.2.1" "atom_length('a\\tb', 3)" :true)
  ("6.4.2.1" "atom_codes('\\x41\\', [65])" :true)
  ("6.4.2.1" "atom_codes('\\101\\', [65])" :true)
  ;; 6.4.4: character-code and radix constants.
  ("6.4.4" "0'a =:= 97" :true)
  ("6.4.4" "0'' =:= 39" :true)
  ("6.4.4" "0'\\n =:= 10" :true)
  ("6.4.4" "0xff =:= 255" :true)
  ("6.4.4" "0o17 =:= 15" :true)
  ("6.4.4" "0b101 =:= 5" :true)
  ;; 6.3.4.4 table 7: the bitwise operators are declared.
  ("6.3.4.4" "1 << 3 =:= 8" :true)
  ("6.3.4.4" "8 >> 3 =:= 1" :true)
  ("6.3.4.4" "12 /\\ 10 =:= 8" :true)
  ("6.3.4.4" "12 \\/ 10 =:= 14" :true)
  ("6.3.4.4" "12 xor 10 =:= 6" :true)
  ("6.3.4.4" "\\ 0 =:= -1" :true))

;;; 7.2 -- the standard order of terms.

(deftest-iso iso-term-order
  ("7.2.3" "compare(<, 'B', a)" :true)
  ("7.2.3" "a @< b" :true)
  ("7.2.4" "compare(<, 1, a)" :true)
  ("7.2" "msort([b, 'A', a], ['A', a, b])" :true)
  ("7.2" "sort([b, a, b], [a, b])" :true)
  ;; == and compare/3 must agree with the unification =/2 admits.
  ("7.2" "compare(=, hello, 'hello')" :true))

;;; 7.6.1 -- converting a term to a clause.

(deftest-iso iso-clause-conversion
  ("7.6.1" "assertz((h :- true)), h" :true)
  ("7.6.1" "assertz((h(X) :- X > 1)), h(2)" :true)
  ("7.6.1" "assertz((h(X) :- X > 1)), \\+ h(0)" :true)
  ("7.6.1" "assertz(f), clause(f, true)" :true)
  ("7.6.1" "assertz((g :- a, b)), clause(g, (a , b))" :true)
  ("7.6.1" "assertz((d :- true)), retract((d :- true)), \\+ d" :true)
  ("7.6.1" "assertz(p), retract((p :- true)), \\+ p" :true))

;;; 8.x -- the builtin error contracts the standard specifies exactly.

(deftest-iso iso-builtin-errors
  ;; 8.2 unification
  ("8.2.2" "unify_with_occurs_check(X, f(X))" :false)
  ;; 8.4 comparison
  ("8.4.2.3" "compare(1, 1, 2)" "type_error(atom")
  ("8.4.2.3" "compare(foo, 1, 2)" "domain_error(order")
  ;; 8.5 term construction and decomposition
  ("8.5.1.3" "functor(T, N, 2)" "instantiation_error")
  ("8.5.1.3" "functor(T, foo, a)" "type_error(integer")
  ("8.5.1.3" "functor(T, foo, -1)" "domain_error")
  ("8.5.1.3" "functor(T, 1.5, 1)" "type_error(atom")
  ("8.5.2.3" "arg(N, f(a), A)" "instantiation_error")
  ("8.5.2.3" "arg(a, f(a), A)" "type_error(integer")
  ("8.5.3.3" "X =.. Y" "instantiation_error")
  ("8.5.3.3" "X =.. [foo|bar]" "type_error(list")
  ("8.5.3.3" "X =.. []" "domain_error")
  ;; 8.6 arithmetic evaluation
  ("8.6.1.3" "X is Y" "instantiation_error")
  ("8.6.1.3" "X is foo" "type_error(evaluable")
  ("8.6.1.3" "X is 1 // 0" "evaluation_error(zero_divisor")
  ("8.6.1.3" "X is 1 mod 0" "evaluation_error(zero_divisor")
  ("8.7.1.3" "1 =:= Y" "instantiation_error")
  ;; 8.8 clause retrieval and inspection
  ("8.8.1.3" "clause(H, B)" "instantiation_error")
  ("8.8.1.3" "clause(1, B)" "type_error(callable")
  ("8.8.1.3" "clause(foo, 1)" "type_error(callable")
  ("8.8.2.3" "current_predicate(1)" "type_error")
  ;; 8.9 clause creation and destruction
  ("8.9.1.3" "asserta(X)" "instantiation_error")
  ("8.9.1.3" "asserta(1)" "type_error(callable")
  ("8.9.1.3" "asserta((X :- true))" "instantiation_error")
  ("8.9.1.3" "asserta((foo :- 1))" "type_error(callable")
  ("8.9.3.3" "retract(X)" "instantiation_error")
  ("8.9.4.3" "abolish(foo)" "type_error(predicate_indicator")
  ;; 8.10 findall and friends
  ("8.10.1.3" "findall(X, G, L)" "instantiation_error")
  ("8.10.1.3" "findall(X, 1, L)" "type_error(callable")
  ("8.10.1" "findall(X, fail, [])" :true)
  ("8.10.2" "bagof(X, fail, L)" :false)
  ("8.10.2" "bagof(X, Y^member(X-Y,[1-a,2-b]), [1,2])" :true)
  ("8.10.3" "setof(X, member(X,[b,a,b]), [a,b])" :true)
  ;; 8.11 stream selection
  ("8.11.1.3" "current_input(foo)" "domain_error(stream")
  ("8.11.1.3" "current_output(foo)" "domain_error(stream")
  ;; 8.14 term input/output
  ("8.14.1.3" "read_term_from_atom('a', T, [bogus])" "domain_error")
  ;; 8.15 logic and control
  ("8.15.1.3" "call(G)" "instantiation_error")
  ("8.15.1.3" "call(1)" "type_error(callable")
  ("8.15.1" "\\+ fail" :true)
  ("8.15.2" "once(member(X,[a,b])), X == a" :true)
  ("8.15" "catch(throw(ball), B, B == ball)" :true)
  ("8.15" "throw(X)" "instantiation_error")
  ;; 8.16 atomic term processing
  ("8.16.1.3" "atom_length(A, L)" "instantiation_error")
  ("8.16.1.3" "atom_length(1, L)" "type_error(atom")
  ("8.16.1.3" "atom_length(abc, -1)" "domain_error")
  ("8.16.2.3" "atom_concat(A, B, C)" "instantiation_error")
  ("8.16.3" "sub_atom(abracadabra, 0, 5, _, abrac)" :true)
  ("8.16.4.3" "atom_chars(A, L)" "instantiation_error")
  ("8.16.5" "atom_codes(A, [0'a]), A == a" :true)
  ("8.16.6.3" "char_code(C, N)" "instantiation_error")
  ;; 8.16.7.3 / 8.16.8.3: text that spells no number is a syntax error.
  ("8.16.7.3" "number_chars(N, ['a'])" "syntax_error")
  ("8.16.8.3" "number_codes(N, \"bad\")" "syntax_error")
  ("8.16.8" "number_codes(N, \"33\"), N == 33" :true)
  ;; 8.17 implementation-defined hooks
  ("8.17.1.3" "op(1300, xfx, bad)" "domain_error(operator_priority")
  ("8.17.1.3" "op(700, bad, x)" "domain_error(operator_specifier")
  ("8.17.1.3" "op(700, xfx, ',')" "permission_error")
  ("8.17.2.3" "current_prolog_flag(1, V)" "type_error")
  ("8.17.3.3" "set_prolog_flag(bounded, true)" "permission_error"))

;;; Divergences this engine documents rather than fixes.  They are asserted so
;;; that a change of behaviour shows up as a failure here and gets a decision,
;;; rather than drifting silently.

(deftest-iso iso-documented-divergences
  ;; 6.3.6: a non-empty {T} is the engine's internal `brace' term, not the
  ;; compound '{}'(T), because DCG bodies are built on that representation.
  ("6.3.6*" "X = {a}, X = {}(a)" "uncaught host condition")
  ;; 8.2.1: the occurs_check flag defaults to true here, so =/2 refuses the
  ;; cyclic binding ISO would let it make.  See docs/src/reference/semantics.md.
  ("8.2.1*" "X = f(X)" :false)
  ;; 6.3.3: a functor is not required to be followed immediately by `(', so a
  ;; bare +(1,2) reads as the prefix operator applied to (1,2) and the writer
  ;; quotes such a functor to keep write_canonical/1 output re-readable.
  ("6.3.3*" "X = +(1,2), X == 1 + 2" :false)
  ;; 6.5: the character set is implementation-defined, and this engine chooses
  ;; the host's -- Unicode.  A code past 255 is therefore a character, not a
  ;; representation_error.  The 1999 conformance corpus assumes a 256-character
  ;; set and expects the error; passing it would cost the engine every atom
  ;; that is not Latin-1.  Asserted so the choice stays a choice.
  ("6.5*" "atom_codes(A, [12354]), atom_length(A, 1)" :true)
  ("6.5*" "char_code(C, 12354), atom_length(C, 1)" :true))

;;; 9.x -- evaluable functors.  The standard fixes results and rounding exactly,
;;; and this is where systems most often quietly differ from each other.

(deftest-iso iso-arithmetic
  ;; 9.3.1 vs 9.3.10: `**' is the float power, `^' preserves integers.
  ("9.3.1" "X is 2 ** 3, X == 8.0" :true)
  ("9.3.1" "X is 2 ** 0.5, X > 1.41" :true)
  ("9.3.10" "X is 2 ^ 3, X == 8" :true)
  ("9.3.10" "X is 2 ^ (-1)" "type_error(float")
  ("9.3.10" "X is 1 ^ (-1), X == 1" :true)
  ;; 9.1.3: dividing integers exactly stays integral.
  ("9.1.3" "X is 4 / 2, X == 2" :true)
  ("9.1.3" "X is 1 / 2, X == 0.5" :true)
  ;; 9.1.3 with integer_rounding_function = toward_zero.
  ("9.1.3" "X is 7 // -2, X == -3" :true)
  ("9.1.3" "X is 7 rem -2, X == 1" :true)
  ("9.1.3" "X is 7 mod -2, X == -1" :true)
  ("9.1.3" "X is -7 div 2, X == -4" :true)
  ;; 9.1.6 the type-preserving and rounding functions.
  ("9.1.6" "X is abs(-3), X == 3" :true)
  ("9.1.6" "X is sign(-3), X == -1" :true)
  ("9.1.6" "X is sign(-3.0), X == -1.0" :true)
  ("9.1.6" "X is min(2, 3), X == 2" :true)
  ("9.1.6" "X is truncate(1.5), X == 1" :true)
  ("9.1.6" "X is round(1.5), X == 2" :true)
  ("9.1.6" "X is ceiling(1.1), X == 2" :true)
  ("9.1.6" "X is floor(-1.1), X == -2" :true)
  ("9.1.6" "X is float(1), X == 1.0" :true)
  ("9.1.6" "X is float_integer_part(1.5), X == 1.0" :true)
  ("9.1.6" "X is float_fractional_part(1.5), X == 0.5" :true)
  ("9.1.6" "X is pi, X > 3.14" :true)
  ("9.1.2" "X is gcd(12, 8), X == 4" :true)
  ;; 9.1.4.2: the evaluation errors, all catchable.
  ("9.3.10" "X is sqrt(-1.0)" "evaluation_error(undefined")
  ("9.3.7" "X is log(0.0)" "evaluation_error(undefined")
  ("9.1.4.2" "X is 1.0 / 0.0" "evaluation_error(zero_divisor")
  ("9.1.4.2" "X is 1 / 0" "evaluation_error(zero_divisor")
  ("9.1.4.2" "X is 1.0e300 * 1.0e300" "evaluation_error(float_overflow")
  ("9.1.4.2" "X is exp(1000.0)" "evaluation_error(float_overflow")
  ;; The exponent-magnitude resource bound is this engine's own DoS guard and
  ;; fires before the float would overflow.
  ("9.1.4.2*" "X is 2.0 ** 100000" "resource_error(exponent_magnitude")
  ("9.1.1" "X is foo(1)" "type_error(evaluable")
  ("9.1.1" "X is [1]" "type_error"))

;;; 8.10 -- all-solutions predicates, whose free-variable grouping is subtle
;;; enough that an engine can look right on the simple cases and still be wrong.

(deftest-iso iso-all-solutions
  ("8.10.1" "findall(X, member(X,[a,a]), [a,a])" :true)
  ("8.10.1" "findall(X, fail, [])" :true)
  ("8.10.2" "bagof(X, member(X-Y,[1-a,2-b]), L), Y == a, L == [1]" :true)
  ("8.10.2" "findall(Y-L, bagof(X, member(X-Y,[1-a,2-b]), L), [a-[1], b-[2]])" :true)
  ("8.10.2" "bagof(X, Y^member(X-Y,[1-a,2-b]), [1,2])" :true)
  ("8.10.2" "bagof(X, fail, L)" :false)
  ("8.10.3" "setof(X, member(X,[c,a,b,a]), [a,b,c])" :true)
  ("8.10.3" "findall(Y-L, setof(X, member(X-Y,[2-a,1-a]), L), [a-[1,2]])" :true)
  ("8.10.4" "forall(member(X,[1,2]), X > 0)" :true)
  ("8.10.4" "forall(member(X,[1,-2]), X > 0)" :false))

;;; 7.8 -- control constructs.

(deftest-iso iso-control
  ("7.8.1" "true" :true)
  ("7.8.2" "fail" :false)
  ;; 7.8.7: if-then is its own construct and fails when the condition fails --
  ;; it is not only the left half of if-then-else.
  ("7.8.7" "( fail -> true )" :false)
  ("7.8.7" "( true -> true )" :true)
  ("7.8.7" "( member(X,[1,2]) -> X == 1 )" :true)
  ("7.8.8" "( fail -> true ; true )" :true)
  ("7.8.8" "( member(X,[1,2]) -> X == 1 ; fail )" :true)
  ;; The soft cut keeps every solution of its condition.
  ("7.8.8" "findall(X, ( member(X,[1,2]) *-> true ; fail ), [1,2])" :true)
  ("7.8.8" "( fail *-> true ; true )" :true)
  ("7.8.7" "( fail *-> true )" :false)
  ;; 7.8.4: a cut inside call/1 is local to it.
  ("7.8.4" "( call((!, fail)) ; true )" :true)
  ("7.8.3" "call(plus(1), 2, X), X == 3" :true)
  ("7.8.9" "catch(throw(f(1)), f(Y), Y == 1)" :true)
  ("7.8.9" "catch(catch(throw(a), b, true), a, true)" :true))

;;; 8.17 -- the flags the standard requires an implementation to report.

(deftest-iso iso-flags
  ("8.17.2" "current_prolog_flag(bounded, false)" :true)
  ("8.17.2" "current_prolog_flag(integer_rounding_function, toward_zero)" :true)
  ("8.17.2" "current_prolog_flag(double_quotes, codes)" :true)
  ("8.17.2" "current_prolog_flag(max_arity, A)" :true)
  ("8.17.2.3" "current_prolog_flag(bogus_flag, V)" "domain_error(prolog_flag"))

;;; 8.4 -- sorting, whose stability and error contracts are specified.

(deftest-iso iso-sorting
  ("8.4.3" "keysort([b-1,a-2,b-3], [a-2, b-1, b-3])" :true)
  ("8.4.3" "keysort([a], L)" "type_error(pair")
  ("8.4.4" "sort([b,a,b], [a,b])" :true)
  ("8.4.4" "msort([b,a,b], [a,b,b])" :true))

;;; 8.11-8.13 -- streams.  The error class here is load-bearing: a program that
;;; misspells an alias must be able to tell that from an ordinary failure.

(deftest-iso iso-streams
  ("8.11.5.3" "set_input(S)" "instantiation_error")
  ("8.11.5.3" "set_input(1)" "domain_error(stream_or_alias")
  ("8.11.5.3" "set_input(nosuch)" "existence_error(stream")
  ("8.11.6.3" "set_output(nosuch)" "existence_error(stream")
  ("8.11.6.3" "open(F, read, S)" "instantiation_error")
  ("8.11.6.3" "open('/tmp/cl-prolog-kit-nonexistent', 1, S)" "type_error(atom")
  ("8.11.6.3" "open('/tmp/cl-prolog-kit-nonexistent', bogus, S)" "domain_error(io_mode")
  ("8.11.6.3" "open('/tmp/cl-prolog-kit-nonexistent', read, S, bogus)" "type_error(list")
  ("8.11.7.3" "close(S)" "instantiation_error")
  ("8.11.7.3" "close(nosuch)" "existence_error(stream")
  ("8.11.8" "stream_property(S, P), nonvar(S)" :true)
  ("8.11.8.3" "stream_property(user_input, bogus)" "domain_error(stream_property")
  ("8.11.9.3" "flush_output(nosuch)" "existence_error(stream")
  ("8.11.9.3" "at_end_of_stream(nosuch)" "existence_error(stream")
  ("8.11.10.3" "set_stream_position(user_input, P)" "instantiation_error")
  ;; 8.12 character I/O.
  ("8.12.1.3" "get_char(nosuch, C)" "existence_error(stream")
  ("8.12.1.3" "get_char(user_input, 1)" "type_error(in_character")
  ("8.12.2.3" "peek_char(user_input, 1)" "type_error(in_character")
  ("8.12.3.3" "put_char(C)" "instantiation_error")
  ("8.12.3.3" "put_char(1)" "type_error(character")
  ("8.12.3.3" "put_char(ab)" "type_error(character")
  ("8.12.4.3" "nl(nosuch)" "existence_error(stream")
  ;; 8.13 byte I/O.  The argument is checked before the stream's type, and the
  ;; permission error names the stream's actual type as the culprit.
  ("8.13.1.3" "get_byte(user_input, a)" "type_error(in_byte")
  ("8.13.3.3" "put_byte(a)" "type_error(byte")
  ("8.13.3.3" "put_byte(256)" "type_error(byte")
  ("8.13.3.3" "put_byte(1)" "permission_error(output,text_stream"))

;;; 8.14 -- term input/output options, op/3 and char_conversion/2.

(deftest-iso iso-term-io
  ("8.14.1.3" "read_term_from_atom('a', T, O)" "instantiation_error")
  ("8.14.1.3" "read_term_from_atom('a', T, bogus)" "type_error(list")
  ("8.14.1" "read_term_from_atom('f(X)', T, [variable_names(V)]), V = ['X'=_]" :true)
  ("8.14.1" "read_term_from_atom('f(X,Y)', T, [variables(V)]), V = [_,_]" :true)
  ("8.14.1" "read_term_from_atom('f(X,Y,Y)', T, [singletons(S)]), S = ['X'=_]" :true)
  ("8.14.2.3" "write_term(user_output, foo, bogus)" "type_error(list")
  ("8.14.2.3" "write_term(user_output, foo, [bogus(true)])" "domain_error(write_option")
  ("8.14.3.3" "op(P, xfx, foo)" "instantiation_error")
  ("8.14.3.3" "op(700, xfx, 1)" "type_error(list")
  ("8.14.3" "op(700, xfx, [a,b]), current_op(700, xfx, a)" :true)
  ("8.14.4.3" "current_op(a, xfx, foo)" "type_error(integer")
  ("8.14.4.3" "current_op(700, bogus, foo)" "domain_error(operator_specifier")
  ("8.14.4.3" "current_op(P, T, 1)" "type_error(atom")
  ("8.14.5.3" "char_conversion(A, b)" "instantiation_error")
  ("8.14.6.3" "current_char_conversion(1, B)" "type_error(character"))

;;; 8.3 and 8.16 -- type testing and atom processing, including the
;;; enumeration modes, where a missing solution is invisible without a count.

(deftest-iso iso-type-testing-and-atoms
  ("8.3.1" "var(_)" :true)
  ("8.3.2" "\\+ atom(\"x\")" :true)
  ("8.3.3" "\\+ integer(1.0)" :true)
  ("8.3.4" "float(1.0)" :true)
  ("8.3.5" "atomic(1)" :true)
  ("8.3.5" "\\+ atomic(f(x))" :true)
  ("8.3.6" "compound([a])" :true)
  ("8.3.6" "\\+ compound([])" :true)
  ("8.3.7" "nonvar(f(_))" :true)
  ("8.3.8" "number(1.0)" :true)
  ("8.3.9" "callable(foo)" :true)
  ("8.3.9" "callable(f(x))" :true)
  ("8.3.9" "\\+ callable(1)" :true)
  ("8.3.10" "\\+ is_list([a|_])" :true)
  ;; abc has 3+2+1 substrings plus the 4 empty ones: 10 solutions in all.
  ("8.16.3" "findall(S, sub_atom(abc, _, _, _, S), L), length(L, 10)" :true)
  ("8.16.2" "findall(A-B, atom_concat(A, B, abc), L), length(L, 4)" :true)
  ("8.16.1.3" "atom_length(\"ab\", L)" "type_error")
  ("8.16.4.3" "atom_chars(A, [a|_])" "instantiation_error")
  ("8.16.6.3" "char_code(ab, C)" "type_error(character")
  ("8.16.6.3" "char_code(C, -1)" "representation_error(character_code"))

;;; 7.11.2.4 -- the `unknown' flag decides what calling an undefined procedure
;;; does, which is what makes a partially written program runnable.

(deftest-iso iso-unknown-flag
  ("7.11.2.4" "current_prolog_flag(unknown, error)" :true)
  ("7.7.7" "undefined_procedure_xyz" "existence_error(procedure")
  ("7.11.2.4" "set_prolog_flag(unknown, fail), undefined_procedure_xyz" :false)
  ("7.11.2.4" "set_prolog_flag(unknown, fail), !(_)" :false)
  ("7.11.2.4" "set_prolog_flag(unknown, warning), undefined_procedure_xyz" :false)
  ;; ...and the default is restored per rulebase, not left global.
  ("7.11.2.4" "undefined_procedure_xyz" "existence_error(procedure"))
