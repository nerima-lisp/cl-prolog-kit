;;;; PHRASE-TOKEN-VALID-P and PHRASE-FILTER-CANDIDATES.
;;;;
;;;; Expected values come from phrase/2 on concrete inputs (the oracle), or
;;;; are written out by hand; none is computed with the functions under test.

(in-package #:cl-prolog-kit.tests)

(defstruct (token-check-oracle (:constructor %make-token-check-oracle))
  rulebase universe continuation-length (cache (make-hash-table :test #'equal)))

(defun make-token-check-oracle (source universe continuation-length)
  (%make-token-check-oracle :rulebase (consult-prolog source)
                            :universe (read-prolog-term universe)
                            :continuation-length continuation-length))

(defun token-check-oracle-phrase-p (oracle nonterminal input)
  "phrase/2 on the ground INPUT."
  (let ((key (prin1-to-string (list nonterminal input))))
    (multiple-value-bind (value present-p)
        (gethash key (token-check-oracle-cache oracle))
      (if present-p
          value
          (setf (gethash key (token-check-oracle-cache oracle))
                (nth-value 1 (query-prolog-first
                              (token-check-oracle-rulebase oracle)
                              (list (read-prolog-term "phrase") nonterminal input)
                              :max-inferences 20000)))))))

(defun token-check-oracle-continuations (oracle)
  "Every token list over the universe up to the oracle's length."
  (let ((universe (token-check-oracle-universe oracle)))
    (loop with all = (list '())
          with layer = (list '())
          repeat (token-check-oracle-continuation-length oracle)
          do (setf layer (loop for continuation in layer
                               append (loop for token in universe
                                            collect (cons token continuation))))
             (setf all (append all layer))
          finally (return all))))

(defun token-check-oracle-valid-p (oracle nonterminal prefix token)
  "True when PREFIX ++ [TOKEN|S] is a phrase for some continuation S."
  (loop for continuation in (token-check-oracle-continuations oracle)
        thereis (token-check-oracle-phrase-p
                 oracle nonterminal (append prefix (cons token continuation)))))

(defun token-check-missed (oracle nonterminal prefix)
  "The universe tokens the oracle accepts after PREFIX but the check rejects."
  (loop for token in (token-check-oracle-universe oracle)
        when (and (token-check-oracle-valid-p oracle nonterminal prefix token)
                  (not (phrase-token-valid-p (token-check-oracle-rulebase oracle)
                                             nonterminal prefix token
                                             :max-inferences 200000)))
          collect token))

(defun token-check-oracle-count (oracle cases)
  "How many (case, token) pairs the oracle accepts, to keep checks non-vacuous."
  (loop for (nonterminal prefix) in cases
        sum (loop for token in (token-check-oracle-universe oracle)
                  count (token-check-oracle-valid-p oracle nonterminal prefix token))))

(defparameter *token-check-corpus*
  "choose(a) :- !.
choose(b).
c1 --> [X], { choose(X) }.
c2 --> [X], { once(member(X, [a,b])) }.
c3 --> [X], { (X = a -> true ; X = b) }.
c4 --> [f(X)], { \\+ (X = a) }.
br(main).
br(dev).
gen_after --> [B], { br(B) }.
gen_before --> { br(B) }, [B].
flag('-a').
flag('-b').
flag('-v').
excl('-a', '-b').
excl('-b', '-a').
ex_state --> st([]).
st(_) --> [].
st(S) --> ['-a'], { \\+ memberchk(b, S) }, st([a|S]).
st(S) --> ['-b'], { \\+ memberchk(a, S) }, st([b|S]).
st(S) --> ['-v'], st(S).
ex_token --> tk([]).
tk(_) --> [].
tk(S) --> [F], { flag(F), \\+ memberchk(F, S), \\+ (excl(F, G), memberchk(G, S)) }, tk([F|S]).
ex_guard_first --> tg([]).
tg(_) --> [].
tg(S) --> [F], { \\+ memberchk(F, S), \\+ (excl(F, G), memberchk(G, S)) }, { flag(F) }, tg([F|S]).
tok(x).
tok(y).
not_x(S, S) :- \\+ S = [x|_].
look --> not_x, [Y], { tok(Y) }.
not_xz(S, S) :- \\+ S = [x, z|_].
look2 --> not_xz, [x], [_].
q --> [x].
q --> [].
cut_before --> q, !, [y].
relax_cut --> ( not_x -> { true } ; [x] ), !, [y].
ite_read --> ( [a] -> [b] ; [c] ).
ite_var --> ( [X], { X == a } -> [b] ; [Y], { tok(Y) } ).
vtq(S, S) :- var(S), !, fail.
vtq(S, S).
var_tail --> vtq, [z].
ex2(S0, S) :- S0 = [k|S].
explicit --> call(ex2).
app(S0, S) :- append([k], S, S0).
via_append --> app.
g4 --> [X], { \\+ X = b }, { member(X, [a,b,c]) }.
fa --> [X], { findall(Y, member(Y, [a,b]), L), member(X, L) }.
expr --> expr, ['+'], num.
expr --> num.
num --> [N], { integer(N) }.
as --> [].
as --> as, [a].
nb --> [N], { member(N, [1,2]) }, { N > 1 }.
gen_after_foreign --> [B], { nt_branch(B) }.
gen_before_foreign --> { nt_branch(B) }, [B].
not_z(S, S) :- \\+ S = [z|_].
look_after --> [X], { tok(X) }, not_z, [_].
peek_x(S, S) :- \\+ \\+ S = [x|_].
peek --> peek_x, [x].
:- table(tw/2).
tw --> [a], tw.
tw --> [b]."
  "Grammars that each exercise one construct before or after the checked
token.")

(define-foreign-predicate (#.(read-prolog-term "nt_branch") branch)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (dolist (name '("main" "dev"))
    (cl-prolog-kit::%unify-emit branch (read-prolog-term name) environment emit)))

(defparameter *token-check-corpus-cases*
  '(("c1" "[]") ("c2" "[]") ("c3" "[]") ("c4" "[]") ("gen_after" "[]")
    ("gen_before" "[]") ("ex_state" "['-a']") ("ex_token" "['-a']")
    ("ex_guard_first" "['-a']") ("look" "[]") ("look2" "[]") ("cut_before" "[]")
    ("relax_cut" "[]") ("ite_read" "[]") ("ite_read" "[a]") ("ite_var" "[]")
    ("var_tail" "[]") ("explicit" "[]") ("via_append" "[]") ("g4" "[]") ("fa" "[]")
    ("expr" "[1]") ("as" "[a]") ("nb" "[]") ("gen_after_foreign" "[]")
    ("gen_before_foreign" "[]") ("look_after" "[]") ("peek" "[]") ("tw" "[]")
    ("tw" "[a]"))
  "(NONTERMINAL PREFIX) pairs over the corpus, as Prolog text.")

(defun token-check-corpus-cases ()
  (mapcar (lambda (case)
            (list (read-prolog-term (first case)) (read-prolog-term (second case))))
          *token-check-corpus-cases*))

(defvar *token-check-corpus-oracle* nil)

(defun token-check-corpus-oracle ()
  ;; Continuations of one token: the longest any corpus rule needs after the
  ;; checked token.
  (or *token-check-corpus-oracle*
      (setf *token-check-corpus-oracle*
            (make-token-check-oracle
             *token-check-corpus*
             "[a, b, c, x, y, z, k, main, dev, '-a', '-b', '-v', f(a), f(b), '+', 1, 2, s(0)]"
             1))))

(deftest token-check-has-no-false-negatives-on-the-corpus (:timeout 120)
  ;; Cut, once/1, ->, \+, findall/3, tabling (tw, expr, as) and a foreign
  ;; generator, each before or after the checked token.
  (let ((oracle (token-check-corpus-oracle))
        (cases (token-check-corpus-cases)))
    (is (< 40 (token-check-oracle-count oracle cases)))
    (loop for (nonterminal prefix) in cases
          do (is-equal (list nonterminal prefix '())
                       (list nonterminal prefix
                             (token-check-missed oracle nonterminal prefix))))))

(deftest token-check-rejects-tokens-no-input-continues (:timeout 60)
  ;; Hand-checked: each token below cannot start a continuation.
  (let ((rulebase (token-check-oracle-rulebase (token-check-corpus-oracle))))
    (loop for (nonterminal prefix token) in '(("ex_token" "['-a']" "'-b'")
                                              ("ex_guard_first" "['-a']" "'-b'")
                                              ("gen_after" "[]" "feature")
                                              ("c4" "[]" "f(a)")
                                              ("num" "[]" "seven")
                                              ("look" "[]" "x")
                                              ("peek" "[]" "y")
                                              ("ite_read" "[a]" "c")
                                              ("tw" "[]" "c"))
          do (is-equal (list nonterminal token nil)
                       (list nonterminal token
                             (phrase-token-valid-p rulebase
                                                   (read-prolog-term nonterminal)
                                                   (read-prolog-term prefix)
                                                   (read-prolog-term token)
                                                   :max-inferences 200000))))))

(defparameter *token-check-construct-names*
  '("cut" "once" "ite" "not" "forall" "findall" "var" "eq" "memberchk" "copy"
    "foreign"))

(defparameter *token-check-construct-matrix*
  (with-output-to-string (out)
    (write-string "tv_cut(V) :- V = x, !, fail.
tv_cut(_).
tv_once(V) :- once(member(V, [y, z, w, q])).
tv_ite(V) :- ( V = x -> fail ; true ).
tv_not(V) :- \\+ V = x.
tv_forall(V) :- forall(V = x, fail).
tv_findall(V) :- findall(V, V = x, []).
tv_var(V) :- nonvar(V).
tv_eq(V) :- V \\== x.
tv_memberchk(V) :- memberchk(V, [y, z, w, q]).
tv_copy(V) :- copy_term(V, C), member(C, [y, z, w, q]).
tv_foreign(V) :- nt_not_x(V).
sv_cut(S, S) :- S = [x|_], !, fail.
sv_cut(S, S).
sv_once(S, S) :- once(S = [_|_]).
sv_ite(S, S) :- ( S = [x|_] -> fail ; true ).
sv_not(S, S) :- \\+ S = [x|_].
sv_forall(S, S) :- forall(S = [x|_], fail).
sv_findall(S, S) :- findall(S, S = [x|_], []).
sv_var(S, S) :- nonvar(S).
sv_eq(S, S) :- S \\== [x].
sv_memberchk(S, S) :- memberchk(_, S).
sv_copy(S, S) :- copy_term(S, C), C \\= [x|_].
sv_foreign(S, S) :- nt_stream_ok(S).
nest --> peek_x, [x].
nest --> [y].
peek_x(S, S) :- \\+ \\+ S = [x|_].
not_x(S, S) :- \\+ S = [x|_].
relaxed_cut --> ( not_x -> { true } ; [x] ), !, [y].
" out)
    (dolist (name *token-check-construct-names*)
      (format out "m_~A_state --> { tv_~:*~A(q) }, [y].~%m_~:*~A_state --> [z].~%" name)
      (format out "m_~A_stream --> sv_~:*~A, [y].~%m_~:*~A_stream --> [x], [z].~%" name)
      (format out "m_~A_token --> [X], { tv_~:*~A(X) }, [y].~%m_~:*~A_token --> [x], [z].~%" name)
      (format out "m_~A_rest --> [w], sv_~:*~A, [y].~%m_~:*~A_rest --> [w], [x], [z].~%" name)))
  "One grammar per construct and position, as Prolog source.")

(define-foreign-predicate (#.(read-prolog-term "nt_not_x") value)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((value (cl-prolog-kit::logic-substitute value environment)))
    (unless (or (cl-prolog-kit::logic-var-p value) (eq value (read-prolog-term "x")))
      (funcall emit environment))))

(define-foreign-predicate (#.(read-prolog-term "nt_stream_ok") stream)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (let ((stream (cl-prolog-kit::logic-substitute stream environment)))
    (unless (or (cl-prolog-kit::logic-var-p stream)
                (and (consp stream) (eq (first stream) (read-prolog-term "x"))))
      (funcall emit environment))))

(deftest token-check-has-no-false-negatives-across-constructs (:timeout 300)
  ;; Each construct on the parse state before the token, on the stream
  ;; before it, on the token itself, and on the stream after it; continuations
  ;; of two tokens, since m_C_rest needs w and two more.
  (let* ((oracle (make-token-check-oracle *token-check-construct-matrix*
                                          "[x, y, z, w, q]" 2))
         (cases (loop for nonterminal in
                        (append (loop for name in *token-check-construct-names*
                                      append (loop for position in '("state" "stream" "token" "rest")
                                                   collect (format nil "m_~A_~A" name position)))
                                '("nest" "relaxed_cut"))
                      append (loop for prefix in '("[]" "[w]")
                                   collect (list (read-prolog-term nonterminal)
                                                 (read-prolog-term prefix))))))
    (is (< 40 (token-check-oracle-count oracle cases)))
    (loop for (nonterminal prefix) in cases
          do (is-equal (list nonterminal prefix '())
                       (list nonterminal prefix
                             (token-check-missed oracle nonterminal prefix))))))

(define-foreign-predicate (#.(read-prolog-term "member") list)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (when (consp (cl-prolog-kit::logic-substitute list environment))
    (funcall emit environment)))

(deftest token-check-does-not-take-a-foreign-predicate-for-a-same-named-builtin ()
  ;; member/1 here is foreign and fails on an unbound list.  Taken for the
  ;; builtin member/2 it would run on the open tail and reject a.
  (let ((rulebase (consult-prolog "fm --> [a], nonempty, [_].
nonempty(S, S) :- member(S).")))
    (is (nth-value 1 (query-prolog-first rulebase (read-prolog-term "phrase(fm, [a, x])"))))
    (is (phrase-token-valid-p rulebase (read-prolog-term "fm") '()
                              (read-prolog-term "a")))))

(deftest token-check-follows-table-answers-replayed-inside-once ()
  ;; findall/3 completes opt/1's table, and once/1 replays its answers
  ;; before it reads the token.
  (let ((rulebase (consult-prolog ":- table(opt/1).
opt(a).
opt(b).
pick(S0, S) :- once((opt(T), S0 = [T|S])).
fo --> { findall(X, opt(X), _) }, pick.")))
    (dolist (case '(("a" t) ("b" t) ("c" nil)))
      (destructuring-bind (token expected) case
        (is-equal (list token expected)
                  (list token (nth-value 1 (query-prolog-first
                                            rulebase
                                            (read-prolog-term
                                             (format nil "phrase(fo, [~A])" token))))))
        (is-equal (list token expected)
                  (list token (phrase-token-valid-p rulebase (read-prolog-term "fo") '()
                                                    (read-prolog-term token))))))))

(deftest token-check-matches-valued-tokens-of-def-dcg-rule-grammars ()
  ;; A token is a bare kind or a (kind . value) cons; a bare kind has the
  ;; value NIL and so does not match a rule that names a value.
  (let ((rulebase (make-rulebase
                   :clauses (list (def-dcg-rule kind-only (terminal :name))
                                  (def-dcg-rule bob-only
                                    (dcg-token-match-value :name "bob"))
                                  (def-dcg-rule kit-only
                                    (dcg-token-match-value :name "kit"))
                                  (def-dcg-rule two-tokens
                                    (terminal :name) (terminal :end))))))
    (loop for (rule token expected) in '((kind-only (:name . "bob") t)
                                         (bob-only (:name . "bob") t)
                                         (kit-only (:name . "bob") nil)
                                         (kind-only :name t)
                                         (bob-only :name nil)
                                         (two-tokens (:name . "bob") t)
                                         (two-tokens :end nil))
          do (is-equal (list rule token expected)
                       (list rule token
                             (nth-value 1 (phrase rulebase rule
                                                  (if (eq rule 'two-tokens)
                                                      (list token :end)
                                                      (list token))))))
             (is-equal (list rule token expected)
                       (list rule token (phrase-token-valid-p rulebase rule '() token))))))

(deftest token-check-filter-equals-checking-each-candidate (:timeout 120)
  (let* ((oracle (token-check-corpus-oracle))
         (rulebase (token-check-oracle-rulebase oracle))
         (universe (token-check-oracle-universe oracle)))
    (loop for (nonterminal prefix) in (token-check-corpus-cases)
          do (is-equal (remove-if-not (lambda (token)
                                        (phrase-token-valid-p rulebase nonterminal prefix token
                                                              :max-inferences 200000))
                                      universe)
                       (phrase-filter-candidates rulebase nonterminal prefix universe
                                                 :max-inferences 2000000)))
    (is-equal '() (phrase-filter-candidates rulebase (read-prolog-term "c1") '() '()))))

(deftest token-check-over-accepts-past-the-checked-token ()
  ;; Specified behaviour: the check stops once the derivation reads past the
  ;; token, so a guard on a later token is not run.  No input makes oa a
  ;; phrase, yet a is accepted.
  (let ((oracle (make-token-check-oracle "oa --> [a], [_], { fail }." "[a, b]" 2)))
    (is (not (token-check-oracle-valid-p oracle (read-prolog-term "oa") '()
                                         (read-prolog-term "a"))))
    (is (phrase-token-valid-p (token-check-oracle-rulebase oracle)
                              (read-prolog-term "oa") '() (read-prolog-term "a")))))

(deftest token-check-of-a-schema-cannot-reject-its-instances ()
  ;; file(_) is not a token: pk/1 commits to a for it, so the check fails,
  ;; while the token file(b) is valid.
  (let ((rulebase (consult-prolog "pk(a) :- !.
pk(b).
fl --> [file(X)], { pk(X), X == b }.")))
    (is (nth-value 1 (query-prolog-first rulebase (read-prolog-term "phrase(fl, [file(b)])"))))
    (is (not (phrase-token-valid-p rulebase (read-prolog-term "fl") '()
                                   (read-prolog-term "file(_)"))))
    (is (phrase-token-valid-p rulebase (read-prolog-term "fl") '()
                              (read-prolog-term "file(b)")))))

(defparameter *token-check-grammar*
  "git     --> [git], git_sub.
git_sub --> [checkout], branch.
git_sub --> [commit], commit_opts.
commit_opts --> [].
commit_opts --> ['-m'], message, commit_opts.
commit_opts --> ['--amend'], commit_opts.
message --> [_].
branch  --> [B], { git_branch(B) }.
git_branch(main).
git_branch(dev).
spin --> [a].
spin --> { repeat, fail }.
fd_guarded --> { X in 1..2 }, [X], [b].")

(deftest token-check-on-a-command-line ()
  (let ((rulebase (consult-prolog *token-check-grammar*))
        (git (read-prolog-term "git")))
    (loop for (prefix token expected) in '(("[git]" "commit" t)
                                           ("[git]" "push" nil)
                                           ("[git, commit]" "'--amend'" t)
                                           ("[git, commit]" "dev" nil)
                                           ("[git, checkout]" "dev" t)
                                           ("[git, checkout]" "feature" nil)
                                           ("[git, commit, '-m']" "anything" t))
          do (is-equal (list prefix token expected)
                       (list prefix token
                             (phrase-token-valid-p rulebase git (read-prolog-term prefix)
                                                   (read-prolog-term token)))))))

(deftest token-check-keeps-constraint-propagation ()
  (let ((rulebase (consult-prolog *token-check-grammar*))
        (fd (read-prolog-term "fd_guarded")))
    (is (not (phrase-token-valid-p rulebase fd '() 5)))
    (is (phrase-token-valid-p rulebase fd '() 1))))

(deftest token-check-signals-the-query-limits ()
  (let ((rulebase (consult-prolog *token-check-grammar*))
        (spin (read-prolog-term "spin")))
    (is (phrase-token-valid-p rulebase spin '() (read-prolog-term "a")
                              :max-inferences 1000))
    (signals-prolog-condition prolog-inference-limit-exceeded
      (phrase-token-valid-p rulebase spin '() (read-prolog-term "z")
                            :max-inferences 1000))
    (signals-prolog-condition prolog-deadline-exceeded
      (phrase-token-valid-p rulebase spin '() (read-prolog-term "z")
                            :deadline (1- (get-internal-real-time))))
    (signals-prolog-condition prolog-inference-limit-exceeded
      (phrase-filter-candidates rulebase spin '() (read-prolog-term "[a, z]")
                                :max-inferences 1000))))

(deftest token-check-leaves-the-shared-tables-intact ()
  (let* ((rulebase (consult-prolog *token-check-grammar*))
         (query (read-prolog-term "phrase(git, [git, commit | T])"))
         (before (query-prolog rulebase query :limit 3))
         (check (lambda ()
                  (phrase-token-valid-p rulebase (read-prolog-term "git")
                                        (read-prolog-term "[git, commit]")
                                        (read-prolog-term "'--amend'")))))
    (is (funcall check))
    (is-equal (values (cl-prolog-kit::%canonicalize-variant before))
              (values (cl-prolog-kit::%canonicalize-variant
                       (query-prolog rulebase query :limit 3))))
    (is (funcall check))))

(deftest token-check-validates-arguments ()
  (let ((rulebase (consult-prolog *token-check-grammar*)))
    (signals-error (phrase-token-valid-p rulebase 'git '(a . b) 'x))
    (signals-error (phrase-token-valid-p rulebase 'git '() 'x :max-inferences -1))
    (signals-error (phrase-token-valid-p rulebase 'git '() 'x :deadline "soon"))
    (signals-error (phrase-filter-candidates rulebase 'git '() '(a . b)))))

(define-foreign-predicate (token-check-nested-query token)
    (rulebase environment depth emit)
  (declare (cl:ignore token depth))
  (query-prolog rulebase (read-prolog-term "X is Y"))
  (funcall emit environment))

(deftest token-check-passes-on-errors-of-nested-queries ()
  ;; The foreign predicate's argument does not reach the open tail, so it
  ;; runs, and the error of its nested query reaches the caller.
  (signals-prolog-condition prolog-instantiation-error
    (phrase-token-valid-p
     (prolog ((nested-guard ?s0 ?s)
              (token-check-nested-query none)
              (= ?s0 (?x . ?s))))
     'nested-guard '() 'a)))
