;;;; PHRASE-NEXT-TERMINALS and MAP-PHRASE-NEXT-TERMINALS.

(in-package #:cl-prolog-kit.tests)

(defparameter *next-terminals-grammar*
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

num --> [N], { integer(N) }.
long_word --> [W], { atom_length(W, L), L > 2 }.

cut_after_terminal --> ['-v'], !, [x].
cut_after_terminal --> ['-q'].
cut_after_variable --> [X], { atom(X) }, !, [y].
cut_after_variable --> ['-h'].
cut_in_guard --> [X], { choose(X) }.
choose(a) :- !.
choose(b).

opts(Seen) --> [].
opts(Seen) --> ['-a'], { \\+ memberchk(b, Seen) }, opts([a|Seen]).
opts(Seen) --> ['-b'], { \\+ memberchk(a, Seen) }, opts([b|Seen]).
opts(Seen) --> ['-v'], opts(Seen).
exclusive --> opts([]).

expr --> expr, ['+'], num.
expr --> num.
as --> [].
as --> as, [a].

dup --> [f(X)], [z].
dup --> [f(Y)], [w].
dup --> [g].
dup --> [f(1)].
open_rest(_, _).
spin --> [a].
spin --> { repeat, fail }.
fd_guarded --> { X in 1..2 }, [X], [b]."
  "Grammars shared by the tests below, as consulted Prolog source.")

(defun next-terminals-rulebase ()
  (consult-prolog *next-terminals-grammar*))

(defun next-terminals (nonterminal prefix &rest options)
  "Return PHRASE-NEXT-TERMINALS' three values as a list; NONTERMINAL and
PREFIX are Prolog source text."
  (multiple-value-list
   (apply #'phrase-next-terminals (next-terminals-rulebase)
          (read-prolog-term nonterminal) (read-prolog-term prefix)
          options)))

(defun next-terminals-expect (terminals any-p end-p)
  (list (read-prolog-term terminals) any-p end-p))

(deftest next-terminals-complete-a-command-line ()
  (is-equal (next-terminals-expect "[checkout, commit]" nil nil)
            (next-terminals "git" "[git]"))
  (is-equal (next-terminals-expect "['-m', '--amend']" nil t)
            (next-terminals "git" "[git, commit]"))
  ;; commit_opts recurses after a terminal, so it is tabled; its call on the
  ;; unread rest must not replay the pruned table of the call on the tail.
  (is-equal (next-terminals-expect "['-m', '--amend']" nil t)
            (next-terminals "git" "[git, commit, '-m', hello]")))

(deftest next-terminals-report-an-invalid-prefix ()
  (is-equal '(nil nil nil) (next-terminals "git" "[git, bogus]"))
  (is-equal '(nil t nil) (next-terminals "git" "[git, commit, '-m']")
            "-m takes any one token as its message and cannot end the phrase"))

(deftest next-terminals-enumerate-a-variable-terminal-bound-by-a-guard ()
  (is-equal (next-terminals-expect "[main, dev]" nil nil)
            (next-terminals "git" "[git, checkout]")))

(deftest next-terminals-report-any-when-a-guard-cannot-decide ()
  ;; A type test or atom_length/2 on the unknown token is not run.
  (is-equal '(nil t nil) (next-terminals "num" "[]"))
  (is-equal '(nil t nil) (next-terminals "long_word" "[]")))

(deftest next-terminals-do-not-let-a-later-cut-prune-other-tokens ()
  (is-equal (next-terminals-expect "['-v', '-q']" nil nil)
            (next-terminals "cut_after_terminal" "[]"))
  (is-equal (next-terminals-expect "['-h']" t nil)
            (next-terminals "cut_after_variable" "[]"))
  ;; The cut in choose/1 runs after the read and would commit to the first
  ;; binding of X, though phrase(cut_in_guard, [b]) succeeds.
  (is-equal (next-terminals-expect "[a, b]" nil nil)
            (next-terminals "cut_in_guard" "[]")))

(deftest next-terminals-check-guards-after-the-terminal ()
  (is-equal (next-terminals-expect "['-a', '-b', '-v']" nil t)
            (next-terminals "exclusive" "[]"))
  (is-equal (next-terminals-expect "['-a', '-v']" nil t)
            (next-terminals "exclusive" "['-a']"))
  (is-equal (next-terminals-expect "['-b', '-v']" nil t)
            (next-terminals "exclusive" "['-b', '-v']")))

(deftest next-terminals-terminate-on-left-recursion ()
  (is-equal (next-terminals-expect "['+']" nil t)
            (next-terminals "expr" "[1]" :max-inferences 100000))
  (is-equal '(nil t nil)
            (next-terminals "expr" "[1, '+']" :max-inferences 100000))
  ;; Nullable left recursion: each answer of the table begun before the read
  ;; would otherwise extend the tail by one more token.
  (is-equal (next-terminals-expect "[a]" nil t)
            (next-terminals "as" "[a, a]" :max-inferences 100000)))

(deftest p7-next-terminals-signal-the-query-limits ()
  (signals-prolog-condition prolog-inference-limit-exceeded
    (next-terminals "spin" "[]" :max-inferences 1000))
  (signals-prolog-condition prolog-deadline-exceeded
    (next-terminals "spin" "[]" :deadline (1- (get-internal-real-time))))
  (let ((reported '()))
    (signals-prolog-condition prolog-inference-limit-exceeded
      (map-phrase-next-terminals (lambda (kind value) (push (cons kind value) reported))
                                 (next-terminals-rulebase)
                                 (read-prolog-term "spin") '()
                                 :max-inferences 1000))
    (is-equal (list (cons :terminal (read-prolog-term "a"))) reported
              "candidates found before the limit were already delivered")))

(defun next-terminals-variant (term)
  "TERM with its variables renamed canonically, so answers compare by EQUAL."
  (values (cl-prolog-kit::%canonicalize-variant term)))

(deftest next-terminals-leave-the-shared-tables-intact ()
  ;; A probe's tables hold only the answers that did not read the tail.
  ;; Kept in the rulebase's shared session they would answer the next probe
  ;; on the same rulebase, which would then find nothing.
  (let* ((rulebase (next-terminals-rulebase))
         (query (read-prolog-term "phrase(git, [git, commit | T])"))
         (before (query-prolog rulebase query :limit 3))
         (probe (lambda ()
                  (multiple-value-list
                   (phrase-next-terminals rulebase (read-prolog-term "git")
                                          (read-prolog-term "[git, commit]")))))
         (first-probe (funcall probe)))
    (is-equal 3 (length before))
    (is-equal (next-terminals-variant before)
              (next-terminals-variant (query-prolog rulebase query :limit 3)))
    (is-equal (next-terminals-expect "['-m', '--amend']" nil t) first-probe)
    (is-equal first-probe (funcall probe))))

(deftest next-terminals-keep-constraint-propagation ()
  (is-equal '(nil nil nil) (next-terminals "fd_guarded" "[5]")
            "the FD hook still rejects a token outside the domain")
  (is-equal (next-terminals-expect "[b]" nil nil)
            (next-terminals "fd_guarded" "[1]")))

(deftest next-terminals-cover-def-dcg-rule-grammars ()
  (let ((rulebase (make-rulebase
                   :clauses (list (def-dcg-rule greeting
                                    (terminal :hello)
                                    (dcg-alt world named))
                                  (def-dcg-rule world (terminal :world))
                                  (def-dcg-rule named
                                    (dcg-token-match-value :name "kit"))))))
    (is-equal '((:hello) nil nil)
              (multiple-value-list (phrase-next-terminals rulebase 'greeting '())))
    (is-equal '((:world (:name . "kit")) nil nil)
              (multiple-value-list
               (phrase-next-terminals rulebase 'greeting '(:hello))))))

(deftest next-terminals-validate-arguments ()
  (signals-error (phrase-next-terminals (next-terminals-rulebase) 'git '(a . b)))
  (signals-error (phrase-next-terminals (next-terminals-rulebase) 'git '()
                                        :max-inferences -1))
  (signals-error (phrase-next-terminals (next-terminals-rulebase) 'git '()
                                        :deadline "soon")))

(deftest next-terminals-deduplicate-variants-in-discovery-order ()
  (destructuring-bind (terminals any-p end-p) (next-terminals "dup" "[]")
    (is-equal 3 (length terminals))
    (is-equal (read-prolog-term "f") (first (first terminals)))
    (is (cl-prolog-kit::logic-var-p (second (first terminals))))
    (is-equal (read-prolog-term "[g, f(1)]") (rest terminals))
    (is (not any-p))
    (is (not end-p)))
  (is-equal '(nil t t) (next-terminals "open_rest" "[x]")
            "a rule that ignores its remainder accepts anything, and the end"))

(deftest next-terminals-separate-the-tail-from-other-open-lists ()
  ;; aux calls p on an unrelated open list while p is tabled and in progress
  ;; on the probe's tail.  Were the two calls one variant, aux would replay
  ;; the tail call's pruned, still empty table and fail.
  (let ((rulebase (consult-prolog
                   ":- table(p/2).
p --> {aux}, [a], p.
p --> [].
aux :- once(phrase(p, Y)), Y == [].")))
    (is-equal (list (list (read-prolog-term "a")) nil t)
              (multiple-value-list
               (phrase-next-terminals rulebase (read-prolog-term "p") '()
                                      :max-inferences 100000)))))

(define-foreign-predicate (next-terminals-nested-query token)
    (rulebase environment depth emit)
  (declare (cl:ignore token depth))
  (query-prolog rulebase (read-prolog-term "X is Y"))
  (funcall emit environment))

(deftest next-terminals-pass-on-errors-of-nested-queries ()
  ;; The foreign predicate's argument shares nothing with the tail, so the
  ;; probe runs it, and the error of its nested query reaches the caller.
  (signals-prolog-condition prolog-instantiation-error
    (phrase-next-terminals
     (prolog ((nested-guard ?s0 ?s)
              (next-terminals-nested-query none)
              (= ?s0 (?x . ?s))))
     'nested-guard '())))

(deftest next-terminals-pass-on-errors-of-the-callback ()
  (signals-prolog-condition prolog-instantiation-error
    (map-phrase-next-terminals
     (lambda (kind value)
       (declare (cl:ignore kind value))
       (cl-prolog-kit::%raise-instantiation-error
        nil (cl-prolog-kit::%iso-atom "CALLBACK") "raised by the callback"))
     (next-terminals-rulebase) (read-prolog-term "spin") '()
     :max-inferences 1000)
    "an error of the callback reaches the caller"))

(define-foreign-predicate (next-terminals-nested-probe token)
    (rulebase environment depth emit)
  (declare (cl:ignore token depth))
  (map-phrase-next-terminals
   (lambda (kind value)
     (declare (cl:ignore kind value))
     (cl-prolog-kit::%raise-instantiation-error
      nil (cl-prolog-kit::%iso-atom "CALLBACK") "raised by the inner callback"))
   rulebase 'inner-word '())
  (funcall emit environment))

(deftest next-terminals-pass-on-errors-of-a-nested-probe-callback ()
  ;; The inner probe re-signals its callback's error from inside the outer
  ;; derivation; nothing in the outer probe may absorb it.
  (let ((rulebase (prolog ((outer-word ?s0 ?s)
                           (next-terminals-nested-probe none)
                           (= ?s0 (?x . ?s)))
                          ((inner-word ?s0 ?s)
                           (= ?s0 (a . ?s))))))
    (signals-prolog-condition prolog-instantiation-error
      (phrase-next-terminals rulebase 'outer-word '())
      "the inner callback's error reaches the caller")))
;;; Properties P1-P7 of the two-stage design, checked against phrase/2.

(defparameter *next-terminals-corpus*
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
peek --> peek_x, [x]."
  "The prototype's grammar corpus: each rule exercises one construct the
probe classifies.")

(define-foreign-predicate (#.(read-prolog-term "nt_branch") branch)
    (rulebase environment depth emit)
  (declare (cl:ignore rulebase depth))
  (dolist (name '("main" "dev"))
    (cl-prolog-kit::%unify-emit branch (read-prolog-term name) environment emit)))

(defparameter *next-terminals-corpus-cases*
  '(("c1" "[]") ("c2" "[]") ("c3" "[]") ("c4" "[]") ("gen_after" "[]")
    ("gen_before" "[]") ("ex_state" "['-a']") ("ex_token" "['-a']")
    ("ex_guard_first" "['-a']") ("look" "[]") ("look2" "[]") ("cut_before" "[]")
    ("relax_cut" "[]") ("ite_read" "[]") ("ite_read" "[a]") ("ite_var" "[]")
    ("var_tail" "[]") ("explicit" "[]") ("via_append" "[]") ("g4" "[]") ("fa" "[]")
    ("expr" "[1]") ("as" "[a]") ("nb" "[]") ("gen_after_foreign" "[]")
    ("gen_before_foreign" "[]") ("look_after" "[]") ("peek" "[]"))
  "(NONTERMINAL PREFIX) pairs of the prototype harness, as Prolog text.")

(defparameter *next-terminals-corpus-universe*
  "[a, b, c, x, y, z, k, main, dev, '-a', '-b', '-v', f(a), f(b), '+', 1, 2, s(0)]")

(defstruct (next-terminals-oracle (:constructor %make-next-terminals-oracle))
  rulebase universe continuation-length (cache (make-hash-table :test #'equal)))

(defun make-next-terminals-oracle (source universe continuation-length)
  (%make-next-terminals-oracle :rulebase (consult-prolog source)
                               :universe (read-prolog-term universe)
                               :continuation-length continuation-length))

(defun next-terminals-oracle-phrase-p (oracle nonterminal input)
  "phrase/2 on the ground INPUT, the reference every property is checked
against."
  (let ((key (prin1-to-string (list nonterminal input))))
    (multiple-value-bind (value present-p)
        (gethash key (next-terminals-oracle-cache oracle))
      (if present-p
          value
          (setf (gethash key (next-terminals-oracle-cache oracle))
                (nth-value 1 (query-prolog-first
                              (next-terminals-oracle-rulebase oracle)
                              (list (read-prolog-term "phrase") nonterminal input)
                              :max-inferences 20000)))))))

(defun next-terminals-oracle-continuations (oracle)
  "Every token list over the universe up to the oracle's length."
  (let ((universe (next-terminals-oracle-universe oracle)))
    (loop with all = (list '())
          with layer = (list '())
          repeat (next-terminals-oracle-continuation-length oracle)
          do (setf layer (loop for continuation in layer
                               append (loop for token in universe
                                            collect (cons token continuation))))
             (setf all (append all layer))
          finally (return all))))

(defun next-terminals-oracle-tokens (oracle nonterminal prefix)
  "The universe tokens T for which PREFIX ++ [T|S] is a phrase for some S
in the oracle's continuations."
  (let ((continuations (next-terminals-oracle-continuations oracle)))
    (loop for token in (next-terminals-oracle-universe oracle)
          when (loop for continuation in continuations
                     thereis (next-terminals-oracle-phrase-p
                              oracle nonterminal
                              (append prefix (cons token continuation))))
            collect token)))

(defun next-terminals-covered-p (token terminals any-p)
  "TOKEN, ground, is covered when ANY-P or a reported terminal subsumes it."
  (or any-p
      (some (lambda (terminal) (nth-value 1 (unify terminal token))) terminals)))

(defun next-terminals-check-case (oracle nonterminal prefix)
  "Return (VALUES MISSED END-P TRUTH-END-P TERMINALS ANY-P) for one case."
  (multiple-value-bind (terminals any-p end-p)
      (phrase-next-terminals (next-terminals-oracle-rulebase oracle)
                             nonterminal prefix :max-inferences 200000)
    (values (remove-if (lambda (token)
                         (next-terminals-covered-p token terminals any-p))
                       (next-terminals-oracle-tokens oracle nonterminal prefix))
            end-p
            (next-terminals-oracle-phrase-p oracle nonterminal prefix)
            terminals
            any-p)))

(defvar *next-terminals-corpus-oracle* nil)

(defun next-terminals-corpus-oracle ()
  ;; Continuations of one token: the longest any corpus rule needs after the
  ;; candidate.
  (or *next-terminals-corpus-oracle*
      (setf *next-terminals-corpus-oracle*
            (make-next-terminals-oracle *next-terminals-corpus*
                                        *next-terminals-corpus-universe* 1))))

(defun next-terminals-corpus-cases ()
  (mapcar (lambda (case)
            (list (read-prolog-term (first case)) (read-prolog-term (second case))))
          *next-terminals-corpus-cases*))

(deftest p1-next-terminals-cover-every-oracle-token (:timeout 120)
  (let ((oracle (next-terminals-corpus-oracle)))
    ;; The oracle itself must find tokens, or the check below is vacuous.
    (is (< 40 (loop for (nonterminal prefix) in (next-terminals-corpus-cases)
                    sum (length (next-terminals-oracle-tokens oracle nonterminal prefix)))))
    (loop for (nonterminal prefix) in (next-terminals-corpus-cases)
          do (is-equal (list nonterminal prefix '())
                       (list nonterminal prefix
                             (next-terminals-check-case oracle nonterminal prefix))))))

(deftest p2-next-terminals-end-p-is-phrase-of-the-prefix (:timeout 120)
  (let ((oracle (next-terminals-corpus-oracle)))
    (loop for (nonterminal prefix) in (next-terminals-corpus-cases)
          do (multiple-value-bind (missed end-p truth-end-p)
                 (next-terminals-check-case oracle nonterminal prefix)
               (declare (cl:ignore missed))
               (is-equal (list nonterminal prefix truth-end-p)
                         (list nonterminal prefix end-p))))))

(deftest p3-next-terminals-ground-candidates-pass-the-token-check (:timeout 120)
  (let ((oracle (next-terminals-corpus-oracle)))
    (loop for (nonterminal prefix) in (next-terminals-corpus-cases)
          do (dolist (terminal (fourth (multiple-value-list
                                        (next-terminals-check-case
                                         oracle nonterminal prefix))))
               (when (null (cl-prolog-kit::%collect-variables terminal))
                 (is-equal (list nonterminal prefix terminal t)
                           (list nonterminal prefix terminal
                                 (phrase-token-valid-p
                                  (next-terminals-oracle-rulebase oracle)
                                  nonterminal prefix terminal
                                  :max-inferences 200000))))))))

(deftest p4-next-terminals-token-check-accepts-every-oracle-token (:timeout 120)
  (let ((oracle (next-terminals-corpus-oracle)))
    (loop for (nonterminal prefix) in (next-terminals-corpus-cases)
          do (dolist (token (next-terminals-oracle-tokens oracle nonterminal prefix))
               (is-equal (list nonterminal prefix token t)
                         (list nonterminal prefix token
                               (phrase-token-valid-p
                                (next-terminals-oracle-rulebase oracle)
                                nonterminal prefix token
                                :max-inferences 200000)))))
    ;; The check also rejects: these tokens cannot continue any phrase.
    (loop for (nonterminal prefix token) in '(("ex_token" "['-a']" "'-b'")
                                              ("ex_guard_first" "['-a']" "'-b'")
                                              ("gen_after" "[]" "feature")
                                              ("c4" "[]" "f(a)")
                                              ("num" "[]" "seven")
                                              ("look" "[]" "x")
                                              ("peek" "[]" "y")
                                              ("ite_read" "[a]" "c"))
          do (is-equal (list nonterminal token nil)
                       (list nonterminal token
                             (phrase-token-valid-p
                              (next-terminals-oracle-rulebase oracle)
                              (read-prolog-term nonterminal)
                              (read-prolog-term prefix)
                              (read-prolog-term token)
                              :max-inferences 200000))))))

;;; P5: each construct that can test a binding, applied to the parse state
;;; before the read, the stream before the read, the read token, and the
;;; stream after the read.  tv_C/1 holds when its argument is not x or is in
;;; a set without x; sv_C//0 holds when the stream does not start with x.

(defparameter *next-terminals-construct-names*
  '("cut" "once" "ite" "not" "forall" "findall" "var" "eq" "memberchk" "copy"
    "foreign"))

(defparameter *next-terminals-construct-matrix*
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
    (dolist (name *next-terminals-construct-names*)
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

(deftest p5-next-terminals-construct-position-matrix (:timeout 300)
  ;; Continuations of two tokens: m_C_rest needs w, then two more.
  (let ((oracle (make-next-terminals-oracle *next-terminals-construct-matrix*
                                            "[x, y, z, w, q]" 2)))
    (dolist (nonterminal
             (append (loop for name in *next-terminals-construct-names*
                           append (loop for position in '("state" "stream" "token" "rest")
                                        collect (format nil "m_~A_~A" name position)))
                     '("nest" "relaxed_cut")))
      (dolist (prefix '("[]" "[w]"))
        (let ((nonterminal (read-prolog-term nonterminal))
              (prefix (read-prolog-term prefix)))
          (multiple-value-bind (missed end-p truth-end-p)
              (next-terminals-check-case oracle nonterminal prefix)
            (is-equal (list nonterminal prefix '() truth-end-p)
                      (list nonterminal prefix missed end-p))))))))

(deftest p6-next-terminals-report-any-for-an-infinite-generator-in-bounded-time
    (:timeout 120)
  ;; nat/1 generates ever larger tokens; the phase budget ends the branch
  ;; with ANY-P well inside the inference budget.  It takes well under a
  ;; second unloaded; the 30 s bound leaves a wide margin for a loaded CI
  ;; runner while still failing on the cubic growth the budget prevents
  ;; (a budget of 2000 ran past two minutes).
  (let ((rulebase (consult-prolog "nat(0).
nat(s(N)) :- nat(N).
peano --> [X], { nat(X) }."))
        (start (get-internal-real-time)))
    (multiple-value-bind (terminals any-p end-p)
        (phrase-next-terminals rulebase (read-prolog-term "peano") '()
                               :max-inferences 200000
                               :deadline (prolog-deadline-after 60))
      (is any-p)
      (is (not end-p))
      (is (every (lambda (terminal) (null (cl-prolog-kit::%collect-variables terminal)))
                 terminals)))
    (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 30))))
