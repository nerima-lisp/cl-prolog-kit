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
  (is-equal '(nil t nil) (next-terminals "num" "[]")
            "integer(N) fails on the unknown token")
  (is-equal '(nil t nil) (next-terminals "long_word" "[]")
            "atom_length/2 raises an instantiation error on the unknown token"))

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

(deftest next-terminals-signal-the-query-limits ()
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
  ;; Only an instantiation error raised by the probed derivation itself
  ;; stands for a guard that cannot decide the unknown token.
  (signals-prolog-condition prolog-instantiation-error
    (phrase-next-terminals
     (prolog ((nested-guard ?s0 ?s)
              (= ?s0 (?x . ?s))
              (next-terminals-nested-query ?x)))
     'nested-guard '())
    "an error of a query nested in a foreign predicate reaches the caller"))

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
