;;;; The control stack limit of top-level queries.

(in-package #:cl-prolog-kit.tests)

;;; Every test binds the limit to a quarter of the stack.  The goals then
;;; trip it at a depth of a few hundred levels on any platform, well under
;;; the sizes used here, and three quarters of the stack stay free, so a
;;; test cannot reach SBCL's guard page even if the limit were missed by a
;;; wide margin: under --script a guard page hit ends the process.
(defconstant +stack-test-fraction+ 1/4)

(defun stack-test-rules ()
  (consult-prolog "cnt(N, N) :- !.
cnt(I, N) :- I1 is I + 1, cnt(I1, N).
deep(0, a) :- !.
deep(N, f(T)) :- N1 is N - 1, deep(N1, T).
wrap(_, A, f(A))."
                  (make-rulebase)))

(defun stack-test-query (text &key (rulebase (stack-test-rules)))
  "Run TEXT under the test's stack fraction, a budget and a deadline.
Return the PROLOG-STACK-LIMIT-EXCEEDED it raised, or :FINISHED."
  (let ((cl-prolog-kit::*control-stack-limit-fraction* +stack-test-fraction+)
        (*standard-output* (make-broadcast-stream)))
    (handler-case
        (progn (query-prolog rulebase (read-prolog-term text)
                             :max-inferences 10000000
                             :deadline (prolog-deadline-after 10))
               :finished)
      (prolog-stack-limit-exceeded (condition) condition))))

(defun stack-test-stopped-p (text)
  (typep (stack-test-query text) 'prolog-stack-limit-exceeded))

(deftest deep-proof-search-raises-the-stack-limit ()
  ;; A tail-recursive count and a term built one level per call both
  ;; deepen the control stack at every level of proof search.
  (dolist (text '("cnt(0, 3000)." "deep(3000, T)."
                  "deep(3000, T), findall(T, true, [X]), write(X)."))
    (is (stack-test-stopped-p text) text)
    ;; The guard page was not touched, so the same goal stops the same way
    ;; again in this process.
    (is (stack-test-stopped-p text) text))
  ;; Shallow recursion is unaffected.
  (is-equal :finished (stack-test-query "cnt(0, 100).")))

(defun stack-test-deep-term (depth leaf)
  (let ((term leaf))
    (dotimes (index depth term)
      (setf term (list 'f term)))))

(defun stack-test-goal (goal &key (fraction +stack-test-fraction+))
  "Like STACK-TEST-QUERY for a goal given as a term, under FRACTION."
  (let ((cl-prolog-kit::*control-stack-limit-fraction* fraction))
    (handler-case
        (progn (query-prolog (stack-test-rules) goal
                             :max-inferences 10000000
                             :deadline (prolog-deadline-after 10))
               :finished)
      (prolog-stack-limit-exceeded (condition) condition))))

(deftest deep-terms-inside-one-builtin-raise-the-stack-limit ()
  ;; Unifying two terms 5000 levels deep and copying one walk them inside a
  ;; single builtin call, with no goal dispatch to check the stack, so only
  ;; the check in the term walk stops them.  foldl/4 grows a deep
  ;; accumulator and checks it for occurrences at every step; that costs
  ;; time quadratic in the depth reached, so it runs under a sixteenth of
  ;; the stack, where the dispatch check may be the one that stops it.  Each
  ;; runs twice: the guard page was not touched the first time.  Comparing
  ;; deep terms is not used: term comparison does not dereference through
  ;; the check and takes time quadratic in the depth.
  (dotimes (round 2)
    (is (typep (stack-test-goal (list '= (stack-test-deep-term 5000 'a)
                                      (stack-test-deep-term 5000 'a)))
               'prolog-stack-limit-exceeded))
    (is (typep (stack-test-goal (list 'cl-prolog-kit::copy_term
                                      (stack-test-deep-term 5000 '?leaf) '?copy))
               'prolog-stack-limit-exceeded))
    (is (typep (stack-test-goal (read-prolog-term
                                 "numlist(1, 5000, L), foldl(wrap, L, a, _).")
                                :fraction 1/16)
               'prolog-stack-limit-exceeded)))
  ;; A long list is walked down its tail by loops, so sorting one finishes.
  (is-equal :finished (stack-test-query "numlist(1, 20000, L), msort(L, _).")))

(deftest stack-limit-is-a-catchable-resource-error ()
  (let ((condition (stack-test-query "cnt(0, 3000).")))
    (is (typep condition 'prolog-resource-error))
    (is-equal (list (cl-prolog-kit::%iso-atom "RESOURCE_ERROR")
                    (cl-prolog-kit::%iso-atom "CONTROL_STACK"))
              (second (prolog-exception-term condition))))
  ;; catch/3 intercepts it, and the recovery runs on the unwound stack.
  (let ((solutions
          (let ((cl-prolog-kit::*control-stack-limit-fraction* +stack-test-fraction+))
            (query-prolog (stack-test-rules)
                          (read-prolog-term
                           "catch(cnt(0, 3000), error(resource_error(R), _), true).")
                          :max-inferences 10000000
                          :deadline (prolog-deadline-after 10)))))
    (is-equal 1 (length solutions))
    (is-equal (cl-prolog-kit::%iso-atom "CONTROL_STACK")
              (cdr (first (first solutions))))))

(deftest stack-limit-uses-the-querying-thread-s-own-stack ()
  ;; A query in another thread is a root of its own and measures its own
  ;; thread's stack, which lies elsewhere in memory: a limit taken from
  ;; another thread would trip at once or never.
  (destructuring-bind (deep shallow)
      (sb-thread:join-thread
       (sb-thread:make-thread
        (lambda () (list (stack-test-query "cnt(0, 3000).")
                         (stack-test-query "cnt(0, 100).")))
        :name "stack-limit-test"))
    (is (typep deep 'prolog-stack-limit-exceeded))
    (is-equal :finished shallow)))

;;; Cleanup goals after a stack stop.  SBCL runs each unwind-protect cleanup
;;; on top of the stack where the non-local exit started, so after a stop
;;; every cleanup starts past the limit and runs within the cleanup margin.

(defparameter +stack-cleanup-rules+
  ":- dynamic(copied/1).
lv(I) :- assertz(level(I)), I1 is I + 1, lv(I1).
rec(N) :- setup_call_cleanup(assertz(opened(N)), (N1 is N + 1, rec(N1)),
                             assertz(closed(N))).
rec_deep(N) :- setup_call_cleanup(assertz(opened(N)),
                                  (N1 is N + 1, rec_deep(N1)),
                                  (assertz(closed(N)), deep(T), copy_term(T, _),
                                   assertz(copied(N)))).")

(defun stack-cleanup-count (rulebase text)
  (length (query-prolog rulebase (read-prolog-term text))))

(defun stack-cleanup-levels (text)
  "Run TEXT under the test's stack fraction and return how many levels of
lv/1 ran before the stop that TEXT catches."
  (let ((rulebase (consult-prolog +stack-cleanup-rules+ (make-rulebase)))
        (cl-prolog-kit::*control-stack-limit-fraction* +stack-test-fraction+))
    (is-equal 1 (length (query-prolog rulebase (read-prolog-term text)
                                      :max-inferences 10000000
                                      :deadline (prolog-deadline-after 10)))
              text)
    (stack-cleanup-count rulebase "level(X).")))

(deftest a-cleanup-gets-one-margin-however-cleanups-nest ()
  ;; A cleanup run on normal exit recurses past the limit by the margin
  ;; and stops with the resource error; a cleanup nested in it gets no
  ;; more.  At about 1150 bytes a level, the 224 KiB margin is about 200
  ;; levels.
  (let ((plain (stack-cleanup-levels
                "catch(lv(0), error(resource_error(control_stack), _), true)."))
        (cleanup (stack-cleanup-levels
                  "catch(setup_call_cleanup(true, true, lv(0)),
                         error(resource_error(control_stack), _), true)."))
        (nested (stack-cleanup-levels
                 "catch(setup_call_cleanup(true, true,
                          setup_call_cleanup(true, true, lv(0))),
                        error(resource_error(control_stack), _), true).")))
    (is (> plain 100) plain)
    (is (< 100 (- cleanup plain) 400) (list plain cleanup))
    (is (<= nested cleanup) (list cleanup nested))))

(defun stack-cleanup-child-script (goals)
  "Return the text of the child's script; see STACK-CLEANUP-CHILD-RESULTS.
The child reads it after loading the system, so it names the system's
symbols with their package and binds only CL-USER ones."
  (format nil "(require :asdf)
(asdf:load-asd ~S)
(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (asdf:load-system \"cl-prolog-kit\"))
(defvar *deep* (let ((term 0))
                 (dotimes (level 4000 term)
                   (setf term (list (intern \"F\" \"CL-PROLOG-KIT.USER-ATOMS\")
                                    term)))))
(flet ((run (text)
         (let ((rulebase (cl-prolog-kit:consult-prolog
                          ~S (cl-prolog-kit:make-rulebase))))
           (flet ((solutions (goal)
                    (length (cl-prolog-kit:query-prolog rulebase goal)))
                  (read-goal (text)
                    (cl-prolog-kit:read-prolog-term text)))
             (list (solutions (subst-if *deep*
                                        (lambda (x)
                                          (and (symbolp x)
                                               (string= (symbol-name x) \"?DEEP\")))
                                        (read-goal text)))
                   (solutions (read-goal \"opened(X).\"))
                   (solutions (read-goal \"closed(X).\"))
                   (solutions (read-goal \"copied(X).\")))))))
  (print (cons cl-prolog-kit::*control-stack-limit-fraction*
               (mapcar #'run '~S))))
"
          (namestring (asdf:system-source-file "cl-prolog-kit"))
          +stack-cleanup-rules+
          goals))

(defparameter *stack-cleanup-self-exe* "/proc/self/exe"
  "Link to the running executable, where the system has one (Linux).")

(defun stack-cleanup-sbcl-runtime-candidates ()
  "Return the paths that may name the running SBCL runtime binary, the real
executable first.  Under the cl-weave CLI's wrapper, which runs SBCL with
`exec -a \"$0\"`, SB-EXT:*RUNTIME-PATHNAME* names the wrapper script on
Linux, so *STACK-CLEANUP-SELF-EXE* comes first where it exists."
  (remove-duplicates
   (remove nil
           (list (probe-file *stack-cleanup-self-exe*)
                 sb-ext:*runtime-pathname*
                 (and sb-ext:*runtime-pathname*
                      (probe-file sb-ext:*runtime-pathname*))))
   :test #'equal :key #'namestring))

(defun stack-cleanup-sbcl-runtime-and-core ()
  "Return an SBCL runtime binary, SBCL's own sbcl.core for it, and a list of
the runtime and core paths tried, or NIL, NIL and that list when no core
exists.

The first runtime candidate with a core beside it, in the install layout
<prefix>/bin/sbcl and <prefix>/lib/sbcl/sbcl.core that nixpkgs keeps, is
taken, so that the core matches the runtime build; the wrapper script has no
core beside it.  Otherwise the first runtime candidate is paired with
the core in SBCL_HOME or SB-INT:SBCL-HOMEDIR-PATHNAME.  Either may be
unusable under the wrapper: SBCL_HOME may be unset, and SBCL derives the
home directory from argv[0] first, which the wrapper sets to its own path."
  (let* ((runtimes (stack-cleanup-sbcl-runtime-candidates))
         (pairs (mapcar (lambda (runtime)
                          (cons runtime
                                (merge-pathnames "../lib/sbcl/sbcl.core"
                                                 (uiop:pathname-directory-pathname
                                                  runtime))))
                        runtimes))
         (homes (remove nil
                        (list (let ((home (sb-ext:posix-getenv "SBCL_HOME")))
                                (and home (plusp (length home))
                                     (merge-pathnames "sbcl.core"
                                                      (uiop:ensure-directory-pathname
                                                       home))))
                              (let ((home (sb-int:sbcl-homedir-pathname)))
                                (and home (merge-pathnames "sbcl.core" home))))))
         (tried (list :runtimes (mapcar #'namestring runtimes)
                      :cores (mapcar #'namestring (append (mapcar #'cdr pairs) homes))))
         (own (find-if (lambda (pair) (probe-file (cdr pair))) pairs))
         (home (find-if #'probe-file homes)))
    (cond (own (values (car own) (cdr own) tried))
          ((and home runtimes) (values (first runtimes) home tried))
          (t (values nil nil tried)))))

(defun stack-cleanup-child-results (runtime core goals)
  "Run each of GOALS in a child SBCL, the binary RUNTIME started with CORE
under --script, which ends the process on a guard page hit, with the shipped
stack fraction.  Return the child's
exit code, its output when the code is not 0 (else NIL), cut to the first
2000 and the last 1000 characters, and, when it printed them, its stack
fraction and one list of (SOLUTION-COUNT OPENED CLOSED COPIED) per goal.  The variable Deep in a goal
stands for a term nested 4000 levels deep, too deep to copy within the
cleanup margin and shallow enough for query entry, which walks it without
the check.

The parent may itself run from a saved core whose toplevel is a test runner
(the cl-weave CLI), which would take --script as its own option, so the
child gets SBCL's own core; see STACK-CLEANUP-SBCL-RUNTIME-AND-CORE.  It gets a fresh HOME and
cache, and no other environment, so the runner's ASDF configuration (such as
output translations into a read-only store) does not reach it."
  (let* ((sbcl-home (uiop:pathname-directory-pathname core))
         (directory (uiop:ensure-directory-pathname
                     (uiop:merge-pathnames*
                      (format nil "cl-prolog-kit-stack-~36R"
                              (random (expt 2 64) (make-random-state t)))
                      (uiop:temporary-directory))))
         (script (merge-pathnames "child.lisp" directory)))
    (ensure-directories-exist (merge-pathnames "cache/" directory))
    (unwind-protect
         (progn
           (with-open-file (stream script :direction :output)
             (write-string (stack-cleanup-child-script goals) stream))
           (let* ((output (make-string-output-stream))
                  (process
                    (sb-ext:run-program
                     runtime
                     (list "--core" (namestring core)
                           "--script" (namestring script))
                     :environment
                     (list (format nil "SBCL_HOME=~A" (namestring sbcl-home))
                           (format nil "HOME=~A" (namestring directory))
                           (format nil "XDG_CACHE_HOME=~Acache/" (namestring directory))
                           (format nil "CL_SOURCE_REGISTRY=~A"
                                   (namestring
                                    (asdf:system-source-directory "cl-prolog-kit"))))
                     :directory directory
                     :input nil :output output :error output :wait t))
                  (code (sb-ext:process-exit-code process))
                  (text (get-output-stream-string output))
                  (printed (and (eql code 0)
                                (ignore-errors (read-from-string text)))))
             (values code
                     (unless (eql code 0)
                       (if (<= (length text) 3000)
                           text
                           (concatenate 'string (subseq text 0 2000) " ... "
                                        (subseq text (- (length text) 1000)))))
                     (and (consp printed) (car printed))
                     (and (consp printed) (cdr printed)))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defun stack-cleanup-shipped-limit-assertions (runtime core)
  (multiple-value-bind (code output-tail fraction results)
      (stack-cleanup-child-results
       runtime core
       '("catch(rec(0), error(resource_error(control_stack), _), true)."
         "assertz(deep(Deep)),
          catch(rec_deep(0), error(resource_error(control_stack), _), true)."))
    ;; The tail of the child's output is NIL on exit code 0 and shows in
    ;; the failure report otherwise.
    (is-equal '(0 nil) (list code output-tail))
    (is-equal 4/5 fraction)
    (is-equal 2 (length results))
    (loop for (solutions opened closed) in results
          do (is-equal 1 solutions)
             ;; A quarter of the stack holds about 300 levels of rec/1.
             (is (> opened 600))
             (is-equal opened closed))
    (is-equal 0 (fourth (second results)))))

(deftest cleanups-after-a-stack-stop-run-at-the-shipped-limit (:timeout 120)
  ;; A child SBCL loads the system and runs about 2000 levels of proof
  ;; search, so this takes longer than the default timeout.  Run at the
  ;; shipped fraction, where before the cleanup margin the
  ;; cleanups of rec/1 stopped again one on top of the other until SBCL
  ;; ended the process.  Every cleanup runs at about the depth of the stop
  ;; and fits in the margin, so each opened level is closed.  In rec_deep/1
  ;; each cleanup then copies a term too deep for the margin and stops at
  ;; the ceiling, so none reaches copied/1; that stop is dropped while the
  ;; first one unwinds, so catch/3 still receives the resource error.
  (multiple-value-bind (runtime core tried) (stack-cleanup-sbcl-runtime-and-core)
    ;; Names the paths tried in the failure report when none exists.
    (is-equal nil (unless core (list :no-sbcl-core-among tried)))
    (when core
      (stack-cleanup-shipped-limit-assertions runtime core))))
