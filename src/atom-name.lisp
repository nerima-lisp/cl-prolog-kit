;;;; The bijection between a Prolog atom's text and the Common Lisp symbol
;;;; that represents it.
;;;;
;;;; Prolog atoms are ordinary Common Lisp symbols, so that a Lisp-authored
;;;; rulebase -- `(parent ?x ?y)', read by the Common Lisp reader under its
;;;; default :UPCASE readtable case -- and a Prolog-authored one --
;;;; `parent(X, Y).', read by this engine's parser -- denote the same
;;;; predicate.  That fixes the canonical spelling of an atom whose text holds
;;;; no upper-case letter: it is the UPCASED symbol name, so `foo' is
;;;; CL-PROLOG-KIT::FOO and `%ATOM-TEXT' recovers "foo" by downcasing.
;;;;
;;;; An atom whose text does not survive that round trip -- anything holding an
;;;; upper-case letter, such as `fooBar', `FooBar', or `Hello world' -- can
;;;; neither be stored upcased (its spelling would be lost) nor be stored
;;;; verbatim in CL-PROLOG-KIT (the name "ABC" of `'ABC'' would collide with the
;;;; canonical spelling of `abc').  Such atoms are interned verbatim in their
;;;; own package, CL-PROLOG-KIT.VERBATIM-ATOMS.
;;;;
;;;; The home package therefore records which of the two encodings is in
;;;; force, which makes %INTERN-PROLOG-ATOM and %ATOM-TEXT exact inverses.
;;;; Quoting an atom is consequently invisible to the engine, as ISO 13211-1
;;;; 6.4.2 requires: `hello == 'hello'' holds, and so does `'FooBar' \== foobar'.
;;;;
;;;; Two atoms are the same Prolog atom exactly when their texts are equal, so
;;;; unification (%SAME-ATOM-TEXT-P) and the standard order of terms
;;;; (%COMPARE-ATOM-TEXTS) both decide on text and never on symbol identity --
;;;; a distinction that matters because a name inherited from COMMON-LISP is
;;;; interned in CL-PROLOG-KIT.USER-ATOMS by the parser but in COMMON-LISP by the
;;;; Lisp reader.  Both comparisons read characters in place, so neither
;;;; allocates the downcased text they compare.

(in-package #:cl-prolog-kit)

;;; Cached for the classification predicates below, which run on every term
;;; node the engine walks.  Interning resolves CL-PROLOG-KIT by name instead -- see
;;; %INTERN-PROLOG-ATOM.
(defvar *user-atom-package* (find-package '#:cl-prolog-kit.user-atoms))

(defvar *verbatim-atom-package* (find-package '#:cl-prolog-kit.verbatim-atoms))

(defparameter +empty-list-atom-text+ "[]"
  "The text of the atom Common Lisp NIL stands for.  ISO 13211-1 6.3.5 makes
`[]' an atom, and `[] == '[]'' holds because both spellings denote it.")

(declaim (inline %verbatim-atom-p))
(defun %verbatim-atom-p (atom)
  "True when ATOM's symbol name is its Prolog text verbatim, rather than upcased."
  (eq (symbol-package atom) *verbatim-atom-package*))

(defun %atom-text-upcase-canonical-p (text)
  "True when TEXT is recoverable from (STRING-UPCASE TEXT) by STRING-DOWNCASE.

False for any TEXT holding an upper-case letter, and also for the characters
whose upcase/downcase pair is not involutive (U+0131 DOTLESS I, say), which
must be stored verbatim for the same reason."
  (string= text (string-downcase (string-upcase text))))

(defun %reserved-atom-name-p (name)
  "True when NAME would read back as a logic variable rather than an atom."
  (and (plusp (length name)) (char= (char name 0) #\?)))

(defun %intern-prolog-atom (text &optional (interner #'intern))
  "Return the Prolog atom whose text is TEXT.

INTERNER is called as (INTERNER NAME PACKAGE) in place of INTERN, so the parser
can account a newly interned name against its symbol budget."
  (cond
    ((string= text +empty-list-atom-text+) nil)
    ((%atom-text-upcase-canonical-p text)
     ;; Resolved by name, not through the cached package object, so interning
     ;; into a CL-PROLOG-KIT that has been renamed out from under the engine fails
     ;; loudly instead of populating a package nothing can find again.
     (let ((name (string-upcase text))
           (package (find-package '#:cl-prolog-kit)))
       (multiple-value-bind (symbol status) (find-symbol name package)
         ;; A name inherited from COMMON-LISP must not be interned into
         ;; CL-PROLOG-KIT (that would shadow the Lisp binding), and a `?'-prefixed
         ;; name must not become a logic variable; both go to USER-ATOMS.
         (if (or (eq status :inherited) (%reserved-atom-name-p name))
             (funcall interner name (find-package '#:cl-prolog-kit.user-atoms))
             (or symbol (funcall interner name package))))))
    (t (funcall interner text (find-package '#:cl-prolog-kit.verbatim-atoms)))))

(defun %atom-text-encoding (atom)
  "Return the string ATOM's text is encoded in, and true when it is verbatim.

An uninterned symbol counts as verbatim.  The engine builds one exactly when it
must carry untrusted text -- a missing source's pathname, a syntax error's
description, a stream handle, a malformed format string -- as an atom without
interning it, and it sets the symbol name to that text unchanged."
  (cond
    ((null atom) (values +empty-list-atom-text+ nil))
    ((null (symbol-package atom)) (values (symbol-name atom) t))
    (t (values (symbol-name atom) (%verbatim-atom-p atom)))))

(defun %atom-text (atom)
  "Return ATOM's Prolog text: the exact inverse of %INTERN-PROLOG-ATOM."
  (multiple-value-bind (name verbatimp) (%atom-text-encoding atom)
    (if verbatimp name (string-downcase name))))

(defun %atom-text-length (atom)
  "Return the number of characters in ATOM's Prolog text without building it."
  (length (%atom-text-encoding atom)))

(defun %same-atom-text-p (left right)
  "True when the atoms LEFT and RIGHT have the same Prolog text.

A verbatim encoding always holds an upper-case letter and an upcased one never
decodes to one, so texts can only agree when both atoms share an encoding."
  (or (eq left right)
      (multiple-value-bind (left-name left-verbatim) (%atom-text-encoding left)
        (multiple-value-bind (right-name right-verbatim) (%atom-text-encoding right)
          (and (eq (not left-verbatim) (not right-verbatim))
               (string= left-name right-name))))))

(defun %compare-atom-texts (left right)
  "Return -1, 0, or 1 as the atom LEFT's text precedes, equals, or follows
RIGHT's in the character-code order the standard order of terms uses."
  (multiple-value-bind (left-name left-verbatim) (%atom-text-encoding left)
    (multiple-value-bind (right-name right-verbatim) (%atom-text-encoding right)
      (let ((left-length (length left-name))
            (right-length (length right-name)))
        (dotimes (index (min left-length right-length))
          (let ((left-character (char left-name index))
                (right-character (char right-name index)))
            (unless left-verbatim
              (setf left-character (char-downcase left-character)))
            (unless right-verbatim
              (setf right-character (char-downcase right-character)))
            (cond ((char< left-character right-character)
                   (return-from %compare-atom-texts -1))
                  ((char> left-character right-character)
                   (return-from %compare-atom-texts 1)))))
        (cond ((< left-length right-length) -1)
              ((> left-length right-length) 1)
              (t 0))))))

(defun prolog-atom (text)
  "Return the Prolog atom whose printed text is the string TEXT.

Use this rather than a literal symbol whenever TEXT is not already lower case:
`(prolog-atom \"FooBar\")' is the atom Prolog source spells `'FooBar'', which
is a different atom from CL-PROLOG-KIT::|FooBar|."
  (%intern-prolog-atom text))

(defun prolog-atom-text (atom)
  "Return the string ATOM prints as, the inverse of PROLOG-ATOM."
  (%atom-text atom))
