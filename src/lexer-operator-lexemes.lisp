;;;; Operator-lexeme tables: deriving the plain-atom-named ("word") and
;;;; symbolic-token lexeme lists an operator table makes available to the
;;;; tokenizer (lexer-tokenizer.lisp), with a cache for the immutable
;;;; standard operator table.

(in-package #:cl-prolog-kit)

(defun %identifier-character-p (character)
  (or (alphanumericp character) (char= character #\_)))

(defun %operator-lexeme (definition)
  (string-downcase (symbol-name (operator-definition-name definition))))

(defparameter +prolog-graphic-characters+ "#$&*+-./:<=>?@^~\\"
  "The ISO 13211-1 6.4.2 graphic characters.

A maximal run of them is one graphic token, and a graphic token is an atom
whether or not it has been declared an operator -- which is what lets
`:- op(700, xfx, ===).' name an operator that does not exist yet.")

(defun %prolog-graphic-character-p (character)
  (and character (find character +prolog-graphic-characters+ :test #'char=) t))

(defun %plain-prolog-atom-name-p (lexeme)
  (and (plusp (length lexeme))
       (lower-case-p (char lexeme 0))
       (every #'%identifier-character-p lexeme)))

(defun %operator-lexemes (table &optional word-p)
  (remove-duplicates
   (loop for definition in (%operator-table-current table)
         for lexeme = (%operator-lexeme definition)
         when (eq word-p (%plain-prolog-atom-name-p lexeme))
           collect lexeme)
   :test #'string=))

(defun %compute-symbolic-token-lexemes (table)
  (coerce
   (sort
    (coerce (remove-duplicates
             (append (%operator-lexemes table nil)
                     (list "(" ")" "[" "]" "{" "}" "." "|"))
             :test #'string=)
            'vector)
    #'> :key #'length)
   'list))

(defvar *operator-lexeme-cache* (make-hash-table :test #'eq))

(defun %operator-table-lexemes (table)
  "Cache lexemes only for the immutable standard operator table."
  (if (eq table *standard-operator-table*)
      (progn
        (when (> (hash-table-count *operator-lexeme-cache*) 1)
          (clrhash *operator-lexeme-cache*))
        (or (gethash table *operator-lexeme-cache*)
            (setf (gethash table *operator-lexeme-cache*)
                  (cons (%operator-lexemes table t)
                        (%compute-symbolic-token-lexemes table)))))
      (cons (%operator-lexemes table t)
            (%compute-symbolic-token-lexemes table))))

(defun %standard-operator-lexemes (&optional word-p)
  (if word-p
      (car (%operator-table-lexemes *standard-operator-table*))
      (%operator-lexemes *standard-operator-table*)))

(defun %symbolic-token-lexemes (&optional (table *standard-operator-table*))
  (if (eq table *standard-operator-table*)
      (cdr (%operator-table-lexemes table))
      (%compute-symbolic-token-lexemes table)))
