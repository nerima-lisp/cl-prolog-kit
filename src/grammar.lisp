;;;; Precedence-climbing grammar parser for conventional Prolog source
;;;; text, built on top of the token stream produced by lexer.lisp, plus
;;;; the public read-prolog-term/read-prolog-clause/parse-prolog API.

(in-package #:cl-prolog-kit)

(defun %current-token (parser)
  (aref (%parser-tokens parser) (%parser-position parser)))

(defun %peek-token (parser &optional (offset 1))
  "Return the token OFFSET positions past the current one.

Reading past the end yields the terminating :EOF token, which every token
vector carries, so a caller never has to bounds-check its lookahead."
  (let ((tokens (%parser-tokens parser)))
    (aref tokens (min (+ (%parser-position parser) offset) (1- (length tokens))))))

(defun %accept-token (parser kind &optional value)
  (let ((token (%current-token parser)))
    (when (and
        (eq kind (%token-kind token))
        (or (null value) (equal value (%token-value token))))
      (incf (%parser-position parser))
      token)))

(defun %expect-token (parser kind &optional value)
  (or
    (%accept-token parser kind value)
    (%parse-error
      "Expected Prolog token ~S~@[ ~S~], got ~S."
      kind
      value
      (%current-token parser))))

(defun %prolog-symbol (name &key (position 0) track-resource-p)
  "Intern NAME as an engine-internal CL-PROLOG-KIT symbol (an operator or control
functor), which is always the upcased spelling."
  (let ((canonical-name (string-upcase name))
        (package (find-package '#:cl-prolog-kit)))
    (if track-resource-p
        (%intern-parser-symbol canonical-name package position)
        (intern canonical-name package))))

(defun %prolog-atom-symbol (name &key (position 0) track-resource-p)
  "Return the Prolog atom whose text is NAME, accounting the interning against
the parser's symbol budget when TRACK-RESOURCE-P.  See src/atom-name.lisp for
the text/symbol mapping this uses; quoting is not part of it, so `'foo'' and
`foo' return the same atom."
  (%intern-prolog-atom
   name
   (if track-resource-p
       (lambda (symbol-name target-package)
         (%intern-parser-symbol symbol-name target-package position))
       #'intern)))

(defun %variable-symbol (name variables &key (position 0))
  (if (string= name "_")
      (fresh-logic-variable "?ANON")
      (or (gethash name variables)
          (setf (gethash name variables)
                (%intern-parser-symbol
                 (concatenate 'string "?" (string-upcase name))
                 (find-package '#:cl-prolog-kit)
                 position)))))

(defun %operator-definition-for-token (parser token specifiers)
  (when (eq :operator (%token-kind token))
    (find-if
     (lambda (definition)
       (and (member (operator-definition-specifier definition)
                    specifiers
                    :test #'eq)
            (string= (%token-value token)
                     (%operator-lexeme definition))))
     (%operator-table-current (%parser-operator-table parser)))))

(defun %operator-binding-power (definition)
  (- +maximum-operator-priority+
     (operator-definition-priority definition)))

(defun %binary-operator-definition (parser token)
  (%operator-definition-for-token parser token '(:xfx :xfy :yfx)))

(defun %prefix-operator-definition (parser token)
  (%operator-definition-for-token parser token '(:fx :fy)))

(defun %operator-symbol (operator &optional (position 0))
  (cond ((string= operator ",") 'and)
        ((string= operator ";") 'or)
        ((string= operator "\\+") 'not)
        (t (%prolog-symbol operator
                           :position position
                           :track-resource-p t))))

(defun %normalize-control-expression (operator left right &optional (position 0))
  (cond
    ((and (string= operator ";") (consp left) (eq (first left) '->))
     (list 'if-then-else (second left) (third left) right))
    ((and (string= operator ";") (consp left) (eq (first left) '*->))
     (list 'soft-if-then-else (second left) (third left) right))
    (t (list (%operator-symbol operator position) left right))))

(declaim (ftype function %parse-expression %parse-list))

(defvar *double-quotes* :codes
  "How a \"...\" literal is read: :codes (ISO default), :chars, :atom, or
:string.  Flag-aware readers bind this from the rulebase's double_quotes flag;
the direct reader APIs default to :codes.")

(defun %double-quoted-value (text position)
  "Convert the raw TEXT of a \"...\" literal per *DOUBLE-QUOTES*."
  (ecase *double-quotes*
    (:codes (map 'list #'char-code text))
    (:chars (map 'list
                 (lambda (character)
                   (%prolog-atom-symbol (string character) :position position))
                 text))
    (:atom (%prolog-atom-symbol text :position position
                                :track-resource-p t))
    (:string text)))

(defparameter +structural-operator-lexemes+ '(")" "]" "}" "," "|" ".")
  "Operator lexemes that never stand for an atom where a term is expected.

Each closes or separates a construct, so meeting one in a term position is a
syntax error rather than the atom of the same name.  The quoted spellings
(`','', `'|'') still read as atoms, since quoting bypasses this table.")

(defun %term-start-token-p (token)
  "True when TOKEN could begin a term, so a preceding operator has an operand."
  (not (or (eq :eof (%token-kind token))
           (and (eq :operator (%token-kind token))
                (member (%token-value token) +structural-operator-lexemes+
                        :test #'string=)))))

(defun %operator-token-is-atom-p (parser token)
  "True when the operator TOKEN denotes the atom of the same name.

ISO 13211-1 6.3.3.1 admits an atom that is an operator as an argument and
6.3.4.3 admits it bracketed, so `functor(T, +, 2)', `T =.. [+, 1, 2]',
`sort(0, @<, L, S)' and `X = (+)' are all well-formed.  Those cases look alike
from here: no operand follows, so the operator has nothing to apply to and the
only reading left is the atom.

An operator used as the left operand of another operator (`+ == '+'') is not
covered -- ISO requires brackets there, and guessing would change how a
genuine prefix operator parses."
  (and (eq :operator (%token-kind token))
       (not (member (%token-value token) +structural-operator-lexemes+
                    :test #'string=))
       (not (%term-start-token-p (%peek-token parser)))))

(defun %parse-primary (parser variables minimum-precedence)
  (let ((token (%current-token parser)))
    (cond
      ((%accept-token parser :operator "(")
       (prog1 (%parse-expression parser variables 0)
         (%expect-token parser :operator ")")))
      ((%accept-token parser :operator "[")
       (let ((list (%parse-list parser variables)))
         (if *parsing-dcg-body-p* (list 'dcg-terminals list) list)))
      ((%accept-token parser :operator "{")
       ;; ISO 13211-1 6.3.6 makes a bare `{}' the atom of that name; only a
       ;; non-empty `{T}' is the curly-brace term.
       (if (%accept-token parser :operator "}")
           (%prolog-atom-symbol "{}" :position (%token-position token)
                                     :track-resource-p t)
           (prog1 (list 'brace (let ((*parsing-dcg-body-p* nil))
                                 (%parse-expression parser variables 0)))
             (%expect-token parser :operator "}"))))
      ;; Checked before the prefix-operator branch: `- 1' is prefix minus
      ;; because a term follows, while the `-' in `f(-, 1)' is the atom.
      ((%operator-token-is-atom-p parser token)
       (incf (%parser-position parser))
       (%prolog-atom-symbol (%token-value token)
                            :position (%token-position token)
                            :track-resource-p t))
      ((%prefix-operator-definition parser token)
       (let* ((definition (%prefix-operator-definition parser token))
              (binding-power (%operator-binding-power definition)))
         (when (< binding-power minimum-precedence)
           (%parse-error "Prefix Prolog operator ~A is not valid in this context."
                         (%token-value token)))
         (incf (%parser-position parser))
         ;; A "-" immediately preceding a number is that number's negative
         ;; sign, not the prefix minus operator applied to a positive
         ;; number: this keeps a written negative number's canonical text
         ;; reading back as an atomic negative number rather than the
         ;; compound term (- N).
         (if (and (string= (%token-value token) "-")
                  (eq :number (%token-kind (%current-token parser))))
             (prog1 (- (%token-value (%current-token parser)))
               (incf (%parser-position parser)))
             (list (%operator-symbol (%token-value token)
                                     (%token-position token))
                   (%parse-expression
                    parser variables
                    (if (eq :fx (operator-definition-specifier definition))
                        (1+ binding-power)
                        binding-power))))))
      ((eq :number (%token-kind token))
       (incf (%parser-position parser))
       (%token-value token))
      ((eq :variable (%token-kind token))
       (incf (%parser-position parser))
       (%variable-symbol (%token-value token)
                         variables
                         :position (%token-position token)))
      ((eq :string (%token-kind token))
       (incf (%parser-position parser))
       (%double-quoted-value (%token-value token) (%token-position token)))
      ((member (%token-kind token) '(:atom :quoted-atom))
       (incf (%parser-position parser))
       (let ((atom (%prolog-atom-symbol
                    (%token-value token)
                    :position (%token-position token)
                    :track-resource-p t)))
         (if (%accept-token parser :operator "(")
             (let ((arguments '()))
               (unless (%accept-token parser :operator ")")
                 (loop (push (let ((*parsing-dcg-body-p* nil))
                               (%parse-expression parser variables 201))
                             arguments)
                       (cond ((%accept-token parser :operator ","))
                             (t
                              (%expect-token parser :operator ")")
                              (return)))))
               (cons atom (nreverse arguments)))
             atom)))
      (t
       (%parse-error "Expected a Prolog term, got ~S." token)))))

(defun %parse-list (parser variables)
  (when (%accept-token parser :operator "]") (return-from %parse-list '()))
  (let ((elements '()) (tail '()))
    (loop (push (%parse-expression parser variables 201) elements)
          (cond
            ((%accept-token parser :operator ","))
            ((%accept-token parser :operator "|")
             (setf tail (%parse-expression parser variables 201))
             (%expect-token parser :operator "]") (return))
            (t (%expect-token parser :operator "]") (return))))
    (reduce #'cons (nreverse elements) :from-end t :initial-value tail)))

(defun %non-associative-conflict-p (parser definition specifier)
  "True when SPECIFIER is :XFX and the next token is a binary operator sharing
DEFINITION's priority -- ISO forbids chaining same-priority non-associative
operators without parentheses."
  (and (eq specifier :xfx)
       (let ((next (%binary-operator-definition parser (%current-token parser))))
         (and next
              (= (operator-definition-priority definition)
                 (operator-definition-priority next))))))

(defun %parse-expression (parser variables minimum-precedence)
  (let ((depth (1+ (%parser-depth parser))))
    (%check-parser-limit "PARSER_DEPTH"
                         *max-prolog-parser-depth*
                         depth
                         (%token-position (%current-token parser)))
    (incf (%parser-depth parser))
    (unwind-protect
         (let ((left (%parse-primary parser variables minimum-precedence)))
           (loop for token = (%current-token parser)
                 for definition = (%binary-operator-definition parser token)
                 for precedence = (and definition
                                       (%operator-binding-power definition))
                 while (and precedence (>= precedence minimum-precedence))
                 for operator = (%token-value token)
                 for specifier = (operator-definition-specifier definition)
                 do (incf (%parser-position parser))
                    (setf left
                          (%normalize-control-expression
                           operator
                           left
                           (let ((*parsing-dcg-body-p*
                                   (or *parsing-dcg-body-p*
                                       (string= operator "-->"))))
                             (%parse-expression
                              parser
                              variables
                              (if (eq specifier :xfy)
                                  precedence
                                  (1+ precedence))))
                           (%token-position token)))
                    (when (%non-associative-conflict-p parser definition specifier)
                      (%parse-error
                       "Non-associative Prolog operator ~A cannot be chained."
                       operator))
                 finally (return left)))
      (decf (%parser-depth parser)))))

(defun %body-goals (body)
  (if (and (consp body) (eq (first body) 'and)) (rest body) (list body)))

(defun %parse-next-prolog-form (parser)
  (when (eq :eof (%token-kind (%current-token parser)))
    (return-from %parse-next-prolog-form (values nil :eof)))
  (let ((variables (make-hash-table :test #'equal)))
    (if (%accept-token parser :operator "?-")
        (let ((body (%parse-expression parser variables 0)))
          (%expect-token parser :operator ".")
          (values body :query))
        (let ((head (%parse-expression parser variables 201)))
          (if (%accept-token parser :operator ":-")
              (let ((body (%parse-expression parser variables 0)))
                (%expect-token parser :operator ".")
                (values (make-clause (if (symbolp head) (list head) head)
                                     (%body-goals body))
                        :clause))
              (progn
                (%expect-token parser :operator ".")
                (values (make-clause (if (symbolp head) (list head) head)) :clause)))))))

(defun read-prolog-term (source &optional (operator-table *standard-operator-table*))
  "Read one Prolog term from SOURCE, which may be a string or stream."
  (let* ((parser (%parser (%tokenize-prolog source operator-table) operator-table))
         (term (%parse-expression parser (make-hash-table :test #'equal) 0)))
    (%accept-token parser :operator ".")
    (%expect-token parser :eof)
    term))

(defun read-prolog-clause (source &optional (operator-table *standard-operator-table*))
  "Read one fact or rule from SOURCE and return a CLAUSE."
  (let ((parser (%parser (%tokenize-prolog source operator-table) operator-table)))
    (multiple-value-bind (form kind) (%parse-next-prolog-form parser)
      (unless (eq kind :clause) (%parse-error "Expected a Prolog clause."))
      (%expect-token parser :eof)
      form)))

(defun parse-prolog (source &optional (operator-table *standard-operator-table*))
  "Parse SOURCE into CLAUSE objects and untagged query goal forms in source order."
  (let ((parser (%parser (%tokenize-prolog source operator-table) operator-table))
        (forms '()))
    (loop (multiple-value-bind (form kind) (%parse-next-prolog-form parser)
            (when (eq kind :eof) (return (nreverse forms)))
            (push form forms)))))
