;;;; Persistent operator table tests.

(in-package #:cl-prolog-kit.tests)

(defun operator-summary (definition)
  (list (cl-prolog-kit::operator-definition-priority definition)
        (cl-prolog-kit::operator-definition-specifier definition)
        (cl-prolog-kit::operator-definition-name definition)))

(deftest operator-table-validates-domain ()
  (dolist (specifier '(:fx :fy :xf :yf :xfx :xfy :yfx))
    (let* ((empty (cl-prolog-kit::%make-operator-table '()))
           (table (cl-prolog-kit::%operator-table-define empty 'sample 1 specifier)))
      (is-equal (list (list 1 specifier 'sample))
                (mapcar #'operator-summary
                        (cl-prolog-kit::%operator-table-current table)))))
  (let ((empty (cl-prolog-kit::%make-operator-table '())))
    (is (cl-prolog-kit::operator-table-p
         (cl-prolog-kit::%operator-table-define empty 'low 1 :xfx)))
    (is (cl-prolog-kit::operator-table-p
         (cl-prolog-kit::%operator-table-define empty 'high 1200 :xfx)))
    (signals-error (cl-prolog-kit::%operator-table-define empty 'bad -1 :xfx))
    (signals-error (cl-prolog-kit::%operator-table-define empty 'bad 1201 :xfx))
    (signals-error (cl-prolog-kit::%operator-table-define empty 'bad 1 :unknown))
    (signals-error (cl-prolog-kit::%operator-table-define empty "bad" 1 :xfx))))

(deftest operator-table-updates-are-persistent ()
  (let* ((empty (cl-prolog-kit::%make-operator-table '()))
         (infix (cl-prolog-kit::%operator-table-define empty 'shared 500 :yfx))
         (both (cl-prolog-kit::%operator-table-define infix 'shared 200 :fy))
         (unchanged (cl-prolog-kit::%operator-table-define both 'shared 500 :yfx))
         (redefined (cl-prolog-kit::%operator-table-define both 'shared 600 :yfx))
         (removed (cl-prolog-kit::%operator-table-define redefined 'shared 0 :fy)))
    (is-equal '() (cl-prolog-kit::%operator-table-current empty))
    (is-equal '((500 :yfx shared))
              (mapcar #'operator-summary (cl-prolog-kit::%operator-table-current infix)))
    (is-equal '((200 :fy shared) (500 :yfx shared))
              (mapcar #'operator-summary (cl-prolog-kit::%operator-table-current both)))
    (is (eq both unchanged))
    (is-equal '((200 :fy shared) (600 :yfx shared))
              (mapcar #'operator-summary (cl-prolog-kit::%operator-table-current redefined)))
    (is-equal '((600 :yfx shared))
              (mapcar #'operator-summary (cl-prolog-kit::%operator-table-current removed)))
    (is (eq removed (cl-prolog-kit::%operator-table-remove removed 'missing :xfx)))))

(deftest operator-table-query-and-order-are-deterministic ()
  (let* ((empty (cl-prolog-kit::%make-operator-table '()))
         (table (cl-prolog-kit::%operator-table-define empty 'zeta 500 :yfx))
         (table (cl-prolog-kit::%operator-table-define table 'alpha 500 :yfx))
         (table (cl-prolog-kit::%operator-table-define table 'alpha 500 :fx))
         (table (cl-prolog-kit::%operator-table-define table 'omega 200 :xfy)))
    (is-equal '((200 :xfy omega) (500 :fx alpha) (500 :yfx alpha) (500 :yfx zeta))
              (mapcar #'operator-summary (cl-prolog-kit::%operator-table-current table)))
    (is-equal '((500 :fx alpha) (500 :yfx alpha))
              (mapcar #'operator-summary
                      (cl-prolog-kit::%operator-table-find table 'alpha)))
    (is-equal '((500 :yfx alpha))
              (mapcar #'operator-summary
                      (cl-prolog-kit::%operator-table-find table 'alpha :yfx)))
    (let ((first (cl-prolog-kit::%operator-table-current table)))
      (setf (rest first) nil)
      (is-equal 4 (length (cl-prolog-kit::%operator-table-current table))))
    (signals-error (cl-prolog-kit::%operator-table-find table "alpha"))
    (signals-error (cl-prolog-kit::%operator-table-find table 'alpha :unknown))))

(deftest operator-table-breaks-ties-by-operator-name-text ()
  "Same-priority, same-specifier definitions enumerate in the order of their
name text -- what `current_op/3' reports -- and not by the home package of the
symbol that happens to represent each name, which is an internal detail."
  (let* ((empty (cl-prolog-kit::%make-operator-table '()))
         (table (cl-prolog-kit::%operator-table-define empty 'cl-prolog-kit::zeta 500 :yfx))
         (table (cl-prolog-kit::%operator-table-define table 'cl-prolog-kit.tests::alpha 500 :yfx))
         (table (cl-prolog-kit::%operator-table-define table 'cl-prolog-kit.tests::beta 500 :yfx)))
    (is-equal '((500 :yfx cl-prolog-kit.tests::alpha)
                (500 :yfx cl-prolog-kit.tests::beta)
                (500 :yfx cl-prolog-kit::zeta))
              (mapcar #'operator-summary
                      (cl-prolog-kit::%operator-table-current table)))))

(deftest operator-table-identifies-a-name-by-its-text ()
  "One atom is one operator, so redefining `+' through a symbol the parser
would produce replaces the standard table's COMMON-LISP:+ entry instead of
adding a second, invisible definition at the same priority."
  (let* ((table (cl-prolog-kit::%operator-table-define
                 cl-prolog-kit::*standard-operator-table*
                 (prolog-atom "+") 700 :yfx))
         (found (cl-prolog-kit::%operator-table-find table 'cl:+ :yfx)))
    (is-equal 1 (length found))
    (is-equal 700 (cl-prolog-kit::operator-definition-priority (first found)))))

(deftest standard-operator-table-is-self-contained ()
  (let ((before (cl-prolog-kit::%operator-table-current
                 cl-prolog-kit::*standard-operator-table*)))
    (is-equal (length cl-prolog-kit::+standard-operator-declarations+)
              (length before))
    (dolist (definition before)
      (cl-prolog-kit::%operator-table-find
       cl-prolog-kit::*standard-operator-table*
       (cl-prolog-kit::operator-definition-name definition)
       (cl-prolog-kit::operator-definition-specifier definition)))
    (is-equal before
              (cl-prolog-kit::%operator-table-current
               cl-prolog-kit::*standard-operator-table*)))
  (dolist (expected '((1200 :xfx cl-prolog-kit::|:-|)
                       (1200 :fx cl-prolog-kit::|:-|)
                       (1100 :xfy cl-prolog-kit::|;|)
                       (1000 :xfy cl-prolog-kit::|,|)
                       (900 :fy cl-prolog-kit::|\\+|)
                       (700 :xfx cl-prolog-kit::|=:=|)
                      (700 :xfx =) (500 :yfx +) (200 :fy -)))
    (destructuring-bind (priority specifier name) expected
      (is-equal (list expected)
                (mapcar #'operator-summary
                        (remove-if-not
                         (lambda (definition)
                           (= priority
                              (cl-prolog-kit::operator-definition-priority definition)))
                         (cl-prolog-kit::%operator-table-find
                          cl-prolog-kit::*standard-operator-table* name specifier)))))))
