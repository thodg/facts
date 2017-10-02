;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  Bindings

(defun binding-p (sym)
  (when (typep sym 'symbol)
    (char= #\? (char (symbol-name sym) 0))))

(defun collect-bindings (form &optional bindings)
  (typecase form
    (null bindings)
    (symbol (if (binding-p form)
                (pushnew form bindings)
                bindings))
    (cons (collect-bindings (car form)
                            (collect-bindings (cdr form)
                                              bindings)))
    (t bindings)))

(defun gensym-bindings (bindings)
  (mapcar (lambda (b)
            (cons b (gensym (concatenate 'string
                                         (string-upcase (subseq (string b) 1))
                                         "-"))))
          bindings))

;;  Ordering of join patterns

(defun fact-binding-count (x)
  (+ (if (binding-p (pop x)) 1 0)
     (if (binding-p (pop x)) 1 0)
     (if (binding-p (pop x)) 1 0)))

(defun pattern< (a b)
  (< (fact-binding-count a)
     (fact-binding-count b)))

(defun sort-bindings (pattern)
  "Transforms ((?s ?p ?o) (?s x y)) into ((?s x y) (?s ?p ?o)). Huge optimization."
  (sort pattern #'pattern<))

;;  Fact specifications

(defun expand-spec (spec)
  (destructuring-bind (s p o &rest more-p-o) spec
    (labels ((expand/po (p-o-list result)
               (if (endp p-o-list)
                   result
                   (destructuring-bind (p o &rest list) p-o-list
                     (expand/po list (cons `(,s ,p ,o)
                                           result))))))
      (nreverse (expand/po more-p-o
                           (cons `(,s ,p ,o)
                                 nil))))))

(defun expand-specs (specs)
  "
Facts specification

For any values of subject S, predicate P, object O we can write a fact as a triple :
    (S P O)
A join between multiple facts is written as a set of facts :
    ((S1 P1 O1) ... (Sn Pn On))
For more predicates and objects for the same subject we can also write :
    ((S P1 O1 ... Pn On))
which is equivalent to :
    ((S P1 O1) ... (S Pn On))
"
  (mapcan #'expand-spec specs))
