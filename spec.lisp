;;
;;  facts - in-memory graph database
;;
;;  Copyright 2011-2014 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
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
