;;
;;  lowh-facts  -  facts database
;;
;;  Copyright 2011,2012 Thomas de Grivel <billitch@gmail.com>
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

(in-package :lowh-facts)

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
  (mapcan #'expand-spec specs))
