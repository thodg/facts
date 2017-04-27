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

;;  Tools

(defun nor (&rest forms)
  (declare (dynamic-extent forms))
  (every #'null forms))

;;  WITH

(defun with/3 (form-s form-p form-o body)
  ;; TODO: compiler macro
  ;;       ,(make-fact/v form-s form-p form-o)
  `(when (db-get ,form-s ,form-p ,form-o)
     ,@body
     (values)))

(defun with/0 (var-s var-p var-o body)
  `(db-each (,var-s ,var-p ,var-o) (db-index-spo)
     ,@body))

(defun ignorable-bindings (&rest vars)
  (let ((ignorable (mapcan (lambda (x)
			     (when (char= #\- (char (symbol-name x) 0))
			       (list x)))
			   vars)))
    (when ignorable
      `((declare (ignorable ,@ignorable))))))

(defun with/1-2 (s p o var-s var-p var-o tree body)
  (let* ((value-s (unless var-s (gensym "VALUE-S-")))
	 (value-p (unless var-p (gensym "VALUE-P-")))
	 (value-o (unless var-o (gensym "VALUE-O-")))
	 (fact-s (or var-s (gensym "FACT-S-")))
	 (fact-p (or var-p (gensym "FACT-P-")))
	 (fact-o (or var-o (gensym "FACT-O-")))
	 (block-name (gensym "BLOCK-")))
    `(block ,block-name
       (let (,@(when value-s `((,value-s ,s)))
	     ,@(when value-p `((,value-p ,p)))
	     ,@(when value-o `((,value-o ,o))))
	 (db-each (,fact-s ,fact-p ,fact-o)
	     (,tree :start (make-fact/v ,value-s ,value-p ,value-o))
	   ,@(ignorable-bindings fact-s fact-p fact-o)
	   (unless (and ,@(unless var-s `((equal ,value-s ,fact-s)))
			,@(unless var-p `((equal ,value-p ,fact-p)))
			,@(unless var-o `((equal ,value-o ,fact-o))))
	     (return-from ,block-name (values)))
	   ,@body)))))

(eval-when (:compile-toplevel :load-toplevel)

  (defun with/iter (spec binding-vars body)
    (destructuring-bind (s p o) spec
      (let ((var-s (when (binding-p s) (cdr (assoc s binding-vars))))
	    (var-p (when (binding-p p) (cdr (assoc p binding-vars))))
	    (var-o (when (binding-p o) (cdr (assoc o binding-vars)))))
	(cond ((and var-s var-p var-o) (with/0 var-s var-p var-o body))
	      ((nor var-s var-p var-o) (with/3 s p o body))
	      (t (with/1-2 s p o var-s var-p var-o
			   (cond ((and (null var-s) var-o) 'db-index-spo)
				 ((null var-p)             'db-index-pos)
				 (t                        'db-index-osp))
			   body)))))))

(defmacro with/rec ((spec &rest more-specs) &body body)
  (let* ((bindings (collect-bindings spec))
	 (binding-vars (gensym-bindings bindings))
	 (body-subst (sublis binding-vars body)))
    (with/iter spec binding-vars
	       (if more-specs
		   `((with/rec ,(sublis binding-vars more-specs)
		       ,@body-subst))
		   body-subst))))

(defmacro with/expanded (binding-specs &body body)
  `(block nil
     (with/rec ,binding-specs
       ,@body)))

(defmacro with (binding-specs &body body)
  `(with/expanded ,(sort-bindings (expand-specs binding-specs))
     ,@body))

;;  With sugar, please

(defmacro bound-p (binding-specs)
  `(with ,binding-specs
     (return (values t ,@(collect-bindings binding-specs)))))

(defmacro collect (binding-specs &body body)
  (let ((g!collect (gensym "COLLECT-")))
    `(let ((,g!collect ()))
       (with ,binding-specs
	 (push (progn ,@body) ,g!collect))
       ,g!collect)))

(defmacro collect-facts (fact-specs)
  (let ((g!facts (gensym "FACTS-"))
	(specs (expand-specs fact-specs)))
    `(let (,g!facts)
       (with/expanded ,specs
	 ,@(mapcar (lambda (fact)
		     `(push (make-fact/v ,@fact) ,g!facts))
		   specs))
       (remove-duplicates ,g!facts :test #'fact-equal))))

(defmacro first-bound (binding-specs)
  ;; FIXME: detect multiple bindings
  (let* ((bindings (collect-bindings binding-specs)))
    (assert (= 1 (length bindings)) ()
	    "Invalid BINDING-SPEC: ~S
You should provide exactly one unbound variable."
	    binding-specs)
    `(with ,binding-specs
       (return ,(first bindings)))))

(defmacro let-with (let-spec &body body)
  `(let* (,@(mapcar
	     (lambda (b)
	       (if (third b)
		   `(,(first b) (or (first-bound ,(second b)) ,(third b)))
		   `(,(first b) (first-bound ,(second b)))))
	     let-spec))
     ,@body))

(defmacro push-tail (tail &rest values)
  `(setf ,@(mapcan (lambda (v)
		     `((cdr ,tail) (cons ,v nil)
		       ,tail (cdr ,tail)))
		   values)))

;;  ADD

(defmacro add (&rest specs)
  (let ((bindings (collect-bindings specs)))
    `(with-transaction
       (let ,(mapcar (lambda (b)
		       `(,b (anon ,(subseq (symbol-name b) 1))))
		     bindings)
	 ,@(mapcar (lambda (fact)
		     `(db-insert ,@fact))
		   (expand-specs specs))))))

;;  RM

(defmacro rm (specs)
  `(with-transaction
     (mapc #'db-delete (collect-facts ,specs))))
