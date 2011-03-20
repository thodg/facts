;;
;;  lowh-facts  -  facts database
;;
;;  Copyright 2011 Thomas de Grivel <billitch@gmail.com>
;;
;;  All rights reserved
;;

(defpackage :lowh-facts
  (:nicknames :facts)
  (:use :cl :lessp)
  (:export #:add #:with))

(in-package :lowh-facts)

;;  Fact

(deftype fact/v () '(simple-vector 3))

(defun make-fact/v (spec-or-subject &optional predicate (object nil object-p))
  (the fact/v
    (if object-p
	(make-array 3 :initial-contents (list spec-or-subject predicate object))
	(make-array 3 :initial-contents spec-or-subject))))

(defun fact/v-subject (f)
  (svref f 0))

(defun fact/v-predicate (f)
  (svref f 1))

(defun fact/v-object (f)
  (svref f 2))

(deftype fact/l () '(cons t (cons t (cons t null))))

(deftype fact () '(or fact/v fact/l))

(defun fact-subject (f)
  (if (consp f)
      (first f)
      (svref f 0)))

(defun fact-predicate (f)
  (declare (type fact f))
  (if (consp f)
      (second f)
      (svref f 1)))

(defun fact-object (f)
  (declare (type fact f))
  (if (consp f)
      (third f)
      (svref f 2)))

;;  Fact index order

(defun lessp/3 (a1 a2 a3 b1 b2 b3)
  (or (lessp a1 b1)
      (and (not (lessp b1 a1))
	   (or (lessp a2 b2)
	       (and (not (lessp b2 a2))
		    (lessp a3 b3))))))

(defun fact-spo-lessp (a b)
  (lessp/3 (elt a 0) (elt a 1) (elt a 2)
	   (elt b 0) (elt b 1) (elt b 2)))

(defun fact-pos-lessp (a b)
  (lessp/3 (elt a 1) (elt a 2) (elt a 0)
	   (elt b 1) (elt b 2) (elt b 0)))

(defun fact-osp-lessp (a b)
  (lessp/3 (elt a 2) (elt a 0) (elt a 1)
	   (elt b 2) (elt b 0) (elt b 1)))

;;  Database

(defstruct db
  (spo-tree (llrbtree:make-tree :lessp #'fact-spo-lessp))
  (pos-tree (llrbtree:make-tree :lessp #'fact-pos-lessp))
  (osp-tree (llrbtree:make-tree :lessp #'fact-osp-lessp)))

(defparameter *db* (make-db))

(defun db-clear ()
  (setf *db* (make-db)))

(defun db-get (fact &optional (db *db*))
  (llrbtree:tree-get fact (db-spo-tree db)))

(defun db-insert (subject predicate object &optional (db *db*))
  (let ((fact (make-fact/v subject predicate object)))
    (or (llrbtree:tree-get fact (db-spo-tree db))
	(setf (llrbtree:tree-get fact (db-spo-tree db)) fact
	      (llrbtree:tree-get fact (db-pos-tree db)) fact
	      (llrbtree:tree-get fact (db-osp-tree db)) fact))))

(defun db-delete (fact-spec &optional (db *db*))
  (let ((fact (llrbtree:tree-get fact-spec (db-spo-tree db))))
    (when fact
      (llrbtree:tree-delete fact (db-spo-tree db))
      (llrbtree:tree-delete fact (db-pos-tree db))
      (llrbtree:tree-delete fact (db-osp-tree db))
      fact)))

;;  Bindings

(defun binding-symbol-p (sym)
  (when (typep sym 'symbol)
    (char= #\? (char (symbol-name sym) 0))))

(defstruct binding name bound)

(defun collect-bindings% (form &optional bindings)
  (typecase form
    (null bindings)
    (symbol (if (binding-symbol-p form)
		(pushnew form bindings)
		bindings))
    (cons (collect-bindings% (car form)
			     (collect-bindings% (cdr form)
						bindings)))
    (t bindings)))

(defun collect-bindings (form)
  (mapcar (lambda (name)
	    (make-binding :name name))
	  (collect-bindings% form)))

(defun replace-bindings (bindings form)
  (sublis (mapcar (lambda (b) `(,(binding-name b) . ,b)) bindings)
	  form))

(defmacro add (&rest facts-definition)
  (let ((bindings (collect-bindings facts-definition)))
    `(let ,(mapcar (lambda (b)
		     `(,(binding-name b)
			',(gensym (concatenate 'string
					       (symbol-name (binding-name b))
					       "-"))))
		   bindings)
       ,@(mapcar (lambda (fact)
		   `(db-insert ,@fact))
		 facts-definition))))

(defun binding-boundp (b)
  (and (typep b 'binding)
       (binding-bound b)))

(defun binding-unboundp (b)
  (and (typep b 'binding)
       (not (binding-bound b))))

(defmacro with/rec (((s p o) &rest bindings) &body body)
  (let ((g!fact (gensym "FACT-"))
	(g!value (gensym "VALUE-"))
	(g!s (unless (binding-unboundp s) (gensym "S-")))
	(g!p (unless (binding-unboundp p) (gensym "P-")))
	(g!o (unless (binding-unboundp o) (gensym "O-")))
	(?s (when (binding-unboundp s) (setf (binding-bound s) (binding-name s))))
	(?p (when (binding-unboundp p) (setf (binding-bound p) (binding-name p))))
	(?o (when (binding-unboundp o) (setf (binding-bound o) (binding-name o))))
	(s (if (binding-p s) (binding-name s) s))
	(p (if (binding-p p) (binding-name p) p))
	(o (if (binding-p o) (binding-name o) o))
	(body (if bindings `((with/rec ,bindings ,@body)) body)))
    (cond ((and g!s g!p g!o)
	   `(when (db-get (make-fact/v ,s ,p ,o))
	      ,@body))
	  ((not (or g!s g!p g!o))
	   `(llrbtree:map-tree (lambda (,g!fact ,g!value)
				 (declare (ignore ,g!value))
				 (let ((,s (fact/v-subject ,g!fact))
				       (,p (fact/v-predicate ,g!fact))
				       (,o (fact/v-object ,g!fact)))
				   ,@body))
			       (db-spo-tree *db*)))
	  (t
	   `(let (,@(when g!s `((,g!s ,s)))
		  ,@(when g!p `((,g!p ,p)))
		  ,@(when g!o `((,g!o ,o))))
	      (llrbtree:map-tree
	       (lambda (,g!fact ,g!value)
		 (declare (ignore ,g!value))
		 (let (,@(when ?s `((,?s (fact/v-subject ,g!fact))))
		       ,@(when ?p `((,?p (fact/v-predicate ,g!fact))))
		       ,@(when ?o `((,?o (fact/v-object ,g!fact)))))
		   (unless (and ,@(when g!s `((equal (fact/v-subject ,g!fact) ,g!s)))
				,@(when g!p `((equal (fact/v-predicate ,g!fact) ,g!p)))
				,@(when g!o `((equal (fact/v-object ,g!fact) ,g!o))))
		     (return (values)))
		   ,@body))
	       (,(cond ((and g!s (or g!p (not g!o))) 'db-spo-tree)
		       (g!p 'db-pos-tree)
		       (t 'db-osp-tree))
		 *db*)
	       :start (make-fact/v ,g!s ,g!p ,g!o)))))))

(defmacro with (bindings-spec &body body)
  (let* ((bindings (collect-bindings bindings-spec)))
    `(block nil
       (with/rec ,(replace-bindings bindings bindings-spec)
	 ,@body))))
