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
  (:export #:add #:do-facts/sp))

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

;;  Prefix

(defun fact/v-prefixp/s (fact subject)
  (equal (fact/v-subject fact) subject))

(defun fact/v-prefixp/p (fact predicate)
  (equal (fact/v-predicate fact) predicate))

(defun fact/v-prefixp/o (fact object)
  (equal (fact/v-object fact) object))

(defun fact/v-prefixp/sp (fact subject predicate)
  (and (equal (fact/v-subject fact) subject)
       (equal (fact/v-predicate fact) predicate)))

(defmacro do-facts/sp (s p o db &body body)
  (let ((key (gensym "KEY"))
	(fact (gensym "FACT")))
    `(llrbtree:map-tree (lambda (,key ,fact)
			  (let ((,o (fact-object/v ,fact)))
			    (unless (fact/v-prefixp/sp ,fact ,s ,p)
			      (return-from llrbtree:map-tree (values)))
			    ,@body))
			(db-spo-tree ,db)
			:start (make-fact/v ,s ,p nil))))

(defun fact-prefixp/po (fact predicate object)
  (and (equal (fact-predicate fact) predicate)
       (equal (fact-object fact) object)))

(defun fact-prefixp/os (fact object subject)
  (and (equal (fact-object fact) object)
       (equal (fact-subject fact) subject)))

;;  Bindings

(defun binding-p (sym)
  (when (typep sym 'symbol)
    (char= #\? (char (symbol-name sym) 0))))

(binding-p "plop")

(defun collect-bindings (form &optional bindings)
  (etypecase form
    (null bindings)
    (symbol (if (binding-p form)
		(pushnew form bindings)
		bindings))
    (cons (collect-bindings (car form)
			    (collect-bindings (cdr form)
					      bindings)))))

(defmacro add (&rest facts-definition)
  (let* ((bindings (collect-bindings facts-definition)))
    `(progn
       ,@(mapcar (lambda (fact)
		   `(db-insert ,@fact))
		 (sublis (mapcar (lambda (var)
				   (cons var
					 (gensym (symbol-name var))))
				 bindings)
			 facts-definition)))))
