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
  (:export #:anon #:add #:rm #:with #:when-bound
	   #:clear-db #:save-db #:load-db
	   #:binding-p #:collect-bindings))

(in-package :lowh-facts)

;;  Anonymous value

(defpackage :lowh-facts.anon
  (:nicknames :facts.anon))

(defun anon (&rest name-hints)
  (let* ((name (string-upcase (format nil "~{~A~^-~}" name-hints)))
	 (anon-pkg (find-package :lowh-facts.anon))
	 (sym (find-symbol name anon-pkg))
	 (count (when sym
		  (setf (get sym 'anon-counter)
			(1+ (or (get sym 'anon-counter) 0))))))
    (if count
	(intern (format nil "~A-~4,'0X" name count) anon-pkg)
	(intern name anon-pkg))))

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

(defun fact-equal (a b)
  (and (equal (fact-subject a) (fact-subject b))
       (equal (fact-predicate a) (fact-predicate b))
       (equal (fact-object a) (fact-object b))))

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

(defvar *db* (make-db))

(defun clear-package (package)
  (let ((pkg (typecase package
	       (package package)
	       (t (find-package package)))))
    (do-symbols (sym pkg)
      (unintern sym pkg))))

(defun clear-db ()
  (setf *db* (make-db))
  (clear-package :lowh-facts.anon))

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

(defmacro rm (facts-spec)
  (let ((g!facts (gensym "FACTS-")))
    `(let (,g!facts)
       (with ,facts-spec
	     ,@(mapcar (lambda (fact)
			 `(pushnew (make-fact/v ,@fact) ,g!facts
				   :test #'fact-equal))
		       facts-spec))
       (mapcar #'db-delete ,g!facts))))

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

(defun binding-bound (sym env)
  (when (binding-p sym)
    (find sym env)))

(defun binding-unboundp (sym env)
  (and (binding-p sym)
       (not (find sym env))))

;;  WITH

(defun with/body (env bindings body)
  (if bindings `((with/rec ,env ,bindings ,@body)) body))

(defun with/3 (env s p o bindings body)
  `(when (db-get (make-fact/v ,s ,p ,o))
     ,@(with/body env bindings body)))

(defun with/0 (env s p o bindings body)
  (let ((g!fact (gensym "FACT-"))
	(g!value (gensym "VALUE-"))
	(env (list* s p o env)))
    `(llrbtree:map-tree (lambda (,g!fact ,g!value)
			  (declare (ignore ,g!value))
			  (let ((,(binding-bound s env) (fact/v-subject ,g!fact))
				(,(binding-bound p env) (fact/v-predicate ,g!fact))
				(,(binding-bound o env) (fact/v-object ,g!fact)))
			    ,@(with/body env bindings body)))
			(db-spo-tree *db*))))

(defun with/1or2 (env s p o bindings body)
  (let ((g!block (gensym "BLOCK-"))
	(g!fact (gensym "FACT-"))
	(g!value (gensym "VALUE-"))
	(g!s (unless (binding-unboundp s env) (gensym "S-")))
	(g!p (unless (binding-unboundp p env) (gensym "P-")))
	(g!o (unless (binding-unboundp o env) (gensym "O-")))
	(?s (when (binding-unboundp s env) s))
	(?p (when (binding-unboundp p env) p))
	(?o (when (binding-unboundp o env) o))
	(env (append (when (binding-unboundp s env) `(,s))
		     (when (binding-unboundp p env) `(,p))
		     (when (binding-unboundp o env) `(,o))
		     env)))
    `(block ,g!block
       (let (,@(when g!s `((,g!s ,s)))
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
	      (return-from ,g!block (values)))
	    ,@(with/body env bindings body)))
	(,(cond ((and g!s (or g!p (not g!o))) 'db-spo-tree)
		(g!p 'db-pos-tree)
		(t 'db-osp-tree))
	  *db*)
	:start (make-fact/v ,g!s ,g!p ,g!o))))))

(defmacro with/rec (env ((s p o) &rest bindings) &body body)
  (let ((bs (not (binding-unboundp s env)))
	(bp (not (binding-unboundp p env)))
	(bo (not (binding-unboundp o env))))
    (cond ((and bs bp bo)	   (with/3 env s p o bindings body))
	  ((not (or bs bp bo))	   (with/0 env s p o bindings body))
	  (t			   (with/1or2 env s p o bindings body)))))

(defmacro with (bindings-spec &body body)
  `(block nil
     (with/rec () ,bindings-spec
       ,@body)))

(defmacro when-bound (bindings-spec)
  `(with ,bindings-spec
     (return t)))

;;  ADD

(defmacro add (&rest facts-definition)
  (let ((bindings (collect-bindings facts-definition)))
    `(unless (with ,facts-definition (return t))
       (let ,(mapcar (lambda (b)
		       `(,b (anon ,(subseq (symbol-name b) 1))))
		     bindings)
	 ,@(mapcar (lambda (fact)
		     `(db-insert ,@fact))
		   facts-definition)))))
