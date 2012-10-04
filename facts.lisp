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
  (:export #:anon
	   #:with #:bound-p #:collect #:first-bound #:let-with
	   #:add #:rm
	   #:*db* #:clear-db #:save-db #:load-db #:make-db
	   #:binding-p #:collect-bindings))

(in-package :lowh-facts)

;;  Tools

(defun nor (&rest forms)
  (declare (dynamic-extent forms))
  (every #'null forms))

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

(defmacro db-map ((var-s var-p var-o) (tree &key start end) &body body)
  (let ((g!fact (gensym "FACT-"))
	(g!value (gensym "VALUE-")))
    `(llrbtree:map-tree (lambda (,g!fact ,g!value)
			  (declare (ignore ,g!value))
			  (let ((,var-s (fact/v-subject   ,g!fact))
				(,var-p (fact/v-predicate ,g!fact))
				(,var-o (fact/v-object    ,g!fact)))
			    ,@body))
			(,tree *db*)
			,@(when start `(:start ,start))
			,@(when end   `(:end ,end)))))

(defmacro collect-facts (facts-spec)
  (let ((g!facts (gensym "FACTS-")))
    `(let (,g!facts)
       (with ,facts-spec
	 ,@(mapcar (lambda (fact)
		     `(push (make-fact/v ,@fact) ,g!facts))
		   facts-spec))
       (remove-duplicates ,g!facts :test #'fact-equal))))

(defmacro rm (facts-spec)
  `(mapc #'db-delete (collect-facts ,facts-spec)))

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

(defun binding-bound (sym env)
  (when (binding-p sym)
    (find sym env)))

(defun binding-unboundp (sym env)
  (and (binding-p sym)
       (not (find sym env))))

;;  WITH

(defun with/3 (form-s form-p form-o body)
  ;; TODO: compiler macro
  ;;       ,(make-fact/v form-s form-p form-o)
  `(when (db-get (make-fact/v ,form-s ,form-p ,form-o))
     ,@body))

(defun with/0 (var-s var-p var-o body)
  `(db-map (,var-s ,var-p ,var-o) (db-spo-tree)
     ,@body))

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
	 (db-map (,fact-s ,fact-p ,fact-o)
	     (,tree :start (make-fact/v ,value-s ,value-p ,value-o))
	   (unless (and ,@(unless var-s `((equal ,value-s ,fact-s)))
			,@(unless var-p `((equal ,value-p ,fact-p)))
			,@(unless var-o `((equal ,value-o ,fact-o))))
	     (return-from ,block-name (values)))
	   ,@body)))))

(defun with/iter (spec binding-vars body)
  (destructuring-bind (s p o) spec
    (let ((var-s (when (binding-p s) (cdr (assoc s binding-vars))))
	  (var-p (when (binding-p p) (cdr (assoc p binding-vars))))
	  (var-o (when (binding-p o) (cdr (assoc o binding-vars)))))
      (cond ((and var-s var-p var-o) (with/0 var-s var-p var-o body))
	    ((nor var-s var-p var-o) (with/3 s p o body))
	    (t (with/1-2 s p o var-s var-p var-o
			 (cond ((and (null var-s) var-o) 'db-spo-tree)
			       ((null var-p)             'db-pos-tree)
			       (t                        'db-osp-tree))
			 body))))))

(defmacro with/rec ((spec &rest more-specs) &body body)
  (let* ((bindings (collect-bindings spec))
	 (binding-vars (gensym-bindings bindings))
	 (body-subst (sublis binding-vars body)))
    (with/iter spec binding-vars
	       (if more-specs
		   `((with/rec ,(sublis binding-vars more-specs)
		       ,@body-subst))
		   body-subst))))

(defmacro with (bindings-spec &body body)
  `(block nil
     (with/rec ,bindings-spec
       ,@body)))

;;  WITH sugar, please

(defmacro bound-p (bindings-spec)
  `(with ,bindings-spec
     (return t)))

(defmacro collect (binding-spec &body body)
  (let ((g!collect (gensym "COLLECT-")))
    `(let ((,g!collect ()))
       (with ,binding-spec
	 (push (progn ,@body) ,g!collect))
       ,g!collect)))

(defmacro first-bound (bindings-spec)
  (let ((binding (car (collect-bindings bindings-spec))))
    (assert binding ()
	    "Invalid BINDING-SPEC: ~S
You should provide exactly one unbound variable."
	    bindings-spec)
    `(with ,bindings-spec
       (return ,binding))))

(defmacro let-with (let-spec &body body)
  `(let* (,@(mapcar
	     (lambda (b)
	       (if (third b)
		   `(,(first b) (or (first-bound ,(second b)) ,(third b)))
		   `(,(first b) (first-bound ,(second b)))))
	     let-spec))
     ,@body))

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
