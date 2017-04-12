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

;;  Database

(defclass db ()
  ((index-spo :initform (make-index #'fact-spo-lessp)
              :reader db-index-spo)
   (index-pos :initform (make-index #'fact-pos-lessp)
              :reader db-index-pos)
   (index-osp :initform (make-index #'fact-osp-lessp)
              :reader db-index-osp)))

(defgeneric db-fact (db fact))
(defgeneric db-indexes-insert (db fact))
(defgeneric db-indexes-delete (db fact))

(defmethod db-fact ((db db) fact)
  (index-get (db-index-spo db) fact))

;;  Database operations on indexes

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (rollback-function 'db-indexes-insert) 'db-indexes-delete)
  (setf (rollback-function 'db-indexes-delete) 'db-indexes-insert))

(defmethod db-indexes-insert ((db db) fact)
  (with-rollback*
    (index-insert (db-index-spo db) fact)
    (index-insert (db-index-pos db) fact)
    (index-insert (db-index-osp db) fact)
    (log-transaction-operation db-indexes-insert db fact))
  fact)

(defmethod db-indexes-delete ((db db) fact)
  (with-rollback*
    (index-delete (db-index-spo db) fact)
    (index-delete (db-index-pos db) fact)
    (index-delete (db-index-osp db) fact)
    (log-transaction-operation db-indexes-delete db fact))
  fact)

;;  High level database operations

(defvar *db* (make-instance 'db))

(setf *transaction-vars* nil)
(transaction-var *db* '*db*)

(defun clear-package (package)
  (let ((pkg (typecase package
	       (package package)
	       (t (find-package package)))))
    (do-symbols (sym pkg)
      (unintern sym pkg))))

(defun clear-db ()
  (setf *db* (make-instance 'db))
  (setf *transaction-vars* nil)
  (transaction-var *db* '*db*)
  (clear-package :facts.anon))

(defun db-get (s p o &optional (db *db*))
  (db-fact db (make-fact/v s p o)))

(defun db-insert (subject predicate object &optional (db *db*))
  (let ((fact (make-fact/v subject predicate object)))
    (or (db-fact db fact)
	(db-indexes-insert db fact))))

(defun db-delete (fact &optional (db *db*))
  (let ((fact (db-fact db fact)))
    (when fact
      (db-indexes-delete db fact))))

(defmacro db-each ((var-s var-p var-o) (tree &key start end) &body body)
  (let ((g!fact (gensym "FACT-"))
	(g!value (gensym "VALUE-")))
    `(index-each (,tree *db*)
		 (lambda (,g!fact)
		   (let ((,var-s (fact/v-subject   ,g!fact))
			 (,var-p (fact/v-predicate ,g!fact))
			 (,var-o (fact/v-object    ,g!fact)))
		     ,@body
		     (values)))
		 ,@(when start `(:start ,start))
		 ,@(when end   `(:end ,end)))))
