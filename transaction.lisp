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

;;  Transactions

(defvar *transaction* nil)
(defvar *db-path* nil)
(defvar *transaction-vars* nil)
(defvar *transaction-mutex* (sb-thread:make-mutex :name "transaction-mutex"))

(defun transaction-var (value name)
  (pushnew (cons value name) *transaction-vars* :key #'car :test #'eq))

(defun transaction-vars ()
  *transaction-vars*)

(defstruct transaction
  (completed nil :type (member nil t))
  (log () :type list))

(defmacro log-transaction-operation (op &rest args)
  (unless (rollback-function op)
    (warn "Undefined rollback function for ~S" op))
  `(when *transaction*
     (push (list ',op ,@args)
	   (transaction-log *transaction*))))

(defun commit-transaction (tx)
  (when *db-path*
    (ensure-directories-exist *db-path*)
    (with-open-file (out (make-pathname :name "facts-log" :type "lisp"
					:defaults *db-path*)
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (dolist (operation (reverse (transaction-log tx)))
	(write (sublis (transaction-vars) operation)
	       :stream out :readably t)
	(fresh-line out))))
  (setf (transaction-completed tx) t))

(defun rollback-transaction (tx)
  (dolist (operation (transaction-log tx))
    (apply #'rollback operation)))

(defmacro with-mutex ((mutex timeout) &body body)
  (let ((g!mutex (gensym "MUTEX-"))
	(g!result (gensym "RESULT-")))
    `(let ((,g!mutex ,mutex)
	   ,g!result)
       (if (sb-thread:with-mutex (,g!mutex :wait-p t)
	     (setf ,g!result (progn ,@body))
	     t)
	   ,g!result
	   (error "Could not acquire ~S for ~D seconds."
		  ,g!mutex ,timeout)))))

(defmacro with-transaction (&body body)
  `(if *transaction*
       (progn ,@body)
       (with-mutex (*transaction-mutex* 1)
	 (let ((*transaction* (make-transaction)))
	   (unwind-protect (prog1 (progn ,@body)
			     (commit-transaction *transaction*))
	     (unless (transaction-completed *transaction*)
	       (rollback-transaction *transaction*)))))))
