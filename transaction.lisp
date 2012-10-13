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

(defstruct transaction
  (completed nil :type (member nil t))
  (log () :type list))

(defmacro log-transaction-operation (op &rest args)
  (unless (rollback-function op)
    (error "Undefined rollback function for ~S" op))
  `(when *transaction*
     (push (list ',op ,@args)
	   (transaction-log *transaction*))))

(defun commit-transaction (tx)
  ;; FIXME: log transaction to disk ?
  (setf (transaction-completed tx) t))

(defun rollback-transaction (tx)
  (dolist (operation (transaction-log tx))
    (destructuring-bind (op &rest args) operation
      (rollback op args))))

(defmacro with-transaction (&body body)
  `(if *transaction*
       (progn ,@body)
       (let ((*transaction* (make-transaction)))
	 (unwind-protect (prog1 (progn ,@body)
			   (transaction-commit *transaction*))
	   (unless (transaction-completed *transaction*)
	     (rollback-transaction *transaction*))))))
