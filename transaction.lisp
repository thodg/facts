;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  Transactions

(defvar *transaction* nil)
(defvar *db-path* nil)
(defvar *db-path-defaults* (make-pathname :type "facts"))
(defvar *db-log-path-defaults* (make-pathname :type "facts-log"))
(defvar *transaction-vars* nil)
(defvar *transaction-mutex* (sb-thread:make-mutex :name "transaction-mutex"))

(defun transaction-var (value name)
  (let ((cell (rassoc name *transaction-vars* :test #'eq)))
    (if cell
        (setf (car cell) value)
        (push (cons value name) *transaction-vars*))))

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

(defun db-path ()
  (and *db-path*
       (merge-pathnames *db-path* *db-path-defaults*)))

(defun db-log-path ()
  (and *db-path*
       (merge-pathnames *db-path* *db-log-path-defaults*)))

(defun commit-transaction (tx)
  (let ((path (db-log-path)))
    (when path
      (ensure-directories-exist path)
      (with-open-file (out path
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
        (dolist (operation (reverse (transaction-log tx)))
          (write (sublis (transaction-vars) operation)
                 :stream out
                 :readably t
                 :pretty nil)
          (fresh-line out)))))
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
