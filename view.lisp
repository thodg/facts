;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  View

(defclass view ()
  ((refresh :initarg refresh
            :reader view-refresh
            :type function)
   (index :initform (make-usl :lessp lessp)
          :reader view-index
          :type usl)))

(defmacro view-refresher ((view) &body body)
  `(lambda (,view)
     ,@body))

(defun refresh-view (view)
  (funcall (view-refresh view) view))

(defmacro defview (name &body body)
  `(progn
     (defvar ,name)
     (setq ,name (make-instance 'view
                                :refresh (view-refresher
                                          (,name) ,@body)))
     (refresh-view ,name)
     ',name))
