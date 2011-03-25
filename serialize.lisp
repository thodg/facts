;;
;;  lowh-facts  -  facts database
;;
;;  Copyright 2011 Thomas de Grivel <billitch@gmail.com>
;;
;;  All rights reserved
;;

(in-package :lowh-facts)

(defun save-db (&optional dest)
  (etypecase dest
    ((or string pathname) (with-open-file (stream dest
						  :direction :output
						  :if-exists :supersede
						  :if-does-not-exist :create)
			    (save-db stream)))
    (null (with-output-to-string (stream) (save-db stream)))
    (stream (let ((*print-readably* t))
	      (format dest "(~%")
	      (with ((?s ?p ?o))
		(let ((*print-case* :downcase))
		  (format dest " (~S ~S ~S)~%" ?s ?p ?o))))
	    (format dest ")")
	    (force-output dest))))

(defun load-db (src &optional (clear t))
  (when clear
    (facts:clear-db))

  (etypecase src
    (string (with-input-from-string (stream src) (load-db stream)))
    (pathname (with-open-file (stream src)
		(load-db stream)))
    (stream (load-db (read src)))
    (list (mapcar (lambda (fact)
		    (apply #'db-insert fact))
		  src))))
