;;
;;  lowh-facts  -  facts database
;;
;;  Copyright 2011 Thomas de Grivel <billitch@gmail.com>
;;
;;  All rights reserved
;;

(in-package :lowh-facts)

(defmethod print-object ((x simple-base-string) s)
  (format s "#.(coerce ~S 'simple-base-string)" (coerce x 'simple-string)))

(defun save-db (&key into (readably t))
  (etypecase into
    ((or string pathname) (with-open-file (stream into
						  :direction :output
						  :if-exists :supersede
						  :if-does-not-exist :create
						  :element-type 'character
						  :external-format :utf-8)
			    (save-db :into stream :readably readably)))
    (null (with-output-to-string (stream) (save-db :into stream :readably readably)))
    (stream (let ((*print-readably* readably))
	      (format into "(~%")
	      (with ((?s ?p ?o))
		(let ((*print-case* :downcase))
		  (format into " (~S ~S ~S)~%" ?s ?p ?o))))
	    (format into ")~%")
	    (force-output into))))

(defun load-db (src)
  (etypecase src
    (string (with-input-from-string (stream src) (load-db stream)))
    (pathname (with-open-file (stream src
				      :element-type 'character
				      :external-format :utf-8)
		(load-db stream)))
    (stream (load-db (read src)))
    (list (mapcar (lambda (fact)
		    (apply #'db-insert fact))
		  src))))
