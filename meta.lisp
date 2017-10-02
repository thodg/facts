;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  Inspect database

(defun about (x)
  (let ((i (the fixnum 0)))
    (with ((x ?p ?o))
      (when (= 0 i)
        (incf i)
        (format t "~&(~S" x))
      (format t "~%  ~S ~S" ?p ?o))
    (unless (= 0 i)
      (format t ")~%")))
  (with ((?s x ?o))
    (format t "(~S ~A ~S)~%" ?s x ?o))
  (with ((?s ?p x))
    (format t "(~S ~S ~A)~%" ?s ?p x)))
