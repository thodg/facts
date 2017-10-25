;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  Anonymous values

(defpackage :facts.anon)

(defvar *randomize-anon* nil)

(defun anon (&rest sym-hints)
  (let* ((sym (intern
               (with-output-to-string (out)
                 (write-string (string-upcase (first sym-hints)))
                 (dolist (sym-hint (rest sym-hints))
                   (write-char #\- out)
                   (write-string (string-upcase sym-hint))))
               (find-package :facts.anon))))
    (labels ((guess (count)
               (multiple-value-bind (n found) (intern
                                               (format nil "~A-~4,'0X"
                                                       sym count)
                                               :facts.anon)
                 (if found
                     (if *randomize-anon*
                         (guess (random most-positive-fixnum))
                         (guess (1+ count)))
                     (prog1 n
                       (setf (get sym 'anon-counter) count))))))
      (try (or (get sym 'anon-counter) 0)))))

(defmacro with-anon ((&rest vars) &body body)
  `(let ,(mapcar (lambda (var)
                   `(,var (anon ,(symbol-name var))))
                 vars)
     ,@body))
