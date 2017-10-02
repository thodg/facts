;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  Anonymous values

(defpackage :facts.anon
  (:nicknames :lowh-facts.anon))

(defun anon (&rest name-hints)
  (let* ((name (string-upcase (format nil "~{~A~^-~}" name-hints)))
         (sym (intern name :facts.anon)))
    (labels ((try (count)
               (multiple-value-bind (s found) (intern (format nil "~A-~4,'0X"
                                                              name count)
                                                      :facts.anon)
                 (if found
                     (try (1+ count))
                     (prog1 s
                       (setf (get sym 'anon-counter) count))))))
      (try (or (get sym 'anon-counter) 0)))))

(defmacro with-anon ((&rest vars) &body body)
  `(let ,(mapcar (lambda (var)
                   `(,var (anon ,(symbol-name var))))
                 vars)
     ,@body))
