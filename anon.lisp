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
