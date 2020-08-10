;;
;;  facts - in-memory graph database
;;  Copyright 2011,2012,2014,2015,2017-2020 Thomas de Grivel <thoxdg@gmail.com>
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
      (guess (or (get sym 'anon-counter) 0)))))

(defmacro with-anon ((&rest vars) &body body)
  `(let ,(mapcar (lambda (var)
                   `(,var (anon ,(symbol-name var))))
                 vars)
     ,@body))
