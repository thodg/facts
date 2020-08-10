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
