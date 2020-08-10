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
