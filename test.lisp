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

(add ('result 'source ?s)
     (?s 'id "plop")
     (?s 'score 42))

(trace replace-bindings collect-bindings% with%)
(trace make-fact/v fact/v-subject fact/v-predicate fact/v-object)

(with ((?s ?p ?o))
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

(llrbtree:map-tree (lambda (key value)
                     (format t "~&~S -> ~S~%" key value))
                   (db-pos-tree *db*)
                   :START (MAKE-FACT/V NIL nil NIL))

(macroexpand-1
 (third
  (macroexpand-1
   '(with (('result 'source ?p))
     (format t "~S~&" ?p)))))

(with (('result 'source ?p)
       (?p 'id ?id))
  (return (list ?p ?id)))

(with (('result 'source ?p)
       (?p 'id ?id)
       (?p 'score ?score))
  (format t "~S~%" (list (list ?p 'score ?score)
                         (list ?p 'id ?id))))
