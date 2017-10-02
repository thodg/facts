;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
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
