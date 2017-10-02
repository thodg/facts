;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

(defclass resource-metaclass (object)
  ())

(defclass resource (object)
  ()
  (:metaclass resource-metaclass))
