
(defclass resource-metaclass (object)
  ())

(defclass resource (object)
  ()
  (:metaclass resource-metaclass))
