;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(defpackage :facts.system
  (:use :cl :asdf))

(in-package :facts.system)

(defsystem :facts
  :name "facts"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.2"
  :description "in-memory graph database"
  :depends-on ("lessp" "local-time" "rollback")
  :components
  ((:file "package")
   (:file "fact" :depends-on ("package"))
   (:file "spec" :depends-on ("package"))
   (:file "anon" :depends-on ("package"))
   (:file "transaction" :depends-on ("package"))
   (:file "usl" :depends-on ("fact"))
   (:file "index" :depends-on ("usl"))
   (:file "database" :depends-on ("index" "transaction"))
   (:file "with" :depends-on ("database" "spec" "anon"))
   (:file "serialize" :depends-on ("with"))
   (:file "meta" :depends-on ("with"))))
