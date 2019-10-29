;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :common-lisp-user)

(defpackage :facts
  (:use :cl :lessp :local-time :rollback)
  (:export #:anon #:with-anon
           #:with #:bound-p #:collect #:first-bound #:let-with
           #:without
           #:add #:add* #:rm
           #:db #:*db* #:*db-path* #:*db-log-path-defaults*
           #:clear-db #:db-path #:db-log-path #:save-db #:load-db
           #:transaction-var #:with-transaction
           #:binding-p #:collect-bindings #:sort-bindings))
