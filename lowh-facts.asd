;;
;;  lowh-facts  -  facts database
;;
;;  Copyright 2011 Thomas de Grivel <billitch@gmail.com>
;;
;;  All rights reserved

(defpackage :lowh-facts.system
  (:use :cl :asdf))

(in-package :lowh-facts.system)

(defsystem :lowh-facts
  :name "lowh-fact"
  :author "Thomas de Grivel <billitch@gmail.com>"
  :version "0.1"
  :description "facts database"
  :components
  ((:file "facts")))
