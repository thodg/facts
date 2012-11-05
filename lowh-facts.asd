;;
;;  lowh-facts  -  facts database
;;
;;  Copyright 2011,2012 Thomas de Grivel <billitch@gmail.com>
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

(defpackage :lowh-facts.system
  (:use :cl :asdf))

(in-package :lowh-facts.system)

(defsystem :lowh-facts
  :name "lowh-fact"
  :author "Thomas de Grivel <billitch@gmail.com>"
  :version "0.2"
  :description "facts database"
  :depends-on ("lessp" "rollback")
  :components
  ((:file "package")
   (:file "fact" :depends-on ("package"))
   (:file "spec" :depends-on ("package"))
   (:file "transaction" :depends-on ("package"))
   (:file "usl" :depends-on ("fact"))
   (:file "index" :depends-on ("usl"))
   (:file "database" :depends-on ("index" "transaction"))
   (:file "with" :depends-on ("database" "spec"))
   (:file "serialize" :depends-on ("with"))))
