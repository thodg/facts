;;
;;  facts - in-memory graph database
;;  Thomas de Grivel (+33.614550127)
;;

(in-package :facts)

;;  Fact

(deftype fact/l () '(cons t (cons t (cons t null))))

(deftype fact/v () '(simple-vector 3))

(defun make-fact/v (spec-or-subject &optional predicate (object nil object-p))
  (the fact/v
    (if object-p
        (make-array 3 :initial-contents (list spec-or-subject predicate object))
        (make-array 3 :initial-contents (the fact/l spec-or-subject)))))

(defun fact/v-subject (f)
  (svref f 0))

(defun fact/v-predicate (f)
  (svref f 1))

(defun fact/v-object (f)
  (svref f 2))

(deftype fact () '(or fact/v fact/l))

(defun fact-subject (f)
  (if (consp f)
      (first f)
      (svref f 0)))

(defun fact-predicate (f)
  (declare (type fact f))
  (if (consp f)
      (second f)
      (svref f 1)))

(defun fact-object (f)
  (declare (type fact f))
  (if (consp f)
      (third f)
      (svref f 2)))

(defun fact-equal (a b)
  (and (equal (fact-subject   a) (fact-subject b))
       (equal (fact-predicate a) (fact-predicate b))
       (equal (fact-object    a) (fact-object b))))
