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

(pushnew :test *features*)

;;  Types

(deftype positive-fixnum (&optional (low 0))
  `(integer ,low ,most-positive-fixnum))

;;  Unlabeled skip list node

(deftype usl-node (&optional (height '*)) `(simple-vector ,height))

(defun usl-node-value (node)
  (svref (the usl-node node) 0))

(define-setf-expander usl-node-value (node &environment env)
  (get-setf-expansion `(svref (the usl-node ,node) 0) env))

(defun usl-node-link (node level)
  (svref (the usl-node node) (1+ level)))

(define-setf-expander usl-node-link (node level &environment env)
  (get-setf-expansion `(svref (the usl-node ,node) (1+ ,level)) env))

(defsetf usl-node-link (node level) (value)
  `(setf (svref (the usl-node ,node) (1+ ,level)) ,value))

(defun usl-node-height (node)
  (1- (length node)))

(defun make-usl-node (value height)
  (declare (type (positive-fixnum 1) height))
  (let ((node (the usl-node (make-array (1+ height) :initial-element nil))))
    (setf (usl-node-value node) value)
    node))

#+test
(defun test-usl-node ()
  (let ((a (make-usl-node nil 2))
        (b (make-usl-node #(1 2 3) 2))
        (c (make-usl-node #(1 2 9) 1)))
    (setf (usl-node-link a 0) b)
    (setf (usl-node-link a 1) b)
    (setf (usl-node-link b 0) c)
    (setf (usl-node-link b 1) c)
    a))

#+test
(test-usl-node)

;;  Testing

#+test
(defun test-height-repartition (fn max-height spacing)
  (declare (type (positive-fixnum 1) max-height)
           (type (positive-fixnum 2) spacing)
           (type (function (fixnum fixnum) fixnum) fn)
           (optimize (speed 3)))
  (let ((height (make-array max-height
                            :element-type 'fixnum
                            :initial-element 0))
        (rounds #1=1000000))
    (declare (type positive-fixnum rounds))
    (format t "~&~S : ~D rounds, max-height ~D, spacing ~D~%"
            fn rounds max-height spacing)
    (force-output)
    (dotimes (i rounds)
      (incf (the (integer 0 #1#)
              (aref height (1- (the positive-fixnum
                                 (funcall fn max-height spacing)))))))
    (dotimes (i max-height)
      (format t "height ~3D | p ~9,6F | 1/~D~%"
              (1+ i)
              (/ (aref height i) rounds)
              (ceiling rounds (1+ (aref height i)))))
    (terpri)
    (force-output)))

#+test
(defun cl-skip-list-random-level (max-level spacing)
  "Returns a random level for a new skip-list node, following Pugh's pattern of 
L1: 50%, L2: 25%, L3: 12.5%, ..."
  (declare (type fixnum spacing max-level)
           (optimize speed))
  (assert (= 2 spacing))
  (do ((level 1 (sb-ext:truly-the fixnum (1+ level))))
      ((or (= level max-level)
           (= (random 4) 3)) ;; 
       level)
    (declare (type fixnum level))))

#+test
(time (test-height-repartition #'cl-skip-list-random-level 8 2))

;;
;;  Random height
;;  -------------
;;
;;  ∀ U ∈ ℕ : 1 < U
;;  ∀ n ∈ ℕ*
;;  ∀ r ∈ ℕ : r ≤ n
;;  ∀ random : ℕ* ⟶ ℕ
;;             random(n) ∈ [0..n-1]
;;             ∀ i ∈ [0..n-1], P(random(n) = i) = n⁻¹               (i)
;;  Qᵣ := random(Uⁿ) < Uⁿ⁻ʳ
;;
;;  (i) ⇒        P(random(n) < v) = ∑ᵢ₌₀ᵛ⁻¹ P(random(n) = i)
;;      ⇒        P(random(n) < v) = v . n⁻¹                        (ii)
;;
;;      ⇒    P(random(Uⁿ) < Uⁿ⁻ʳ) = Uⁿ⁻ʳ . (Uⁿ)⁻¹
;;      ⇔                   P(Qᵣ) = U⁻ʳ                           (iii)
;;
;;  P(Qₙ) = P(random(Uⁿ) < U⁰)
;;        = P(random(Uⁿ) < 1)
;;        = P(random(Uⁿ) = 0)
;;        = U⁻ⁿ
;;
;;  R := maxᵣ(Qᵣ)
;;     = maxᵣ(random(Uⁿ) < Uⁿ⁻ʳ)
;;     = maxᵣ(random(Uⁿ) + 1 ≤ Uⁿ⁻ʳ)
;;     = maxᵣ(logᵤ(random(Uⁿ) + 1) ≤ n - r)
;;     = maxᵣ(⌈logᵤ(random(Uⁿ) + 1)⌉ ≤ n - r)
;;     = maxᵣ(r ≤ n - ⌈logᵤ(random(Uⁿ) + 1)⌉)
;;     = n - ⌈logᵤ(random(Uⁿ) + 1)⌉                                (iv)
;;
;;                       0 ≤ random(Uⁿ) < Uⁿ
;;   ⇔                   1 ≤ random(Uⁿ)+1 ≤ Uⁿ
;;   ⇔        logᵤ(1) ≤ logᵤ(random(Uⁿ)+1) ≤ logᵤ(Uⁿ)
;;   ⇔             0 ≤ ⌈logᵤ(random(Uⁿ)+1)⌉ ≤ n
;;   ⇔           -n ≤ -⌈logᵤ(random(Uⁿ)+1)⌉ ≤ 0
;;   ⇔         0 ≤ n - ⌈logᵤ(random(Uⁿ)+1)⌉ ≤ n
;;   ⇔                      0 ≤ R ≤ n                               (v)
;;

#+test
(defun usl-random-height* (max-height spacing)
  (declare (type (positive-fixnum 1) max-height)
           (type (positive-fixnum 2) spacing)
           (optimize (speed 3)))
  (let ((u spacing)
        (n max-height))
    (the positive-fixnum
      (1+ (mod (- n (ceiling (log (1+ (random (the positive-fixnum (expt u n))))
                                  u)))
               n)))))

#+test
(time (test-height-repartition #'usl-random-height* 8 2))
(time (test-height-repartition #'usl-random-height* 8 3))

(defun usl-random-height (max-height spacing)
  (declare (type (positive-fixnum 1) max-height)
           (type (positive-fixnum 2) spacing)
           (optimize (speed 3)))
  (let* ((u spacing)
         (k (the positive-fixnum (random (expt u max-height)))))
    (do ((uʳ 1 (sb-ext:truly-the positive-fixnum (* u uʳ)))
         (r 0 (1+ r)))
        ((< k uʳ)
         (the fixnum (1+ (mod (- max-height r) max-height))))
      (declare (type positive-fixnum uʳ r)))))

#+test
(time (test-height-repartition #'usl-random-height 8 2))
(time (test-height-repartition #'usl-random-height 8 3))

;;  Skip list

(defstruct usl
  (spacing 3 :type (positive-fixnum 2))
  (lessp #'lessp:lessp :type (function (t t) (or t nil)))
  (head (make-usl-node nil 1) :type usl-node)
  (length 0 :type positive-fixnum))

(defun usl-height (usl)
  (usl-node-height (usl-head usl)))

;;  Find

(defun usl-find (usl value)
  "Return two values : the predecessor node for VALUE in USL,
and the stored value if VALUE was found."
  (declare (type usl usl))
  (with-slots (lessp head) (the usl usl)
    (labels ((usl-find/node (node)
               (declare (type usl-node node))
               ;; We have (lessp node value) => t
               (let ((next (usl-node-link node 0)))
                 (if next
                     (let ((next-value (usl-node-value next)))
                       (if (funcall lessp next-value value)
                           (usl-find/node next)
                           (if (funcall lessp value next-value)
                               (values node nil)
                               (values node next-value))))
                     (values node nil)))))
      (usl-find/node head))))

;;  Insert

(defun usl-node-insert (node value)
  ;;  FIXME: level
  (let ((new (make-usl-node value 1)))
    (setf (usl-node-link new 0) (usl-node-link node 0)
          (usl-node-link node 0) new)
    new))

(defun usl-insert (usl value)
  ;;  FIXME: level
  (multiple-value-bind (node found) (usl-find usl value)
    (if found
        (usl-node-value node)
        (progn
          (incf (usl-length usl))
          (usl-node-value (usl-node-insert node value))))))

#+test
(defun test-usl-insert ()
  (let ((usl (make-usl)))
    (usl-insert usl #(1 1 1))
    (usl-insert usl #(2 2 2))
    (usl-insert usl #(0 0 0))
    (usl-insert usl #(1 2 2))
    (usl-insert usl #(0 0 1))
    (usl-insert usl #(3 3 3))
    usl))

#+test
(let ((*print-circle* t))
  (test-usl-insert))

;;  Get

(defun usl-get (usl value)
  (nth-value 1 (usl-find usl value)))

#+test
(let ((usl (test-usl-insert)))
  (mapcar (lambda (x)
            (format t "~&Get ~S -> ~S" x (usl-get usl x)))
          (list 1 #(0 0 0) #(1 2 2) #(0 0 1) #(1 0 0) #(3 3 3) #(2 2 2))))

;;  Each

(defun usl-each (usl fn &key start end)
  ;;  FIXME: level
  (with-slots (lessp head) usl
    (labels ((usl-each/node (node)
               (when node
                 (unless (and end
                              (funcall lessp end (usl-node-value node)))
                   (funcall fn (usl-node-value node))
                   (usl-each/node (usl-node-link node 0))))))
      (usl-each/node (multiple-value-bind (node found) (usl-find usl start)
                       (if found
                           node
                           (usl-node-link node 0)))))))

#+test
(defun test-usl-each ()
  (usl-each (test-usl-insert)
            (lambda (fact)
              (format t "~&each ~S~%" fact))))

#+test
(test-usl-each)

;;  Delete

(defun usl-delete (usl value)
  (multiple-value-bind (pred found) (usl-find usl value)
    (when found
      (setf (usl-node-link pred 0) (usl-node-link (usl-node-link pred 0)
                                                  0))
      (decf (usl-length usl))
      found)))

#+test
(defun test-usl-delete ()
  (let ((usl (test-usl-insert)))
    (values
     (mapcar (lambda (x)
               (usl-delete usl x))
             (list #(1 2 2) #(0 0 0) #(1 2 2) #(3 3 3)))
     usl)))

#+test
(test-usl-delete)
