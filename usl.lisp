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

(in-package :lowh-facts)

;;
;;  Unlabeled Skip Lists
;;  --------------------

#+nil
(pushnew :test *features*)

(declaim (optimize (debug 3) (speed 0)))

;;  Types

(deftype positive-fixnum (&optional (low 0))
  `(integer ,low ,most-positive-fixnum))

(deftype fixnum* (&optional (start '*) (end '*))
  (let ((start (if (eq '* start) most-negative-fixnum start))
	(end (if (eq '* end) most-positive-fixnum end)))
    `(integer ,start ,end)))

(deftype fixnum-mult (mult &optional (start '*))
  (declare (type (fixnum* 1) mult))
  (let ((start (if (eq '* start)
		   (truncate most-negative-fixnum mult)
		   start)))
    `(integer ,start
	      ,(truncate most-positive-fixnum mult))))

(deftype fixnum-add (&optional (add '*) (start '*))
  (let ((end (if (eq '* add)
		 (truncate most-positive-fixnum 2)
		 (- most-positive-fixnum (the positive-fixnum add)))))
    `(fixnum* ,start ,end)))

(deftype fixnum-float (&optional (start '*) (end '*))
  (let ((start (if (eq '* start) most-negative-fixnum start))
	(end (if (eq '* end) most-positive-fixnum end)))
    (declare (optimize (speed 1)))
    `(long-float ,(coerce start 'long-float)
		 ,(coerce end 'long-float))))

;;  Unlabeled skip list node

(defstruct (usl-node (:constructor make-usl-node%))
  (value nil)
  (links nil :type simple-vector))

(defun make-usl-node (value height)
  (declare (type (fixnum* 1) height))
  (make-usl-node% :value value
		  :links (make-array height
				     :element-type '(or usl-node null)
				     :initial-element nil)))

(defun usl-node-next (node &optional (level 0))
  (declare (type usl-node node)
	   (type (fixnum* 0) level))
  (svref (usl-node-links node) level))

(define-setf-expander usl-node-next (node level &environment env)
  (get-setf-expansion `(svref (usl-node-links (the usl-node ,node))
			      (the (fixnum* 0) ,level))
		      env))

(defun usl-node-height (node)
  (declare (type usl-node node))
  (the (fixnum* 1) (length (usl-node-links node))))

#+test
(defun test-usl-node ()
  (let ((a (make-usl-node nil 2))
	(b (make-usl-node #(1 2 3) 2))
	(c (make-usl-node #(1 2 9) 1)))
    (setf (usl-node-next a 0) b)
    (setf (usl-node-next a 1) b)
    (setf (usl-node-next b 0) c)
    (setf (usl-node-next b 1) c)
    a))

#+test
(test-usl-node)

;;  Testing

#+test
(defun test-height-repartition (fun-fun max-height spacing)
  (declare (type (fixnum* 1) max-height)
	   (type (function (fixnum fixnum) function) fun-fun))
  (let ((height (make-array max-height
			    :element-type 'fixnum
			    :initial-element 0))
	(rounds #1=1000000)
	(fun (funcall fun-fun max-height spacing)))
    (declare (type positive-fixnum rounds))
    (format t "~&~S : ~D rounds, max-height ~D, spacing ~D~%"
	    fun-fun rounds max-height spacing)
    (force-output)
    (locally (declare (optimize speed))
      (time
       (dotimes (i rounds)
	 (incf (the (integer 0 #1#)
		 (aref height (1- (the positive-fixnum
				    (funcall fun)))))))))
    (dotimes (i max-height)
      (format t "~&height ~3D | p ~9,6F | 1/~D~%"
	      (1+ i)
	      (/ (aref height i) rounds)
	      (round rounds (1+ (aref height i)))))))

#+test
(defun cl-skip-list-random-level-fun (max-level spacing)
  "Returns a random level for a new skip-list node, following Pugh's pattern of 
L1: 50%, L2: 25%, L3: 12.5%, ..."
  (declare (type fixnum spacing max-level)
	   (optimize speed))
  (assert (= 2 spacing))
  (lambda ()
    (do ((level 1 (sb-ext:truly-the fixnum (1+ level))))
	((or (= level max-level)
	     (= (random 4) 3)) ;; 
	 level)
      (declare (type fixnum level)))))

#+test
(test-height-repartition #'cl-skip-list-random-level-fun 8 2)

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
(defun usl-random-height-fun* (max-height spacing)
  (let* ((u (coerce spacing '(single-float 0.0s0)))
	 (n (the (fixnum-add 1 1) max-height))
	 (max (the (fixnum-add 1 1) (ceiling (expt u n)))))
    (lambda ()
      (declare (optimize (speed 3)))
      (the positive-fixnum
	(1+ (mod (- n (the fixnum
			(ceiling
			 (log (1+ (random max))
			      u))))
		 n))))))

(defconstant +e+ 2.718281828459045235360287471352662497757l0)

#+test
(test-height-repartition #'usl-random-height-fun* 8 2)
#+test
(test-height-repartition #'usl-random-height-fun* 8 +e+)

(defun usl-height-table (max-height spacing)
  (declare (type (fixnum* 2) max-height)
	   (type (fixnum-float 1 16) spacing))
  (let ((table (make-array max-height
			   :element-type '(fixnum* 1)
			   :initial-element 1)))
    (do ((h 0 (1+ h))
	 (w spacing (* spacing (the fixnum-float w)))
	 (end 1.0l0 (+ end w))
	 (pos 0 end))
	((= max-height h))
      (setf (aref table h) (truncate (the fixnum-float end))))
    table))

(defun usl-random-height-fun (max-height spacing)
  (declare (type positive-fixnum max-height)
	   (optimize speed))
  (let* ((table (the (simple-array (fixnum* 1) (*))
		  (usl-height-table max-height spacing)))
	 (max (the positive-fixnum (1- (aref table (1- (length table)))))))
    (lambda ()
      (let ((k (random max)))
	(do ((i 0 (1+ (the fixnum-add i))))
	    ((< k (aref table i)) (- max-height i)))))))

#+test(test-height-repartition #'usl-random-height-fun 8 2)
#+test(test-height-repartition #'usl-random-height-fun 8 3)
#+test(test-height-repartition #'usl-random-height-fun 8 +e+)

(defun usl-random-height (max-height spacing)
  (declare (type (fixnum* 1) max-height)
	   (type (fixnum-float 1) spacing))
  (let ((max-height* (load-time-value #1=2))
	(spacing* (load-time-value #2=+e+))
	(fun* (load-time-value (usl-random-height-fun #1# #2#))))
    (declare (type (function () (fixnum* 1)) fun*))
    (unless (and (= max-height* max-height)
		 (= spacing* spacing))
      (setf max-height* max-height
	    spacing* spacing
	    fun* (usl-random-height-fun max-height spacing)))
    (funcall fun*)))
      

#+test
(time (test-height-repartition #'usl-random-height 8 2))
#+test
(time (test-height-repartition #'usl-random-height 8 3))

;;  Skip list

(defstruct (usl (:constructor make-usl%))
  (lessp-fun nil :type (function (t t) (or t nil)))
  (head nil :type usl-node)
  (length 0 :type (fixnum* 0))
  (spacing nil :type (fixnum-float 1))
  (height-fun nil :type (function () (fixnum* 1))))

(defun make-usl (&key (lessp #'lessp:lessp) (height 3) (spacing +e+))
  (make-usl% :lessp-fun lessp :spacing spacing
	     :head (make-usl-node nil height)
	     :height-fun (usl-random-height-fun height spacing)))

(defun usl-height (usl)
  (usl-node-height (usl-head usl)))

(defun usl-lessp (usl a b)
  (declare (type usl usl))
  (funcall (the (function (t t) t)
	     (usl-lessp-fun usl))
	   a b))

;;  Find

(defun usl-find (usl value &optional pred)
  "Return the in-tree value if found.
PRED if given must be an array, fill it with predecessor usl nodes."
  (declare (type usl usl)
	   (type (or null (simple-array t (*))) pred))
  (with-slots (head) usl
    (let ((node head))
      (do ((level (1- (the (fixnum* 1) (usl-node-height head)))
		  (1- (the (fixnum* 0) level))))
	  ((< level 0))
        (do ((n node (usl-node-next n level)))
	    ((or (null n)
		 (not (usl-lessp usl (usl-node-value n) value))))
	  (setf node n))
	(when (and pred (< level (length pred)))
	  (setf (aref pred level) node)))
      (let* ((next (usl-node-next node 0))
	     (found (when next
		      (let ((next-value (usl-node-value next)))
			(unless (usl-lessp usl value next-value)
			  value)))))
	found))))

(defun usl-cursor (usl &optional value)
  (let ((pred (make-array 1)))
    (usl-find usl value pred)
    (aref pred 0)))

#+test
(let ((usl (make-usl)))
  (usl-cursor usl nil))

;;  Insert

(defun usl-node-insert (pred new)
  (declare (type (simple-array usl-node (*)) pred))
  (dotimes (i (length pred))
    (let ((node (aref pred i)))
      (setf (usl-node-next new i) (usl-node-next node i)
	    (usl-node-next node i) new))))

(defun usl-insert (usl value)
  (with-slots (head height-fun) usl
    (declare (type (function nil (fixnum* 1)) height-fun))
    (let* ((height (funcall height-fun))
	   (pred (make-array (the (fixnum* 1) height))))
      (or (usl-find usl value pred)
	  (progn (usl-node-insert pred (make-usl-node value height))
		 (incf (usl-length usl))
		 value)))))

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
  (format t "~%~A~&" (test-usl-insert)))

;;  Get

(defun usl-get (usl value)
  (usl-find usl value))

#+test
(let ((usl (test-usl-insert)))
  (mapcar (lambda (x)
	    (format t "~&Get ~S -> ~S" x (usl-get usl x)))
	  (list 1 #(0 0 0) #(1 2 2) #(0 0 1) #(1 0 0) #(3 3 3) #(2 2 2))))

;;  Each

(defun usl-each (usl fn &key start end)
  (declare (type (function (t) t) fn))
  (do ((node (usl-node-next (usl-cursor usl start)) (usl-node-next node)))
      ((or (null node)
	   (when end
	     (usl-lessp usl end (usl-node-value node)))))
    (funcall fn (usl-node-value node))))

#+test
(defun test-usl-each ()
  (usl-each (test-usl-insert)
	    (lambda (fact)
	      (format t "~&each ~S~%" fact))))

#+test
(test-usl-each)

;;  Delete

(defun usl-delete (usl value)
  (let* ((pred (make-array (the (fixnum* 1) (usl-height usl))))
	 (found (usl-find usl value pred)))
    (when found
      (let* ((node (usl-node-next (aref pred 0)))
	     (height (the (fixnum* 1) (usl-node-height node))))
	(dotimes (i height)
	  (setf (usl-node-next (aref pred i) i)
		(usl-node-next node i))))
      (decf (usl-length usl))
      found)))

#+test
(defun test-usl-delete ()
  (let ((usl (test-usl-insert)))
    (mapcar (lambda (x)
	      (format t "~&delete ~S -> ~S~%"
		      x (usl-delete usl x)))
	    (list #(1 2 2) #(0 0 0) #(1 2 2) #(3 3 3)))
    (format t "~&~S~%" usl)))

#+test
(test-usl-delete)
