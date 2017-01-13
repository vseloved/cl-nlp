;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter +inf most-positive-fixnum)

(defun argmax (fn vals
                  &key (test #'>) (key #'identity) (min (- most-positive-fixnum)))
  "Return the val from VALS which is the argument for maximum value of FN under TEST.
   If KEY is provided it's applied to VAL before feeding it to FN.
   Also, MIN can be provided to correspond with the TEST fucntion (default: 0).
   Also return the value of FN at VAL as a second value."
  (let ((max min)
        arg)
    (dolist (v vals)
      (let ((cur (or (funcall fn (funcall key v))
                     min)))
        (when (funcall test cur max)
          (setf max cur
                arg v))))
    (values arg
            max)))

(defun keymax (ht
               &key (test #'>) (key #'identity) (min (- most-positive-fixnum)))
  "Return the key corresponding to the maximum of hash-table HT under TEST.
   If KEY is provided it's applied to VAL before feeding it to FN.
   Also, MIN can be provided to correspond with the TEST fucntion (default: 0).
   Also return the value at this key as a second value."
  (let ((max min)
        arg)
    (dotable (k v ht)
      (let ((cur (or (funcall key v)
                     min)))
        (when (funcall test cur max)
          (:= max cur
              arg k))))
    (values arg
            max)))

(defun bin-search (val vec test-less &key (start 0) end key test)
  "Binary search for VAL in sorted vector VEC (the order property isn't checked).
   Needs to specify TEST-LESS predicate. Handles START, END, KEY as usual.
   It TEST is provided tests the value at the found position against VAL,
   and returns nil if it returns nil."
  (let ((low start)
        (high (or end (1- (length vec)))))
    (do ()
        ((= low high) (when (or (null test)
                                (funcall test val (svref vec high)))
                        (elt vec high)))
      (let ((mid (floor (+ low high) 2)))
        (if (funcall test-less (if key
                                   (funcall key (svref vec mid))
                                   (svref vec mid))
                     val)
            (setf low (1+ mid))
            (setf high mid))))))

(defun dot (v &rest vs)
  "Dot product of vector V and other vectors in VS."
  (assert (reduce '= (mapcar 'length (cons v vs))))
  (let ((rez (make-array (length v))))
    (dotimes (i (length v))
      (:= (? rez i) (reduce '+ (mapcar #`(? % i)
                                       (cons v vs)))))))

(defun sum (fn xs &optional ys)
  "Sum result of application of FN to each of XS (and optionally YS)."
  (reduce '+ (if ys
                 (map 'list fn xs ys)
                 (map 'list fn xs))
          :initial-value 0))

(defun frobenius-norm (m)
  "Frobenius norm of matrix M."
  (let ((rez 0))
    (dotimes (i (array-dimension m 0))
      (dotimes (j (array-dimension m 1))
        (:+ rez (expt (aref m i j) 2))))))

(defun ~= (x y &key (epsilon 0.01))
  "Approximately equality between X & Y to the margin EPSILON."
  (< (abs (- x y)) epsilon))
