;;; (c) 2013-2017 Vsevolod Dyomkin

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

(defun log-likelihood-ratio (ab a~b ~ab ~a~b)
  "Calculate log-likelihood ratio between event A and B given
   probabilites of A and B occurring together and separately."
  (let ((total (+ ab a~b ~ab ~a~b)))
    (* 2 total (- (entropy (list ab a~b ~ab ~a~b) total)
                  (entropy (list (+ ab a~b) (+ ~ab ~a~b)) total)
                  (entropy (list (+ ab ~ab) (+ a~b ~a~b)) total)))))

(defun sample (data n &key (with-replacement? t))
  "Sample N elements from DATA (by default, WITH-REPLACEMENT?)."
  (if with-replacement?
      (with ((data (coerce data 'vector))
             (len (length data)))
        (if (>= n len)
            (progn
              (warn "Sample size exceeds data length: ~A > ~A" n (length data))
              (coerce data 'list))
            (loop :repeat n :collect (? data (random len)))))
      (take n (nshuffle (copy-list data)))))

(defun normal-random (&optional (mean 0.0) (std-dev 1.0))
  "generate a normal random number with the given MEAN and STD-DEV."
  (do* ((rand-u (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-v (* 2 (- 0.5 (random 1.0))) (* 2 (- 0.5 (random 1.0))))
        (rand-s (+ (* rand-u rand-u) (* rand-v rand-v))
                (+ (* rand-u rand-u) (* rand-v rand-v))))
       ((not (or (= 0 rand-s) (>= rand-s 1)))
        (+ mean
           (* std-dev
              (* rand-u (sqrt (/ (* -2.0 (log rand-s)) rand-s))))))))
