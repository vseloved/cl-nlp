;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


(defun argmax (fn vals &key (test #'>) key)
  "Return the val from VALS which is the argument for maximum value of FN
   under TEST. If KEY is provided it's applied to VAL before feeding it to FN."
  (let ((max 0)
        arg)
    (dolist (val vals)
      (let ((cur (or (funcall fn (if key (funcall key val) val)) 0)))
        (when (funcall test cur max)
          (setf max cur
                arg val))))
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