;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(declaim (inline log2))
(defun log2 (x)
  "Base 2 logarithm."
  (/ (log x) (log 2)))

(defun entropy (samples &optional total)
  "Compute Shannon's entropy of SAMPLES list.
   To save on calculation a pre-calculated TOTAL can be provided."
  (unless total
    (setf total (reduce #'+ samples)))
  (reduce #'+ (mapcar #`(if (zerop %)
                            0
                            (let ((r (/ % total)))
                              (* r (log2 r))))
                      samples)))

(defun log-likelihood-ratio (ab a~b ~ab ~a~b)
  "Calculate log-likelihood ratio between event A and B given
   probabilites of A and B occurring together and separately."
  (let ((total (+ ab a~b ~ab ~a~b)))
    (* 2 total (- (entropy (list ab a~b ~ab ~a~b) total)
                  (entropy (list (+ ab a~b) (+ ~ab ~a~b)) total)
                  (entropy (list (+ ab ~ab) (+ a~b ~a~b)) total)))))