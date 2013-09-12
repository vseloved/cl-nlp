;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defun entropy (samples &optional total)
  "Compute Shannon's entropy of SAMPLES list.
   To save on calculation a pre-calculated TOTAL can be provided."
  (unless total
    (setf total (reduce #'+ samples)))
  (reduce #'+ (mapcar #`(if (zerop %)
                            0
                            (let ((r (/ % total)))
                              (* r (log r 2))))
                      samples)))

(defun log-likelihood-ratio (ab a~b ~ab ~a~b)
  "Calculate log-likelihood ratio between event A and B given
   probabilites of A and B occurring together and separately."
  (let ((total (+ ab a~b ~ab ~a~b)))
    (* 2 total (- (entropy (list ab a~b ~ab ~a~b) total)
                  (entropy (list (+ ab a~b) (+ ~ab ~a~b)) total)
                  (entropy (list (+ ab ~ab) (+ a~b ~a~b)) total)))))

;; (defun ovis (words)
;;   "Swedish"
;;   (/ (log (length words))
;;      (log (- 2 (/ (log (length (uniqs words)))
;;                   (log (length words)))))))
