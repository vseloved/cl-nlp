;;; (c) 2013 Vsevolod Dyomkin

(cl:in-package #:cnlang)
(named-readtables:in-readtable rutils-readtable)


(defvar *v* nil
  "Feature weights table.")

(defvar *words* #{equal}
  "Known words table.")

(defun score (word ngram)
  (+ (let* ((tag (last1 ngram))
            (len (length word)))
       (or (get# (fmt "TAG:~A:~A" word tag) *v*)
           (when (> len 2)
             (get# (fmt "SUF:~A:~A" (substr word -3) tag) *v*))
           (when (> len 1)
             (get# (fmt "SUF:~A:~A" (substr word -2) tag) *v*))
           (get# (fmt "SUF:~A:~A" (substr word -1) tag) *v*)
           0))
     (get# (fmt "TRIGRAM~{:~A~}" (substitute "*" nil ngram)) *v* 0)))

(defun read-weights (file)
  (let ((rez #{equal}))
    (dolines (line file)
      (ds-bind (k v) (split #\Space line)
        (set# k rez (read-from-string v))
        (when (starts-with "TAG" line)
          (set# (second (split #\: line)) *words* t))))
    (setf *v* rez)))

(defclass glm ()
  ((order :initarg :order :initform 2 :reader model-order)
   (tags :initarg :tags :reader model-tags)
   (v :initarg :v :reader model-v))
  (:documentation
   "Hmm that uses the viterbi algorithm for inference."))

(defun make-glm ()
  (make 'glm :order 3 :v *v*
        :tags (list :I-GENE :O)))



(defmethod tag ((model glm) (sentence list))
  "Tag the tokenized SENTENCE using a Viterbi algorithm for GLMs."
  (with-slots (order tags v) model
    (let* ((len (length sentence))
           (all-tags (list* nil +stop-tag+ tags))
           (matrix-dims (make-list (1- order)
                                   :initial-element (+ (length tags) 2)))
           (min most-negative-single-float)
           (pi0 (nparse::make-pi-matrix matrix-dims 0.0))
           (pi1 (nparse::make-pi-matrix matrix-dims))
           (bps (make-array (cons (1+ len) matrix-dims) :initial-element nil)))
      (labels ((make-ngrams (step tag)
                 (let ((ngrams0 (list (list tag)))
                       (ngrams1 ())
                       (steps-left step)
                       (rez (make-hash-table :test 'equal)))
                   (loop :repeat (1- order) :do
                     (if (plusp steps-left)
                         (dolist (cur tags)
                           (dolist (ngram ngrams0)
                             (push (cons cur ngram) ngrams1)))
                         (dolist (ngram ngrams0)
                           (push (cons nil ngram) ngrams1)))
                     (setf ngrams0 ngrams1
                           ngrams1 ()
                           steps-left (1- steps-left)))
                   (dolist (ngram ngrams0)
                     (set# (rest ngram) rez
                           (cons ngram (get# (rest ngram) rez))))
                   rez))
               (viterbi-step (step word tag)
                 (let ((max min)
                       argmax)
                   (dotable (suffix ngrams (make-ngrams step tag))
                     (let ((key (nparse::tags->idx suffix all-tags))
                           (cur-max min)
                           cur-argmax)
                       (dolist (ngram ngrams)
                         (let* ((idxs (nparse::tags->idx ngram all-tags))
                                (p (+ (apply #'aref pi0 (butlast idxs))
                                      (score word ngram))))
                           (when (>= p cur-max)
                             (setf cur-max p
                                   cur-argmax idxs))))
                       (setf (apply #'aref pi1 key) cur-max
                             (apply #'aref bps (cons step key)) (car cur-argmax))
                       (when (>= cur-max max)
                         (setf max cur-max
                               argmax cur-argmax))))
                   (values (butlast argmax)
                           max))))
        ;; running the oprimization algorithm
        (doindex (i w sentence)
          (dolist (tag tags)
            (viterbi-step i w tag))
          (setf pi0 pi1
                pi1 (nparse::make-pi-matrix matrix-dims)))
        ;; recreating tags
        (mv-bind (path weight) (viterbi-step len "_____" +stop-tag+)
          (when path
            (do ((i (- len (1- (length path))) (1- i)))
                ((< i (1- order)))
              (push (apply #'aref bps (cons i (sub path 0 (1- order))))
                    path))
            (values (mapcar #`(elt all-tags %) path)
                    weight)))))))

(defun p1 (infile outfile)
  (with-out-file (out outfile)
    (let (words
          end)
      (dolines (line infile)
        (unless (setf end (blankp line))
          (push line words))
        (when (and end words)
          (loop :for word :in (reverse words)
                :and tag :in (tag (make-glm) (reverse words))
             :do (format out "~A ~A~%" word tag))
          (terpri out)
          (setf words nil))))))


(defun perceptron (infile steps)
  (let* (glm
         words
         tags
         end)
    (loop :repeat steps :do
       (setf glm (make-glm))
       (dolines (line infile)
         (unless (setf end (blankp line))
           (ds-bind (word tag) (split #\Space line)
             (push word words)
             (push tag tags)))
         (when (and end words)
           (let ((gold-tags (reverse tags))
                 (tags (tag glm (reverse words))))
             (loop :for i :from 0
                   :and word :in (reverse words)
                   :and tag :in tags
                :do (let ((gold-tag (nth i gold-tags)))
                      (unless (string= tag gold-tag)
                        (let* ((last-idx (1- (length words)))
                               (gold-prev (if (plusp i)
                                              (nth (1- i) gold-tags)
                                              "*"))
                               (cur-prev (if (plusp i)
                                             (nth (1- i) tags)
                                             "*"))
                               (gold-prevprev (if (plusp (1- i))
                                                  (nth (- i 2) gold-tags)
                                                  "*"))
                               (cur-prevprev (if (plusp (1- i))
                                                 (nth (- i 2) tags)
                                                 "*"))
                               (len (length word)))
                          (incf (get# (fmt "TAG:~A:~A" word gold-tag) *v* 0))
                          (decf (get# (fmt "TAG:~A:~A" word tag) *v* 0))
                          (dotimes (i 3)
                            (unless (>= i len)
                              (let ((suf (substr word (1- (- i)))))
                                (incf (get# (fmt "SUF:~A:~A" suf gold-tag) *v* 0))
                                (decf (get# (fmt "SUF:~A:~A" suf tag) *v* 0)))))
                          (incf (get# (fmt "TRIGRAM:~A:~A:~A"
                                           gold-prevprev gold-prev gold-tag)
                                      *v* 0))
                          (decf (get# (fmt "TRIGRAM:~A:~A:~A"
                                           cur-prevprev cur-prev tag)
                                      *v* 0))
                          (incf (get# (fmt "TRIGRAM:~A:~A:~A"
                                           gold-prev gold-tag
                                           (if (< i last-idx)
                                               (nth (1+ i) gold-tags)
                                               "STOP"))
                                      *v* 0))
                          (decf (get# (fmt "TRIGRAM:~A:~A:~A"
                                           cur-prev tag
                                           (if (< i last-idx)
                                               (nth (1+ i) tags)
                                               "STOP"))
                                      *v* 0))
                          (when (< i last-idx)
                            (incf (get# (fmt "TRIGRAM:~A:~A:~A"
                                             gold-tag
                                             (nth (1+ i) gold-tags)
                                             (if (< i (1- last-idx))
                                                 (nth (+ i 2) gold-tags)
                                                 "STOP"))
                                        *v* 0))
                            (decf (get# (fmt "TRIGRAM:~A:~A:~A"
                                             tag
                                             (nth (1+ i) tags)
                                             (if (< i (1- last-idx))
                                                 (nth (+ i 2) tags)
                                                  "STOP"))
                                        *v* 0))))))))
           (setf words nil
                 tags nil)))
       (print (remove-if-not #`(starts-with "TRIGRAM" %) (ht->alist *v*)
                             :key 'car))))
  *v*)