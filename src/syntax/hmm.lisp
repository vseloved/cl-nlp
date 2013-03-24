;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.syntax.hmm)
(named-readtables:in-readtable rutils-readtable)


(defclass hmm ()
  ((order :initarg :order :initform 2 :reader model-order)
   (tags :initarg :tags :reader model-tags)
   (transition-lm :initarg :transition-lm :reader hmm-transition-lm
    :documentation "A language model of tags transition probabilities.")
   (emission-lm :initarg :emission-lm :reader hmm-emission-lm
    :documentation "An ngrams model of tag-to-word conditional probabilities."))
  (:documentation
   "A Hidden Markov Model has an ORDER, a set of TAGS and 2 language models:
    - TRANSITION-LM of transition probabilities between TAGS of the model's ORDER
    - EMISSION-LM of emission probabilities of words from tags"))

(defmethod train ((model hmm) data &rest args
                  &key (transition-lm-class 'plain-lm)
                       (emission-lm-class 'plain-lm)
                  &allow-other-keys)
  "Train hmm MODEL (using language models of types TRANSITION-LM-CLASS
   and EMISSION-LM-CLASS) on a list of tagged sentences DATA."
  (declare (ignore args))
  (with-slots (order transition-lm emission-lm) model
    (let ((tfs (make-hash-table :test 'equal))
          (efs (make-hash-table :test 'equal)))
      ;; collect freqs
      (dolist (s data)
        (loop :for tail :on (append (make-list (1- order)) s) :while tail :do
           ;; collect ngrams from 1 to ORDER
           (dotimes (i order)
             (let* ((ngram (mapcar #`(if % (token-tag %) %)
                                   (take (1+ i) tail)))
                    (len (length ngram)))
               ;; possibly add stop tag
               (when (= len i)
                 (setf ngram (append ngram (list +stop-tag+))))
               ;; record transition freq
               (unless (< len i)
                 (incf (get# ngram tfs 0)))))
           ;; record emission freq
           (when-it (car tail)
             (incf (get# (list (token-tag it) (token-word it)) efs 0)))))
      (let ((ngrams (make-array (1+ order))))
        (dotimes (i order)
          (setf (elt ngrams (1+ i)) (make-hash-table :test 'equal)))
        (dotable (ngram count tfs)
          (set# ngram (elt ngrams (length ngram)) count))
        (setf emission-lm
              (make-lm emission-lm-class
                       :1g (make 'table-ngrams :order 1
                                 :table (elt ngrams 1))
                       :2g (make 'table-ngrams :order 2
                                 :table efs))
              transition-lm
              (apply #'make-lm transition-lm-class
                     (loop :for i :from 1 :to order
                        :nconc (list (mkeyw i :format "~Ag")
                                     (make 'table-ngrams :order order
                                           :table (elt ngrams i)))))))))
  model)

(defun make-hmm (tags data &key (order 2)
                                (hmm-class 'viterbi-hmm)
                                (transition-lm-class 'plain-lm)
                                (emission-lm-class 'plain-lm))
  "A simple wrapper to make hmms."
  (train (make hmm-class :order order :tags tags)
         data
         :transition-lm-class transition-lm-class
         :emission-lm-class emission-lm-class))


(defclass viterbi-hmm (hmm)
  ()
  (:documentation
   "Hmm that uses the viterbi algorithm for inference."))

(defmethod pos-tag ((model viterbi-hmm) (sentence list))
  "POS tag tokenized SENTENCE using a Viterbi algorithm for hmms."
  (with-slots (order tags (tps transition-lm) (eps emission-lm)) model
    (let* ((len (length sentence))
           (all-tags (cons nil (cons +stop-tag+ tags)))
           (matrix-dims (make-list (1- order)
                                   :initial-element (+ (length tags) 2)))
           (pi0 (make-pi-matrix matrix-dims 1.0))
           (pi1 (make-pi-matrix matrix-dims))
           (bps (make-array (cons (1+ len) matrix-dims) :initial-element nil)))
      (labels ((make-ngrams (step tag)
                 "Generate a list of all possible ngram combinations
                  ending in TAG at STEP and arrange them in a table
                  keyed by ngram suffixes (all words but first)."
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
               (viterbi-step (step tag emission-prob)
                 "Run one STEP of the Viterbi algorithm for TAG
                  with the given emission probability EMISSION-PROB."
                 (let ((max most-negative-single-float)
                       argmax)
                   (dotable (suffix ngrams (make-ngrams step tag))
                     (let ((key (tags->idx suffix all-tags))
                           (cur-max most-negative-single-float)
                           cur-argmax)
                       (dolist (ngram ngrams)
                         (let* ((idxs (tags->idx ngram all-tags))
                                (p (if (zerop emission-prob)
                                       most-negative-single-float
                                       (+ (apply #'aref pi0 (butlast idxs))
                                          (cond-logprob tps ngram)
                                          (log emission-prob 2)))))
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
            (viterbi-step i tag (cond-prob eps (list tag w))))
          (setf pi0 pi1
                pi1 (make-pi-matrix matrix-dims)))
        ;; recreating tags
        (mv-bind (path logprob) (viterbi-step len +stop-tag+ 1)
          (when path
            (do ((i (- (length sentence) (1- (length path))) (1- i)))
                ((< i (1- order)))
              (push (apply #'aref bps (cons i (sub path 0 (1- order))))
                    path))
            (values (mapcar #`(elt all-tags %) path)
                    (expt 2 logprob))))))))


;;; Helpers

(defun tags->idx (ngram tags)
  "Convert tags in NGRAM to their indices in TAGS."
  (mapcar #`(position % TAGS) ngram))

(defun make-pi-matrix (dims &optional (init 0.0))
  "Make matrix for the Viterbi algorithm with the dimensions DIMS
   and initial elements INIT."
  (make-array dims :element-type 'float :initial-element init))