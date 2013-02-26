;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defclass language-model ()
  ((order :initarg :order :reader lm-order)
   (ngrams :initarg :ngrams :reader lm-ngrams))
  (:documentation
   "Language model is a collection of NGRAMS of all orders from 1 upto ORDER."))

(defmethod vocab ((model language-model) &key order-by)
  (vocab (get-ngrams 1 model) :order-by order-by))

(defgeneric make-lm (class &key 1g 2g 3g 4g 5g &allow-other-keys)
  (:documentation
   "Make instance of a langauge model of a certain CLASS
    with provided unigrams (1G), ... up to fivegrams (5G)."))

(defmethod make-lm (class &key 1g 2g 3g 4g 5g &allow-other-keys)
  (let ((order (cond (5g 5)
                     (4g 4)
                     (3g 3)
                     (2g 2)
                     (1g 1)
                     (t (error "No ngrams supplied")))))
    (make class
          :order order
          :ngrams
          (make-array
           (1+ order)
           :initial-contents
           (case order
             (1 (list nil
                      1g))
             (2 (list nil
                      (or 1g (error "No unigrams supplied for LM of order 2"))
                      2g))
             (3 (list nil
                      (or 1g (error "No unigrams supplied for LM of order 3"))
                      (or 2g (error "No bigrams supplied for LM of order 3"))
                      3g))
             (4 (list nil
                      (or 1g (error "No unigrams supplied for LM of order 4"))
                      (or 2g (error "No bigrams supplied for LM of order 4"))
                      (or 3g (error "No trigrams supplied for LM of order 4"))
                      4g))
             (5 (list nil
                      (or 1g (error "No unigrams supplied for LM of order 5"))
                      (or 2g (error "No bigrams supplied for LM of order 5"))
                      (or 3g (error "No trigrams supplied for LM of order 5"))
                      (or 4g (error "No fourgrams supplied for LM of order 5"))
                      5g)))))))

(defgeneric perplexity (model test-sentences)
  (:documentation
   "Calculate perplexity of the MODEL on the list of TEST-SENTENCES."))

(defmethod perplexity ((model language-model) test-sentences)
  (expt 2 (- (/ (reduce #'+ (mapcar #`(logprob ngrams %) test-sentences))
                (reduce #'+ (mapcar #'length test-sentences))))))


;;; Stupid Backoff LM

(defclass stupid-backoff-lm (language-model)
  ((backoff :initarg backoff :initform 0.4 :reader lm-backoff))
  (:documentation
   "Stupid Backoff language model."))

(defmethod prob ((lm language-model) (sentence string))
  (prob lm (tokenize <word-tokenizer> sentence)))

(defmethod prob ((lm language-model) (sentence list))
  (expt 2 (logprob lm sentence)))

(defmethod logprob ((lm language-model) (sentence string))
  (logprob lm (tokenize <word-tokenizer> sentence)))

(defmethod logprob ((model language-model) (sentence list))
  (unless sentence (return-from logprob nil))
  (with-slots (order) model
    (let ((rez 0))
      (if (= 1 order)
          (dolist (word sentence rez)
            (incf rez (logprob (get-ngrams 1 model) word)))
          (let ((s (append (cons "<S>" sentence) (list "</S>"))))
            (if (shorter? s order)
                (logprob (get-ngrams (length s) model) s)
                (progn
                  (do ((i 2 (1+ i)))
                      ((= i order))
                    (incf rez (cond-logprob model (sub s 0 i))))
                  (do ((tail s (rest tail)))
                      ((shorter? tail order))
                    (let ((ngram (sub tail 0 order)))
                      (unless (search '("</S>" "<S>") ngram :test 'equal)
                        (incf rez (cond-logprob model ngram)))))
                  rez)))))))

(defmethod cond-prob ((model stupid-backoff-lm) ngram)
  (with-accessors ((ngrams lm-ngrams) (backoff lm-backoff)) model
    (let ((coef 1)
          (len (length ngram)))
      (loop :for i :from len :downto 1 :do
         (let* ((cur (butlast ngram (- len i)))
                (freq (freq (elt ngrams i)
                            (if (cdr cur) cur (car cur)))))
           (if (zerop freq)
               (setf coef (* coef backoff))
               (return-from cond-prob
                 (* coef (/ freq
                            (case i
                              (1 (ngrams-total-freq (elt ngrams 1)))
                              (2 (freq (elt ngrams 1) (car ngram)))
                              (otherwise
                               (freq (elt ngrams (1- i)) (butlast ngram))))))))))
      (* coef (/ (ngrams-min-freq (elt ngrams 1))
                 (ngrams-total-freq (elt ngrams 1)))))))

(defmethod cond-logprob ((model stupid-backoff-lm) ngram)
  (log2 (cond-prob model ngram)))


;;; Helper functions

(declaim (inline get-ngrams))
(defun get-ngrams (order model)
  "Get ngrams of a given ORDER from MODEL."
  (assert (<= order (lm-order model)))
  (elt (lm-ngrams model) order))