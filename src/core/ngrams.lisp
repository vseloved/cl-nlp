;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


;;; Abstract Ngrams

(defclass ngrams ()
  ((order :initarg :order :initform 1 :reader ngrams-order)
   (count :reader ngrams-count)
   (max-freq :reader ngrams-max-freq)
   (min-freq :reader ngrams-min-freq)
   (total-freq :reader ngrams-total-freq))
  (:documentation
   "An abstract ngrams interface."))

(defmethod print-object ((ngrams ngrams) stream)
  (print-unreadable-object (ngrams stream :type t :identity t)
    (if (slot-boundp ngrams 'order)
        (with-accessors ((order ngrams-order) (count ngrams-count)
                         (total-freq ngrams-total-freq)) ngrams
          (format stream "order:~A count:~A outcomes:~A"
                  order count total-freq))
        (format stream "not initialized"))))

(defgeneric ngrams-eq (ngrams)
  (:documentation
   "Get the equality predicate of NGRAMS (can be EQUAL or EQUALP)."))

(defgeneric ngrams-pairs (ngrams &key order-by)
  (:documentation
   "Get the alist of all ngrams with their frequencies in NGRAMS,
    possibly ordered by ORDER-BY predicate (e.g. < or >)."))

(defgeneric vocab (ngrams &key order-by)
  (:documentation
   "Get the list of all ngrams in NGRAMS,
    possibly ordered by ORDER-BY predicate (e.g. < or >)."))

(defgeneric freq (ngrams ngram)
  (:documentation
   "Get the NGRAM frequency in NGRAMS.")
  (:method :around ((ngrams ngrams) (ngram string))
    (if (> (ngrams-order ngrams) 1)
        (freq ngrams (tokenize-ngram ngrams ngram))
        (call-next-method))))

(defgeneric prob (ngrams ngram)
  (:documentation
   "Get the NGRAM probability in NGRAMS.")
  (:method ((ngrams ngrams) ngram)
    (/ (freq ngrams ngram)
       (ngrams-total-freq ngrams)))
  (:method :around ((ngrams ngrams) (ngram string))
    (if (> (ngrams-order ngrams) 1)
        (prob ngrams (tokenize-ngram ngrams ngram))
        (call-next-method))))

(defgeneric logprob (ngrams ngram)
  (:documentation
   "Get the log (to base 2) of NGRAM probability in NGRAMS.")
  (:method ((ngrams ngrams) ngram)
    (let ((prob (prob ngrams ngram)))
      (if (zerop prob)
          nil
          (log prob 2))))
  (:method :around ((ngrams ngrams) (ngram string))
    (if (> (ngrams-order ngrams) 1)
        (logprob ngrams (tokenize-ngram ngrams ngram))
        (call-next-method))))

(defgeneric cond-prob (ngrams ngram)
  (:documentation
   "Get the NGRAM conditional probability in NGRAMS.
    By conditional probability we mean the probability of occurrence
    of the last word given the previous words.")
  (:method :around ((ngrams ngrams) ngram)
    (if (= 1 (ngrams-order ngrams))
        1
        (call-next-method)))
  (:method :around ((ngrams ngrams) (ngram string))
    (if (> (ngrams-order ngrams) 1)
        (cond-prob ngrams (tokenize-ngram ngrams ngram))
        (call-next-method))))

(defgeneric cond-logprob (ngrams ngram)
  (:documentation
   "Get the log of NGRAM conditional probability in NGRAMS.
    By conditional probability we mean the probability of occurrence
    of the last word given the previous words.")
  (:method :around ((ngrams ngrams) (ngram string))
    (if (> (ngrams-order ngrams) 1)
        (cond-logprob ngrams (tokenize-ngram ngrams ngram))
        (call-next-method))))

(defgeneric freqs (ngrams &rest ngrams-list)
  (:documentation
   "Get the list of frequencies of ngrams from NGRAMS-LIST in NGRAMS.")
  (:method (ngrams &rest ngrams-list)
    (mapcar #`(freq ngrams %) ngrams-list)))

(defgeneric probs (ngrams &rest ngrams-list)
  (:documentation
   "Get the list of probabilities of ngrams from NGRAMS-LIST in NGRAMS.")
  (:method (ngrams &rest ngrams-list)
    (mapcar #`(prob ngrams %) ngrams-list)))

(defgeneric logprobs (ngrams &rest ngrams-list)
  (:documentation
   "Get the list of logs of probability of ngrams from NGRAMS-LIST in NGRAMS.")
  (:method (ngrams &rest ngrams-list)
    (mapcar #`(logprob ngrams %) ngrams-list)))

(defgeneric cond-probs (ngrams &rest ngrams-list)
  (:documentation
   "Get the conditional probabilities of ngrams from NGRAMS-LIST in NGRAMS.
    By conditional probability we mean the probability of occurrence
    of the last word given the previous words.")
  (:method ((ngrams ngrams) &rest ngrams-list)
    (mapcar #`(cond-prob ngrams %) ngrams-list)))

(defgeneric cond-logprobs (ngrams &rest ngrams-list)
  (:documentation
   "Get the logs of conditional probability of ngrams from NGRAMS-LIST in NGRAMS.
    By conditional probability we mean the probability of occurrence
    of the last word given the previous words.")
  (:method ((ngrams ngrams) &rest ngrams-list)
    (mapcar #`(cond-logprob ngrams %) ngrams-list)))

(defgeneric top-ngram (ngrams)
  (:documentation
   "Get some ngram with the highest frequency in NGRAMS."))

(defgeneric hapaxes (ngrams)
  (:documentation
   "Get all the ngrams with the lowest frequency in NGRAMS.
    Second value is the frequency itself."))


;;; Table-based ngrams

(defclass table-ngrams (ngrams)
  ((table :initform (make-hash-table :test 'equal) :initarg :table
          :reader ngrams-table))
  (:documentation
   "Ngrams with hash-table source."))

(defmethod initialize-instance :after ((ngrams table-ngrams) &key)
  (with-slots (table order max-freq min-freq total-freq) ngrams
    (check-type table hash-table)
    (assert (member (hash-table-test table) '(equal equalp)))
    (with-hash-table-iterator (gen-fn table)
      (when-it (nth-value 2 (gen-fn))
        (setf total-freq (setf max-freq (setf min-freq it)))
        (loop
           (mv-bind (next? _ freq) (gen-fn)
             (unless next? (return))
             (incf total-freq freq)
             (when (< freq min-freq)
               (setf min-freq freq))
             (when (> freq max-freq)
               (setf max-freq freq))))))))

(defmethod ngrams-count ((ngrams table-ngrams))
  (hash-table-count (ngrams-table ngrams)))

(defmethod ngrams-eq ((ngrams table-ngrams))
  (hash-table-test (ngrams-table ngrams)))

(defmethod vocab ((ngrams table-ngrams) &key order-by)
  (with-slots (table) ngrams
    (if order-by
        (mapcar #'car (sort (ngrams-pairs ngrams) order-by :key #'cdr))
        (ht-keys table))))

(defmethod ngrams-pairs ((ngrams table-ngrams) &key order-by)
  (with-slots (table) ngrams
    (if order-by
        (sort (ht->alist table) order-by :key #'cdr)
        (ht->alist table))))

;; (defmethod ngrams-pairs ((ngrams table-ngrams))
;;   (let ((total 0)
;;         (freq 0)
;;         (eq-test (ngrams-eq ngrams))
;;         (prefix (butlast ngram)))
;;     (dolist (ng (vocab ngrams))
;;       (cond ((funcall eq-test ngram ng)
;;              (incf total (setf freq (freq ng))))
;;             ((funcall eq-test prefix (butlast ng))
;;              (incf total (freq ng)))))
;;     (if (zerop total) 0
;;         (/ freq total))))

(defmethod freq ((ngrams table-ngrams) ngram)
  (get# ngram (ngrams-table ngrams) 0))

(defmethod freqs ((ngrams table-ngrams) &rest ngrams-list)
  (mapcar #`(freq (ngrams-table ngrams) %)
          ngrams-list))

(defmethod probs ((ngrams table-ngrams) &rest ngrams-list)
  (mapcar #`(prob (ngrams-table ngrams) %)
          ngrams-list))

(defmethod cond-prob ((ngrams hash-table) ngram)
  (let ((total 0)
        (freq 0)
        (eq-test (ngrams-eq ngrams))
        (prefix (butlast ngram)))
    (maphash #`(cond ((funcall eq-test ngram %)
                      (incf total (setf freq %%)))
                     ((funcall eq-test prefix (butlast %))
                      (incf total %%)))
             (ngrams-source ngrams))
    (if (zerop total) 0
        (/ freq total))))

(defmethod top-ngram ((ngrams table-ngrams))
  (with-slots (table max-freq) ngrams
    (dotable (ngram freq table)
      (when (= (max-freq ngrams) freq)
        (return ngram)))))

(defmethod hapaxes ((ngrams table-ngrams))
  (let (rez)
    (with-slots (table min-freq) ngrams
      (dotable (ngram freq table)
        (when (= min-freq freq)
          (push ngram rez)))
      (values rez
              min-freq))))


;;; Helper functions

(defun tokenize-ngram (ngrams str)
  "Transform string STR to a list if necessary (depending of order of NGRAMS)."
  (if (> (ngrams-order ngrams) 1)
      (tokenize <word-tokenizer> str)
      str))
