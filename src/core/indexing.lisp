;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defun index-ngrams (order words &key ignore-case)
  "Make and ngrams-table of ORDER from a list of WORDS.
   May IGNORE-CASE."
  (make 'table-ngrams :order order
        :table
        (let ((ht (make-hash-table :test (if ignore-case 'equalp 'equal)
                                   :rehash-size 10.0))
              (last-idx (1- order)))
          (do ((tail words (rest tail)))
              ((shorter? tail order))
            (let ((cur (car tail)))
              (if (= 1 order)
                  (progn
                    (when (stringp cur)
                      (cond ((string= "¶" cur)
                             (:= cur "<S>"))
                            ((and (every #'period-char-p cur)
                                  (or (null (rest tail))
                                      (upper-case-p (char (second tail) 0))))
                             (:= tail (cons nil (cons "<S>" (rest tail)))))))
                    (:+ (get# cur ht 0)))
                  (:+ (get# (with ((ngram (subseq tail 0 order))
                                   (suffix (rest ngram)))
                              (cond
                                ;; paragraph start is sentence start
                                ((string= "¶" cur)
                                 (cons "<S>" suffix))
                                ;; paragraph end
                                ((string= "¶" (nth last-idx ngram))
                                 (setf tail (nthcdr (1- last-idx) tail))
                                 (append (butlast ngram) (list "</S>")))
                                ;; sentence end
                                ((and (upper-case-p
                                       (char (nth last-idx ngram) 0))
                                      (every 'period-char-p
                                             (nth (1- last-idx) ngram)))
                                 (setf tail (append (list nil "<S>")
                                                    (nthcdr last-idx tail)))
                                 (append (butlast ngram) (list "</S>")))
                                ;; inside sentence
                                (t ngram)))
                            ht 0)))))
          (when (= 1 order)
            (when (get# "<S>" ht)
              (set# "</S>" ht (:- (get# "<S>" ht)))))
          ht)))

(defun index-context-freqs (words &key ignore-order)
  "Create a table of weighted conditional frequencies
   of 1-word contexts to each side of a word
   (if IGNORE-ORDER both left_right and right_left
   are normalized and treated as the same context)
   for each distinct word in WORDS."
  ;; TODO: generalize for broader contexts
  (let ((ctxs #h(equal)))
    (loop :for (prev cur next) :on (cons "<S>" (append words (list "</S>")))
       :while next :do
       (unless (get# cur ctxs)
         (set# cur ctxs (make-hash-table :test 'equal)))
       (when (and (upper-case-p (char cur 0))
                  (ending-word-p prev))
         (setf prev "<S>"))
       (when (and (upper-case-p (char next 0))
                  (ending-word-p cur))
         (setf next "</S>"))
       (let ((prev_next (if (and ignore-order (string< next prev))
                            (strcat next "_" prev)
                            (strcat prev "_" next))))
         (set# prev_next (get# cur ctxs)
               (1+ (get# prev_next (get# cur ctxs) 0)))))
    (normalize-freqs ctxs)))

(defun index-prefix-transition-freqs (words &key (n 1))
  "Create a table of weighted conditional frequencies
   of next words for each distinct reversed N-word sequence in WORDS."
  (let ((transitions (make-hash-table :test 'equalp))
        (limit (length words))
        (count 0))
    ;; traversing the list of words from end
    (loop :for tail :on (reverse (append (make-list n) words))
       :while (< count limit) :do
       (incf count)
       (let* ((word (car tail))
              (prefix (sub tail 1 (1+ n))))
         (when (and (> n 1) (string= "¶" (car prefix)))
           (setf prefix (cons "¶" (make-list (1- n)))))
         (unless (get# prefix transitions)
           (set# prefix transitions #h(equal)))
         (set# word (get# prefix transitions)
               (1+ (get# word (get# prefix transitions) 0)))))
    (normalize-freqs transitions)))

(defun index-word-transition-freqs (words)
  "Create a table of weighted conditional frequencies
   of next words for each distinct word in WORDS."
  (let ((transitions #h(equalp))
        (word-vec (make-array (1+ (length words))
                              :initial-contents (cons "¶" words))))
    (dotimes (i (1- (length words)))
      (let ((prev (? word-vec i))
            (cur (? word-vec (+ i 1))))
        (unless (get# prev transitions)
          (set# prev transitions #h(equal)))
       (set# cur (get# prev transitions)
             (1+ (get# cur (get# prev transitions) 0)))))
    (normalize-freqs transitions)))


;;; Collocations

(defun find-collocations (bigrams &key (n 20) stopwords)
  "Find up to N strongest collocations in BIGRAMS."
  (let ((rez #h(equal))
        (left #h(equal))
        (right #h(equal))
        (total @bigrams.total-freq))
    (dotable (ngram freq @bigrams.table)
      (ds-bind (l r) ngram
        (set# l left (+ freq (get# l left 0)))
        (set# r right (+ freq (get# r right 0)))))
    (dotable (ngram freq (ngrams-table bigrams))
      (unless (reduce 'or2
                      (mapcar ^(member % stopwords :test 'string-equal)
                              ngram))
        (let ((lfreq (- (get# (car ngram) left) freq))
              (rfreq (- (get# (cadr ngram) right) freq)))
          (set# ngram rez
                (log-likelihood-ratio freq lfreq
                                      rfreq (- total lfreq rfreq freq))))))
    (take n (sort (keys ht) fn))))


;;; Helpers

(defun normalize-freqs (ht-of-hts)
  "For each table in HT-OF-HTS normalize all the values.
   Returns the modified HT-OF-HTS."
  (maphash ^(let ((total (sum 'just (vals %%))))
              (dotable (k v %%)
                (set# k %% (/ v total))))
           ht-of-hts)
  ht-of-hts)
  
