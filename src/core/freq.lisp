;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defun index-context-freqs (words &key ignore-order)
  "Create a table of weighted conditional frequencies
   of 1-word contexts to each side of a word
   (if IGNORE-ORDER both left_right and right_left
   are normalized and treated as the same context)
   for each distinct word in WORDS."
  ;; TODO: generalize for broader contexts
  (let ((ctxs (make-hash-table :test 'equal)))
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
              (prefix (subseq tail 1 (1+ n))))
         (when (and (> n 1) (string= "¶" (car prefix)))
           (setf prefix (cons "¶" (make-list (1- n)))))
         (unless (get# prefix transitions)
           (set# prefix transitions (make-hash-table :test 'equal)))
         (set# word (get# prefix transitions)
               (1+ (get# word (get# prefix transitions) 0)))))
    (normalize-freqs transitions)))

(defun index-word-transition-freqs (words)
  "Create a table of weighted conditional frequencies
   of next words for each distinct word in WORDS."
  (let ((transitions (make-hash-table :test 'equalp))
        (word-vec (make-array (1+ (length words))
                              :initial-contents (cons "¶" words))))
    (dotimes (i (1- (length words)))
      (let ((prev (elt word-vec i))
            (cur (elt word-vec (+ i 1))))
        (unless (get# prev transitions)
          (set# prev transitions (make-hash-table :test 'equal)))
       (set# cur (get# prev transitions)
             (1+ (get# cur (get# prev transitions) 0)))))
    (normalize-freqs transitions)))


;;; Helpers

(defun normalize-freqs (ht-of-hts)
  "For each table in HT-OF-HTS normalize all the values.
   Returns the modified HT-OF-HTS."
  (maphash #`(let ((total (reduce '+ (ht-vals %%))))
               (dotable (k v %%)
                 (set# k %% (/ v total))))
           ht-of-hts)
  ht-of-hts)