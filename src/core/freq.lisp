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
        (len (- (length words) n))
        (count 0))
    (loop :for tail :on (reverse (cons "¶" words))
          :while (< count len)
          :do (let ((cur (car tail))
                    (prefix (subseq tail 1 (1+ n))))
                (unless (get# prefix transitions)
                  (set# prefix transitions (make-hash-table :test 'equal)))
                (set# cur (get# prefix transitions)
                      (1+ (get# cur (get# prefix transitions) 0)))))
    (normalize-freqs transitions)))

(defun index-word-transition-freqs (words)
  "Create a table of weighted conditional frequencies
   of next words for each distinct word in WORDS."
  (let ((transitions (make-hash-table :test 'equalp))
        (word-vec (make-array (1+ (length words))
                              :initial-contents (cons "¶" words))))
    (dotimes (i (- (length words) n))
      (let ((prefix (susbeq words i (+ i n)))
            (cur (elt words (+ i 1))))
        (unless (get# prefix transitions)
          (set# prefix transitions (make-hash-table :test 'equal)))
       (set# cur (get# prefix transitions)
             (1+ (get# cur (get# prefix transitions) 0)))))
    (normalize-freqs transitions)))


;;; Helpers

(defun normalize-freqs (ht-of-hts)
  "For each table in HT-OF-HTS normalize all the values.
   Returns the modified HT-OF-HTS."
  (maphash #`(let ((total (reduce '+ (ht-vals %))))
               (dotable (k v %)
                 (set# k % (/ v total))))
           ht-of-hts))