;;; (c) 2015-2016 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutilsx-readtable)


(defclass porter-stemmer (stemmer) ())

(defmethod stem ((stemmer porter-stemmer) word)
  (if (<= (length word) 2)
      word
      (-> (make-array (length word) :element-type 'character
                                    :fill-pointer t :initial-contents word)
          step1ab step1c step2 step3 step4 step5)))

(ncore::def-lang-var porter-stemmer (make 'porter-stemmer)
  "Porter stemmer")

;;; steps

(defun step1ab (str)
  "Get rid of plurals and -ed or -ing.

   caresses  ->  caress
   ponies    ->  poni
   ties      ->  ti
   caress    ->  caress
   cats      ->  cat
   feed      ->  feed
   agreed    ->  agree
   disabled  ->  disable
   matting   ->  mat
   mating    ->  mate
   meeting   ->  meet
   milling   ->  mill
   messing   ->  mess
   meetings  ->  meet"
  (-> str
      stem-s
      stem-ed/ing))

(defun stem-s (str)
  (when (and (ends-with "s" str)
             (not (ends-with "ss" str)))
    (:- (fill-pointer str)
        (if (or (ends-with "sses" str)
                (ends-with "ies" str))
            2 1)))
  str)

(defun stem-ed/ing (str)
  (let (changed)
    (cond ((ends-with "eed" str)
           (when (plusp (consonant-seqs-count str 2))
             (:- (fill-pointer str) 1)
             (:= changed t)))
          ((and (ends-with "ed" str)
                (vowel-in-stem? str 2))
           (:- (fill-pointer str) 2)
           (:= changed t))
          ((and (ends-with "ing" str)
                (vowel-in-stem? str 3))
           (:- (fill-pointer str) 3)
           (:= changed t)))
    (when changed
      (cond ((or (member str '("at" "bl" "iz" "is") :test ^(ends-with %% %))
                 (and (= (consonant-seqs-count str) 1)
                      (consonant-vowel-consonant? str)))
             (vector-push-extend #\e str))
            ((and (double-consonant? str (1- (length str)))
                  (not (member (last-char str) '(#\l #\s #\z))))
             (:- (fill-pointer str)))))
    str))

(defun step1c (str)
  "Turn terminal y to i when there is another vowel in the stem."
  (when (and (ends-with "y" str)
             (vowel-in-stem? str 1))
    (:= (char str (1- (length str))) #\i))
  str)

(defun step2 (str)
  "Map double suffices to single ones.
   So -ization ( = -ize plus -ation) maps to -ize etc."
  (change-suffixes str
                   "ational" "ate"
                   "tional" "tion"
                   "enci" "ence"
                   "anci" "ance"
                   "izer" "ize"
                   "bli" "ble"
                   ;; original algorithm: replace prev line with next
                   ;; "abli" "able"
                   "alli" "al"
                   "entli" "ent"
                   "eli" "e"
                   "ousli" "ous"
                   "ization" "ize"
                   "ation" "ate"
                   "ator" "ate"
                   "alism" "al"
                   "iveness" "ive"
                   "fulness" "ful"
                   "ousness" "ous"
                   "aliti" "al"
                   "iviti" "ive"
                   "biliti" "ble"
                   ;; original algo: remove next line
                   "logi" "log"
                   ))

(defun step3 (str)
  "Deal with -ic-, -full, -ness etc."
  (change-suffixes str
                   "icate" "ic"
                   "ative" ""
                   "alize" "al"
                   "iciti" "ic"
                   "ical" "ic"
                   "ful" ""
                   "ness" ""))

(defun step4 (str)
  "Take off -ant, -ence etc., in context <c>vcvc<v>."
  (if (and (ends-with "ion" str)
           (not (string= "ion" str))
           (member (char str (- (length str) 4)) '(#\s #\t)))
      (change-suffix str "" 3 2)
      (dolist (suffix '("al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ment"
                        "ent" "ou" "ism" "ate" "iti" "ous" "ive" "ize"))
        (when (ends-with suffix str)
          (:= str (change-suffix str "" (length suffix) 2))
          (return))))
  str)

(defun step5 (str)
  "Remove a final -e and change -ll to -l if length > 1."
  (let ((len (fill-pointer str)))
    (if (eql (char str (1- len)) #\e)
        (let ((csc (consonant-seqs-count str)))
          (when (or (> csc 1)
                    (and (= csc 1)
                         (not (consonant-vowel-consonant?
                               (slice str 0 (1- (length str)))))))
            (:- (fill-pointer str))))))
  (let ((len (fill-pointer str)))
    (if (and (eql (last-char str) #\l)
             (double-consonant? str (1- len))
             (> (consonant-seqs-count str) 1))
        (:- (fill-pointer str))))
  str)


;;; predicates

(defun consonant-at? (i str)
  "Tests if character in STR at I is a consonant."
  (case (char str i)
    ((#\a #\e #\i #\o #\u) nil)
    (#\y (or (= i 0)
             (not (consonant-at? (1- i) str))))
    (otherwise t)))

(defun consonant-seqs-count (str &optional (-end 0))
  "Measure the number of consonant sequences in STR between 0 and -END.

   If c is a consonant sequence and v a vowel sequence:

   <c><v>       gives 0
   <c>vc<v>     gives 1
   <c>vcvc<v>   gives 2
   <c>vcvcvc<v> gives 3"
  (let ((n 0)
        (i 0)
        (end (- (length str) -end)))
    (loop
       (when (>= i end) (return-from consonant-seqs-count 0))
       (unless (consonant-at? i str) (return))
       (:+ i))
    (:+ i)
    (loop
       (loop
          (when (>= i end) (return-from consonant-seqs-count n))
          (when (consonant-at? i str) (return))
          (:+ i))
       (:+ i)
       (:+ n)
       (loop
          (when (>= i end) (return-from consonant-seqs-count n))
          (unless (consonant-at? i str) (return))
          (:+ i))
       (:+ i))))

(defun vowel-in-stem? (str &optional (-end 0))
  "Check whether STR's stem (up to -END) contains a vowel."
  (loop :for i :from 0 :below (- (length str) -end)
     :unless (consonant-at? i str) :return t))

(defun consonant-vowel-consonant? (str)
  "Check if STR has a form consonant-vowel-consonant
   and also if the second consonant is not 'w','x' or 'y'.
   This is used when trying to restore an e at the end of a short word. e.g.

      cav(e), lov(e), hop(e), crim(e), but not snow, box, tray."
  (let ((end (length str)))
    (and (> end 2)
         (consonant-at? (:- end) str)
         (not (member (char str end) '(#\w #\x #\y)))
         (not (consonant-at? (:- end) str))
         (consonant-at? (:- end) str))))

(defun double-consonant? (str i)
  "Check if character pair at position I in STR is a double consonant."
  (and (>= i 1)
       (consonant-at? i str)
       (char= (char str i) (char str (1- i)))))


;;; morphers

(defun change-suffixes (str &rest suffix-pairs)
  (when (> (length str) 2)
    (loop :for (suffix change) :on suffix-pairs :by #'cddr :do
      (when (ends-with suffix str)
        (change-suffix str change (length suffix))
        (loop-finish))))
  str)

(defun change-suffix (str suffix offset &optional (consonant-seqs-min 1))
  (when (>= (consonant-seqs-count str offset)
            consonant-seqs-min)
    (:- (fill-pointer str) offset)
    (loop :for char :across suffix :do
      (vector-push-extend char str)))
  str)
