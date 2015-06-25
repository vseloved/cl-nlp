;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.lexics)
(named-readtables:in-readtable rutils-readtable)


(defclass stemmer () ())

(defclass porter-stemmer (stemmer) ())

(defmethod stemmize ((stemmer porter-stemmer) word)
  (when (<= (length str) 2)
    (return-from stem word))
  (-> (make-array (length word) :element-type 'character
                  :fill-pointer t :initial-contents word)
      stem-1ab step1c))


(defun stem-1ab (str)
  "caresses  ->  caress
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
  (-> str stem-s stem-ed stem-ing))

(defun stem-s (str)
  "Get rid of plurals and -ed or -ing."
  (when (and (ends-with "s" str)
             (not (ends-woth "ss" str)))
    (:- (fill-pointer str)
        (if (or (ends-with "sses" str)
                (ends-with "ies" str))
            2 1)))
  str)

(defun stem-ed (str)
  (when (and (ends-with "ed" str)
             (not (vowel-in-stem? str 2)))
    (:- (fill-pointer str)
        (if (and (ends-with "eed" str)
                 (zerop (consonant-seqs-count str 2)))
            3 2)))
  str)

(defun stem-ing (str)
  (when (and (ends-with "ing" str)
             (not (vowel-in-stem? str 3)))
    (:- (fill-pointer str) 3))
  str)

(defun stem-post-ing/ed (str)
  (cond ((or (member str '("at" "bl" "iz" "is") :test #`(ends-with %% %))
             (and (= (consonant-seqs-count str) 1)
                  (consonant-vowel-consonant? str)))
         (vector-push-extend "e" str))
        ((and (doublec str (1- (length str)))
              (not (member (last-char str) '(#\l #\s #\z))))
         (:- (fill-pointer str))))
  str)

(defun step1c (str)
  "Turn terminal y to i when there is another vowel in the stem."
  (let ((saved-fill-pointer (fill-pointer str)))
    (when (and (ends str "y")
               (vowelinstem str))
      (:= (char str (fill-pointer str)) #\i))
    (:= (fill-pointer str) saved-fill-pointer))
  str)


;;; Predicates

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
       (when (>= i end) (return-from consonant-seqs-count n))
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


;; (defun doublec (str i)
;;   (and (>= i 1)
;;        (consonant-at? i str)
;;        (char= (char str i) (char str (1- i)))))



;; (defun r (str s sfp)
;;   (if (> (consonant-seqs-count str (fill-pointer str)) 0)
;;       (setto str s)
;;     (setf (fill-pointer str) sfp)))

;; ;; step1ab()


;; (defun step1ab (str)
;;   "Get rid of plurals and -ed or -ing."
;;   (when (eql (char str (1- (fill-pointer str))) #\s)
;;     (cond ((ends str "sses") (incf (fill-pointer str) 2))
;;           ((ends str "ies")  (setto str "i"))
;;           ((not (eql (char str (- (fill-pointer str) 2)) #\s)) (decf (fill-pointer str)))))
;;   (cond ((ends str "eed") (if (> (consonant-seqs-count str (fill-pointer str)) 0)
;;                               (incf (fill-pointer str) 2)
;;                               (incf (fill-pointer str) 3)))
;;         ((let ((sfp (fill-pointer str)))
;;            (if (or (ends str "ed")
;;                    (ends str "ing"))
;;                (if (vowelinstem str)
;;                    t
;;                    (progn (setf (fill-pointer str) sfp)
;;                           nil))))
;;          (cond ((ends str "at") (setto str "ate"))
;;                ((ends str "bl") (setto str "ble"))
;;                ((ends str "iz") (setto str "ize"))
;;                ((doublec str (1- (fill-pointer str)))
;;                 (unless (member (char str (1- (fill-pointer str))) '(#\l #\s #\z))
;;                   (decf (fill-pointer str))))
;;                (t (if (and (= (consonant-seqs-count str (fill-pointer str)) 1)
;;                            (consonant-vowel-consonant? str (fill-pointer str)))
;;                       (setto str "e"))))))
;;   str)

;; ;; step1c()

;; (defun step1c (str)
;;   "Turn terminal y to i when there is another vowel in the stem."
;;   (let ((p (fill-pointer str)))
;;     (when (and (ends str "y")
;;                (vowelinstem str))
;;       (setf (char str (fill-pointer str)) #\i))
;;     (setf (fill-pointer str) p))
;;   str)

;; ;; step2() maps double suffices to single ones. so -ization ( = -ize plus
;; ;; -ation) maps to -ize etc. note that the string before the suffix must give
;; ;; m() > 0.

;; (defun step2 (str)
;;   "Map double suffices to single ones. so -ization ( = -ize plus -ation) maps to -ize etc."
;;   (let ((p (fill-pointer str)))
;;     (when (> p 2)
;;       (block nil
;;         (case (char str (- (length str) 2))
;;           (#\a (when (ends-with "ational" str) (r str "ate"  sfp)  (return))
;;                (when (ends-with "tional" str)  (r str "tion" sfp) (return)))
;;           (#\c (when (ends-with "enci" str)    (r str "ence" sfp) (return))
;;                (when (ends-with "anci" str)    (r str "ance" sfp) (return)))
;;           (#\e (when (ends-with "izer" str)    (r str "ize"  sfp)  (return)))
;;           (#\l (when (ends-with "bli" str)     (r str "ble"  sfp)  (return))
;;                (when (ends-with "alli" str)    (r str "al"  sfp)   (return))
;;                (when (ends-with "entli" str)   (r str "ent" sfp)  (return))
;;                (when (ends-with "eli" str)     (r str "e"   sfp)    (return))
;;                (when (ends-with "ousli" str)   (r str "ous" sfp)  (return)))
;;           (#\o (when (ends-with "ization" str) (r str "ize" sfp)  (return))
;;                (when (ends-with "ation" str)   (r str "ate" sfp)  (return))
;;                (when (ends-with "ator" str)    (r str "ate" sfp)  (return)))
;;           (#\s (when (ends-with "alism" str)   (r str "al"  sfp)   (return))
;;                (when (ends-with "iveness" str) (r str "ive" sfp)  (return))
;;                (when (ends-with "fulness" str) (r str "ful" sfp)  (return))
;;                (when (ends-with "ousness" str) (r str "ous" sfp)  (return)))
;;           (#\t (when (ends-with "aliti" str)   (r str "al"  sfp)   (return))
;;                (when (ends-with "iviti" str)   (r str "ive" sfp)  (return))
;;                (when (ends-with "biliti" str)  (r str "ble" sfp)  (return)))
;;           (#\g (when (ends-with "logi" str)    (r str "log" sfp)  (return)))))))
;;   str)

;; ;; step3() deals with -ic-, -full, -ness etc. similar strategy to step2.

;; (defun step3 (str)
;;   (let ((sfp (fill-pointer str)))
;;     (block nil
;;       (case (char str (1- (length str)))
;;         (#\e (when (ends str "icate") (r str "ic" sfp) (return))
;;              (when (ends str "ative") (r str "" sfp)   (return)) ; huh?
;;              (when (ends str "alize") (r str "al" sfp) (return)))
;;         (#\i (when (ends str "iciti") (r str "ic" sfp) (return)))
;;         (#\l (when (ends str "ical")  (r str "ic" sfp) (return))
;;              (when (ends str "ful")   (r str "" sfp)   (return))) ; huh?
;;         (#\s (when (ends str "ness")  (r str "" sfp)   (return))) ; huh?
;;         )))
;;   str)

;; ;; step4() takes off -ant, -ence etc., in context <c>vcvc<v>.

;; (defun step4 (str)
;;   (let ((sfp (fill-pointer str)))
;;     (when (> sfp 2)                     ; Unnecessary?
;;       (block nil
;;         (case (char str (- sfp 2))
;;           (#\a (if (ends str "al")    (return)))
;;           (#\c (if (ends str "ance")  (return))
;;                (if (ends str "ence")  (return)))
;;           (#\e (if (ends str "er")    (return)))
;;           (#\i (if (ends str "ic")    (return)))
;;           (#\l (if (ends str "able")  (return))
;;                (if (ends str "ible")  (return)))
;;           (#\n (if (ends str "ant")   (return))
;;                (if (ends str "ement") (return))
;;                (if (ends str "ment")  (return))
;;                (if (ends str "ent")   (return)))
;;           (#\o (if (ends str "ion")
;;                    (let ((len (length str)))
;;                      (if (and (> len 0)
;;                               (let ((c (char str (1- len))))
;;                                 (or (eql c #\s) (eql c #\t))))
;;                          (return)
;;                          (setf (fill-pointer str) sfp))))
;;                (if (ends str "ou")    (return))) ; takes care of -ous
;;           (#\s (if (ends str "ism")   (return)))
;;           (#\t (if (ends str "ate")   (return))
;;                (if (ends str "iti")   (return)))
;;           (#\u (if (ends str "ous")   (return)))
;;           (#\v (if (ends str "ive")   (return)))
;;           (#\z (if (ends str "ize")   (return))))
;;         (return-from step4 str))
;;       (unless (> (consonant-seqs-count str (fill-pointer str)) 1)
;;         (setf (fill-pointer str) sfp)))
;;     str))

;; ;; step5() removes a final -e if m() > 1, and changes -ll to -l if m() > 1.

;; (defun step5 (str)
;;   (let ((len (fill-pointer str)))
;;     (if (eql (char str (1- len)) #\e)
;;         (let ((a (consonant-seqs-count str len)))
;;           (if (or (> a 1)
;;                   (and (= a 1)
;;                        (not (consonant-vowel-consonant? str (1- len)))))
;;               (decf (fill-pointer str))))))
;;   (let ((len (fill-pointer str)))
;;     (if (and (eql (char str (1- len)) #\l)
;;              (doublec str (1- len))
;;              (> (consonant-seqs-count str len) 1))
;;         (decf (fill-pointer str))))
;;   str)

;; ;; In stem(p,i,j), p is a char pointer, and the string to be stemmed is from p[i] to p[j]
;; ;; inclusive. Typically i is zero and j is the offset to the last character of a string,
;; ;; (p[j+1] == '\0'). The stemmer adjusts the characters p[i] ... p[j] and returns the new
;; ;; end-point of the string, k.  Stemming never increases word length, so i <= k <= j. To
;; ;; turn the stemmer into a module, declare 'stem' as extern, and delete the remainder of
;; ;; this file.

;; (defun porter-stem (str)
;;   (let ((len (length str)))
;;     ;; With this line, strings of length 1 or 2 don't go through the
;;     ;; stemming process, although no mention is made of this in the
;;     ;; published algorithm. Remove the line to match the published
;;     ;; algorithm.
;;     (if (<= len 2) (return-from stem str)) ; /*-DEPARTURE-*/
;;     (if (typep str 'simple-string)         ; Primarily for testing.
;;         (setf str
;;               (make-array len :element-type 'character
;;                           :fill-pointer len :initial-contents str)))
;;     (step1ab str) (step1c str) (step2 str) (step3 str) (step4 str) (step5 str)
;;     str))

;; #+never
;; (defun test ()				; Run against the distributed test files.
;;   (with-open-file (f1 "voc.txt")
;;     (with-open-file (f2 "output.txt")
;;       (loop as w1 = (read-line f1 nil nil)
;; 	  while w1
;; 	  as w2 = (read-line f2 nil nil)
;; 	  as w3 = (stem w1)
;; 	  if (equal w2 w3)
;; 	  count t into successes
;; 	  else count t into failures
;; 	  and do (format t "(stem ~s) => ~s wanted ~s~%" w1 w3 w2)
;; 	  finally (progn (format t "sucesses ~d failures ~d~%" successes failures)
;; 			 (return failures))))))
