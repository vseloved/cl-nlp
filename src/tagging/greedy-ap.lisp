;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defclass greedy-ap-tagger (greedy-ap tagger)
  ())

(defmethod extract-fs ((model greedy-ap-tagger) &rest args)
  (ds-bind (i word ctx prev-tag prev2-tag) args
    (let* ((len (length ctx))
           (prev-word  (when (> i 0) (svref ctx (- i 1))))
           (prev2-word (when (> i 1) (svref ctx (- i 2))))
           (next-word  (when (< i (- len 1)) (svref ctx (+ i 1))))
           (next2-word (when (< i (- len 2)) (svref ctx (+ i 2)))))
      (cons word
            (make-fs model
                     "bias"
                     ("i word" word)
                     ("i suffix" (if (> (length word) 3) (substr word -3) word))
                     ("i pref1" (char word 0))
                     ("i-1 tag" prev-tag)
                     ("i-2 tag" prev2-tag)
                     ("i tag+i-2 tag" prev-tag prev2-tag)
                     ("i-1 tag+i word" prev-word word)
                     ("i-1 word" prev-word)
                     ("i-1 suffix" (when prev-word
                                     (if (> (length prev-word) 3)
                                         (substr prev-word -3)
                                         prev-word)))
                     ("i-2 word" prev2-word)
                     ("i+1 word" next-word)
                     ("i+1 suffix" (when next-word
                                     (if (> (length next-word) 3)
                                         (substr next-word -3)
                                         next-word)))
                     ("i+2 word" next2-word))))))

(defmethod train ((model greedy-ap-tagger) sents &key (epochs 5) debug)
  (let ((single-tag-words (or *single-tag-words*
                              (build-single-tag-words-dict sents)))
        (total (length sents)))
;    (init-model model)
    (dotimes (epoch epochs)
       (when debug
         (format t "~%~%==== Epoch: ~A ====~%~%" epoch))
       (let ((c 0) (n 0) (j 0))
         (dolist (sent sents)
           (let* ((prev :start)
                  (prev2 :start)
                  (ctx (map 'vector #`(normalize :for-tagging (token-word %))
                            sent)))
             (doindex (i token sent)
               (let* ((word (token-word token))
                      (fs (extract-fs model i word ctx prev prev2))
                      (guess (or (get# word single-tag-words)
                                 (let ((guess* (classify model fs)))
                                   (train1 model (rest fs) (token-tag token) guess*)
                                   guess*))))
                 (setf prev2 prev
                       prev guess)
                 (when debug
                   (incf c (if (eql guess (token-tag token)) 1 0))
                   (incf n))))
             (incf j)
             (when debug
               (format t "Sample ~A: ~A/~A=~5F% - ~2F% ~%"
                       n c n (float (* 100 (/ c n)))
                       (* 100 (/ j total)))))))
       (setf sents (nshuffle sents))))
  (dotable (f c-ws (m-weights model))
    (dotable (c w c-ws)
      (update1 model f c 0)
      (if (zerop w)
          (rem# c c-ws)
          (set# c c-ws (/ w (ap-step model))))))
  model)

(defmethod classify ((model greedy-ap-tagger) fs)
  (or (get# (first fs) *single-tag-words*)
      (call-next-method model (rest fs))))

;;; normalization

(defmethod normalize ((form (eql :for-tagging)) word)
  ;; (cond
  ;;   ((re:scan *number-regex* word) (make-string (length word) :initial-element #\0))
  ;;   ((re:scan *email-regex* word) "!EMAIL")
  ;;   ((re:scan *url-regex* word) "!URL")
  ;;   ((known? word)
  ;;    (string-downcase word))
  ;;   (t (if-it (position #\- (slice word 1))
  ;;             (let ((suffix (slice word (1+ it))))
  ;;               (if (known? suffix)
  ;;                   (string-downcase suffix)
  ;;                   "!UNK"))
  ;;             "!UNK"))))
  (cond
    ((and (find #\- word) (not (char= #\- (char word 0)))) "!HYPHEN")
    ((every #'digit-char-p word)
     (if (= 4 (length word)) "!YEAR" "!DIGITS"))
    (t (string-downcase word))))


(defun known? (word)
  t)

(defmethod extract-gold ((model greedy-ap-tagger) data)
  (let ((len (length data))
        (i 0)
        rez)
    (dolist (sent data)
      (let* ((prev :start)
             (prev2 :start)
             (ctx (map 'vector #`(normalize :for-tagging (token-word %))
                       sent)))
        (doindex (i token sent)
          (let* ((fs (extract-fs model i (token-word token) ctx prev prev2)))
            (push (pair (classify model fs) fs) rez)
            (setf prev2 prev
                  prev (classify model fs)))))
      (princ-progress (incf i) len))
    rez))

(defgeneric evaluate (model gold-fs)
  (:documentation
   "Measure MODEL's performance on GOLD-FS pairs of gold result and featues.")
  (:method (model gold-fs)
    (let ((matched 0) (total 0) (len (length gold-fs)))
      (dolist (sample gold-fs)
        (with-pair (gold fs) sample
          (when (equal gold (classify model fs))
            (incf matched)))
        (princ-progress (incf total) len))
      (* 100.0 (/ matched total)))))


;;; training

#+nil (

(defpar *train* ())
(defpar *dev* ())
(defpar *test* ())

(defun filter-dummy-tokens (sent)
  (remove-if #`(eql 'tag::-NONE- (token-tag %))
             sent))

(map-corpus :treebank "~/ext4/ontonotes/nw/wsj/"
            #`(when (< (parse-integer (substr (text-name %) -4))
                       1900)
                (appendf *train* (mapcar #'filter-dummy-tokens
                                         (text-tokens %))))
            :ext "parse")

(map-corpus :treebank "~/ext4/ontonotes/nw/wsj/"
            #`(when (<= 1900
                        (parse-integer (substr (text-name %) -4))
                        (1- 2200))
                (appendf *dev* (mapcar #'filter-dummy-tokens
                                       (text-tokens %))))
            :ext "parse")

(map-corpus :treebank "~/ext4/ontonotes/nw/wsj/"
            #`(when (>= (parse-integer (substr (text-name %) -4))
                        2200)
                (appendf *test* (mapcar #'filter-dummy-tokens
                                         (text-tokens %))))
            :ext "parse")

(defpar ntag::*single-tag-words*
    (ntag::build-single-tag-words-dict (append *train* *dev* *test*)))

(defpar *tagger* (make 'greedy-ap-tagger))
(train *tagger* *train* :debug t)

(defpar *gold-test* (extract-gold *tagger* *test*))
(defpar *gold-dev* (extract-gold *tagger* *dev*))
(defpar *gold-train* (extract-gold *tagger* *train*))

(evaluate *tagger* *gold-test*)

)
