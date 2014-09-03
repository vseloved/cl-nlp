;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defclass greedy-ap-tagger (avg-perceptron tagger)
  ((dict :initform #h(equal) :initarg :dict :accessor tgr-dict)
   (single-tag-words :initform #h(equalp) :initarg :single-tag-words
                     :accessor tgr-single-tag-words))
  (:documentation
   "A greedy averaged perceptron tagger with single-tag words dictionary lookup."))


(defmethod tag ((tagger greedy-ap-tagger) (sentence sentence))
  (let ((prev :-START-)
        (prev2 :-START2-)
        (ctx (sent-ctx tagger sentence)))
    (doindex (i token (sent-tokens sentence))
      (:= (token-tag token)
          (classify tagger
                    (extract-fs tagger i (token-word token) ctx prev prev2))
          prev2 prev
          prev (token-tag token)))
    sentence))

(defmethod classify ((model greedy-ap-tagger) fs)
  (or (get# (first fs) (tgr-single-tag-words model))
      (call-next-method model (rest fs))))

(defmethod extract-fs ((model greedy-ap-tagger) &rest args)
  (ds-bind (i word ctx prev-tag prev2-tag) args
    (let* ((i (+ i 2))
           (prev-word  (svref ctx (- i 1)))
           (prev2-word (svref ctx (- i 2)))
           (next-word  (svref ctx (+ i 1)))
           (next2-word (svref ctx (+ i 2))))
      (cons word
            (make-fs model
                     "bias"
                     ;; ("i case" (cond ((every #'upper-case-p word) "all_caps")
                     ;;                 ((upper-case-p (char word 0)) "uper")
                     ;;                 (t "lower")))
                     ;; ("i suffix1" (last-char word))
                     ("i pref1" (char word 0))
                     ("i suf3" (if (> (length word) 3) (substr word -3) word))
                     ("i word" word)
                     ("i-1 tag" prev-tag)
                     ("i-2 tag" prev2-tag)
                     ("i-1 tag + i-2 tag" prev-tag prev2-tag)
                     ("i-1 tag + i word" prev-tag word)
                     ("i-1 word" prev-word)
                     ("i-1 suf3" (unless (or (member prev-word '(:-START-
                                                                 :-START2-))
                                             (starts-with "!" prev-word))
                                   (if (> (length prev-word) 3)
                                       (substr prev-word -3)
                                       prev-word)))
                     ("i+1 word" next-word)
                     ("i+1 suf3" (unless (or (eql :-END- next-word)
                                             (starts-with "!" next-word))
                                   (if (> (length next-word) 3)
                                       (substr next-word -3)
                                       next-word)))
                     ("i-2 word" prev2-word)
                     ("i+2 word" next2-word))))))

(defmethod train ((model greedy-ap-tagger) sents &key (epochs 5) verbose)
  "Train MODEL with a list of SENTENCE objects SENTS over EPOCHS."
  (with-slots (single-tag-words dict) model
    ;; expand dict
    (dolist (sent sents)
      (dolist (tok (sent-tokens sent))
        (set# (token-word tok) dict nil)))
    ;; expand single-tag-words
    (dotable (word tag (build-single-tag-words-dict (mapcar #'sent-tokens sents)
                                                    :ignore-case? t))
      (unless (in# word single-tag-words)
        (set# word single-tag-words tag)))
    ;; train
    (dotimes (epoch epochs)
       (when verbose
         (format t "~%~%==== Epoch: ~A ====~%~%" (1+ epoch)))
       (let ((c 0) (n 0) (j 0) (prev-j 0) (total (length sents)))
         (dolist (sent sents)
           (let* ((prev :START)
                  (prev2 :START2)
                  (ctx (sent-ctx model sent)))
             (doindex (i token (sent-tokens sent))
               (let ((word (token-word token)))
                 (psetf prev
                        (or (get# word single-tag-words)
                            (let* ((fs (extract-fs model i word ctx prev prev2))
                                   (guess (classify model fs)))
                              (train1 model (rest fs) (token-tag token) guess)
                              guess))
                        prev2 prev)
                 (when verbose
                   (:+ c (if (eql prev (token-tag token)) 1 0))
                   (:+ n)))))
           (:+ j)
           (when (and verbose
                      (> (/ (- j prev-j) total) 0.01))
             (setf prev-j j)
             (format t "~A / ~A = ~5F% - ~2F% ~%"
                     c n (float (* 100 (/ c n)))
                     (* 100 (/ j total)))))
         (unless verbose (princ-progress j total)))
       (:= sents (shuffle sents))))
  model)


;;; normalization

(defmethod normalize ((model greedy-ap-tagger) (word string))
  ;; (cond
  ;;   ((and (find #\- word) (not (char= #\- (char word 0))))
  ;;    "!HYPHEN")
  ;;   ((every #'digit-char-p word)
  ;;    (if (= 4 (length word)) "!YEAR" "!DIGITS"))
  ;;   (t (string-downcase word))))
  (cond-it
    ((re:scan *number-regex* word) (make-string (length word) :initial-element #\0))
    ((re:scan *email-regex* word) "!EMAIL")
    ((re:scan *url-regex* word) "!URL")
    ((in# word (tgr-dict model)) (string-downcase word))
    ((position #\- word :start 1 :from-end t)
     (let ((suffix (slice word (1+ it))))
       (if (in# suffix (tgr-dict model))
           (string-downcase suffix)
           "!HYPH")))
    (t (string-downcase word))))

(defun sent-ctx (tagger sentence)
  (let ((sent (sent-tokens sentence)))
    (make-array (+ 4 (length sent))
                :initial-contents
                (append '(:-START- :-START2-)
                        (mapcar #`(normalize tagger (token-word %))
                                sent)
                        '(:-END- :-END2-)))))

(defmethod extract-gold ((model greedy-ap-tagger) data)
  (let ((len (length data))
        (i 0)
        rez)
    (dolist (sent data)
      (let* ((prev :-START-)
             (prev2 :-START2-)
             (ctx (sent-ctx model sent)))
        (doindex (i token (sent-tokens sent))
          (let ((fs (extract-fs model i (token-word token) ctx prev prev2)))
            (push (pair (token-tag token) fs)
                  rez)
            (:= prev2 prev
                prev (classify model fs)))))
      (princ-progress (:+ i) len))
    (reverse rez)))


;;; loading/saving

(defmethod save-model :after ((model greedy-ap-tagger) &optional zip)
  (zip-as-text-file zip "single-tag-words.txt"
                    (strjoin #\Newline
                             (mapcar #`(fmt "~A	~A" (lt %) (rt %))
                                     (ht->pairs (tgr-single-tag-words model)))))
  (zip-as-text-file zip "dict.txt"
                    (strjoin #\Newline (keys (tgr-dict model)))))

(defmethod load-model :after ((model greedy-ap-tagger) path &key)
  (zip:with-zipfile (zip path)
    (:= (tgr-single-tag-words model)
        (pairs->ht (mapcar #`(ds-bind (word tag) (split #\Tab %)
                               (pair word (intern tag :tag)))
                           (split #\Newline
                                  (zipped-file-data zip "single-tag-words.txt")
                                  :remove-empty-subseqs t))
                   :test 'equalp)
        (tgr-dict model)
        (alist->ht (mapcar #'list
                           (split #\Newline
                                  (zipped-file-data zip "dict.txt")
                                  :remove-empty-subseqs t))
                   :test 'equal))))
