;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutilsx-readtable)


(defclass greedy-ap-dict-postagger (avg-perceptron tagger)
  ((dict :initform #h(equal) :initarg :dict :accessor tagger-dict)
   (single-pos-words :initform #h(equalp) :initarg :single-pos-words
                     :accessor tagger-single-pos-words))
  (:documentation
   "A greedy averaged perceptron tagger with single-pos words dictionary lookup."))

(def-lang-var pos-tagger (load-model (make 'greedy-ap-dict-postagger)
                                     (model-file "pos-tagging/wsj.gz")
                                     :class-package '#:nlp.tags)
  "Pos tagger.")


;;; basic interface

(defmacro with-postagger-init ((tagger sent) &body body)
  `(let ((prev :-start-)
         (prev2 :-start2-)
         (ctx (make-sent-ctx ,tagger ,sent)))
     ,@body))

(defmethod classify ((model greedy-ap-dict-postagger) fs &key classes)
  (declare (ignore classes))
  (or (get# (first fs) (tagger-single-pos-words model))
      (call-next-method model (rest fs))))

(defmethod tag ((tagger greedy-ap-dict-postagger) (sent sent))
  (with-postagger-init (tagger sent)
    (doindex (i token (sent-tokens sent))
      (:= (token-pos token) (classify tagger
                                      (extract-fs tagger i (token-word token)
                                                  ctx prev prev2)))
      (:= prev2 prev
          prev (token-pos token)))
    sent))

(defmethod train ((model greedy-ap-dict-postagger) sents &key (epochs 5) verbose)
  (with-slots (single-pos-words dict) model
    ;; expand dict
    (dolist (sent sents)
      (dolist (tok (sent-tokens sent))
        (set# (token-word tok) dict nil)))
    ;; expand single-pos-words
    (dotable (word pos (build-single-pos-words-dict (mapcar #'sent-tokens sents)
                                                    :ignore-case? t))
      (unless (in# word single-pos-words)
        (set# word single-pos-words pos)))
    ;; train
    (training-perceptron (sent sents epochs verbose c n)
      (with-postagger-init (model sent)
        (doindex (i token (sent-tokens sent))
          (let ((word (token-word token)))
            (:= prev (or (get# word single-pos-words)
                         (let* ((fs (extract-fs model i word ctx prev prev2))
                                (guess (classify model fs)))
                           (train1 model (token-pos token) guess (rest fs))
                           guess))
                prev2 prev)
            (when verbose
              (:+ c (if (eql prev (token-pos token)) 1 0))
              (:+ n))))))
    model))

(defmethod extract-gold ((model greedy-ap-dict-postagger) data)
  (let ((len (length data))
        (j 0)
        rez)
    (dolist (sent data)
      (with-postagger-init (model sent)
        (doindex (i token (sent-tokens sent))
          (let ((fs (extract-fs model i (token-word token) ctx prev prev2)))
            (push (make-sample :fs fs :gold (token-pos token))
                  rez)
            (:= prev2 prev
                prev (classify model fs)))))
      ;; Important: we use the model's prediction on the previous step
      ;; as input for the next step of feature generation,
      ;; because this matches the way it will work in the real world
      (princ-progress (:+ j) len))
    (reverse rez)))

(defmethod extract-fs ((model greedy-ap-dict-postagger) &rest args)
  (ds-bind (i word ctx prev-pos prev2-pos) args
    (let* ((i (+ i 2))
           (dict (tagger-dict model))
           (prev-word  (svref ctx (- i 1)))
           (prev2-word (svref ctx (- i 2)))
           (next-word  (svref ctx (+ i 1)))
           (next2-word (svref ctx (+ i 2)))
           (dword (string-downcase word)))
      (cons word
            (make-fs "bias"
                     ("i pref1" (char word 0))
                     ("i suf3" (if (> (length dword) 3) (substr dword -3) dword))
                     ("i word" word)
                     ("i-1 pos" prev-pos)
                     ("i-2 pos" prev2-pos)
                     ("i-1 pos + i-2 pos" prev-pos prev2-pos)
                     ("i-1 pos + i word" prev-pos word)
                     ("i-1 word" prev-word)
                     ("i-1 suf3" (unless (keywordp prev-word)
                                   (if (> (length prev-word) 3)
                                       (substr prev-word -3)
                                       prev-word)))
                     ("i+1 word" next-word)
                     ("i+1 suf3" (unless (keywordp next-word)
                                   (if (> (length next-word) 3)
                                       (substr next-word -3)
                                       next-word)))
                     ("i-2 word" prev2-word)
                     ("i+2 word" next2-word))))))


;;; normalization

(defmethod normalize ((model tagger) (word string))
  (cond-it
    ((re:scan *number-regex* word) (case (length word)
                                     (1 "0")
                                     (2 "00")
                                     (3 "00h")
                                     (4 "00k")
                                     ((5 6) "0kk")
                                     (7 "00m")
                                     (otherwise "00+")))
    ((re:scan *email-regex* word) "@__")
    ((re:scan *url-regex* word) "@@@")
    #+nil ((in# word (tagger-dict model)) (string-downcase word))
    #+nil ((position #\- word :start 1 :from-end t)
     (let ((suffix (slice word (1+ it))))
       (string-downcase (if (in# suffix (tagger-dict model))
                            suffix
                            word))))
    (t (string-downcase word))))

(defun make-sent-ctx (tagger sent)
  (make-array (+ 4 (length @sent.tokens))
              :initial-contents
              (append '(:-start- :-start2-)
                      (mapcar ^(normalize tagger @%.word) @sent.tokens)
                      '(:-end- :-end2-))))


;;; loading/saving

(defmethod save-model :after ((model greedy-ap-dict-postagger) (out stream))
  (with-slots (single-pos-words dict) model
    (let ((total (+ (ht-count single-pos-words) (ht-count dict)))
          (i 0))
      (format out "~&~A~%" (ht-count single-pos-words))
      (dotable (word pos single-pos-words)
        (format out "~S ~S " word pos)
        (princ-progress (:+ i) total))
      (format out "~%~A~%" (ht-count dict))
      (dotable (word _ dict)
        (format out " ~S" word)
        (princ-progress (:+ i) total)))))

(defmethod load-model :after ((model greedy-ap-dict-postagger) in
                              &key class-package)
  (loop :repeat (read in) :do
    (:= (? @model.single-pos-words (read in))
        (let ((*package* (or class-package *package*)))
          (read in))))
  (loop :repeat (read in) :do
    (:= (? @model.dict (read in)) t)))
