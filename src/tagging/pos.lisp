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
  (or (get# (first fs) @model.single-pos-words)
      (call-next-method model (rest fs))))

(defmethod tag ((tagger greedy-ap-dict-postagger) (sent sent))
  (with-postagger-init (tagger sent)
    (doindex (i tok @sent.toks)
      (:= @tok.pos (classify tagger
                             (extract-fs tagger i @tok.word ctx prev prev2)))
      (:= prev2 prev
          prev @tok.pos))
    sent))

(defmethod train ((model greedy-ap-dict-postagger) sents &key (epochs 5) verbose)
  ;; expand dict
  (dolist (sent sents)
    (dolist (tok @sent.toks)
      (void (? @model.dict @tok.word)))
    ;; expand single-pos-words
    (dotable (word pos (build-single-pos-words-dict (mapcar #'sent-tokens sents)
                                                    :ignore-case? t))
      (unless (in# word @model.single-pos-words)
        (set# word @model.single-pos-words pos))))
  ;; train
  (training-perceptron (sent sents epochs verbose c n)
    (with-postagger-init (model sent)
      (doindex (i tok @sent.toks)
        (:= prev (or (? @model.single-pos-words @tok.word)
                     (with ((fs (extract-fs model i @tok.word ctx prev prev2))
                            (guess (classify model fs)))
                       (train1 model @tok.pos guess (rest fs))
                       guess))
            prev2 prev)
        (when verbose
          (:+ c (if (eql prev @tok.pos) 1 0))
          (:+ n)))))
  model)

(defmethod extract-gold ((model greedy-ap-dict-postagger) data)
  (let ((len (length data))
        (j 0)
        rez)
    (dolist (sent data)
      (with-postagger-init (model sent)
        (doindex (i tok @sent.toks)
          (let ((fs (extract-fs model i @tok.word ctx prev prev2)))
            (push (make-ex :fs fs :gold @tok.pos)
                  rez)
            (:= prev2 prev
                prev (classify model fs)))))
      ;; Important: we use the model's prediction on the previous step
      ;; as input for the next step of feature generation,
      ;; because this matches the way it will work in the real world
      (princ-progress (:+ j) len))
    (reverse rez)))

(defmethod extract-fs ((model greedy-ap-dict-postagger) &rest args)
  (with (((i word ctx prev-pos prev2-pos) args)
         (i (+ i 2))
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
                   ("i+2 word" next2-word)))))

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
  (make-array (+ 4 (length @sent.toks))
              :initial-contents
              (append '(:-start- :-start2-)
                      (mapcar ^(normalize tagger @%.word) @sent.toks)
                      '(:-end- :-end2-))))


;;; loading/saving

(defmethod save-model :after ((model greedy-ap-dict-postagger) (out stream))
  (let ((total (+ (ht-count @model.single-pos-words) (ht-count @model.dict)))
        (i 0))
    (format out "~&~A~%" (ht-count @model.single-pos-words))
    (dotable (word pos @model.single-pos-words)
      (format out "~S ~S " word pos)
      (princ-progress (:+ i) total))
    (format out "~%~A~%" (ht-count @model.dict))
    (dotable (word _ @model.dict)
      (format out " ~S" word)
      (princ-progress (:+ i) total))))

(defmethod load-model :after ((model greedy-ap-dict-postagger) in
                              &key class-package)
  (loop :repeat (read in) :do
    (:= (? @model.single-pos-words (read in))
        (let ((*package* (or class-package *package*)))
          (read in))))
  (loop :repeat (read in) :do
    (:= (? @model.dict (read in)) t)))


;;; utils

(defun build-single-pos-words-dict (sents &key ignore-case?
                                            (freq-threshold 20)
                                            (ambiguity-threshold 0.97))
  (let ((posdict (make-hash-table :test (if ignore-case? 'equalp 'equal))))
    (dolist (sent sents)
      (dolist (tok sent)
        (:+ (get# @tok.pos (getset# @tok.word posdict #h()) 0))))
    (dotable (word pos-weights posdict)
      (with ((argmax max (argmax 'rt (pairs pos-weights)))
             (total (sum 'vals pos-weights)))
        (if (and (> total freq-threshold)
                 (> (/ max total) ambiguity-threshold))
            (set# word posdict (lt argmax))
            (rem# word posdict))))
    posdict))
