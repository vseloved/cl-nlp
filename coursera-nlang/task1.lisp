;;; (c) 2013 Vsevolod Dyomkin

(cl:in-package #:cnlang)
(named-readtables:in-readtable rutils-readtable)


(defparameter *tags* '("O" "I-GENE"))
(defparameter +rare-word+ "_RARE_")
(defparameter +numeric+ "_NUMERIC_")
(defparameter +all-caps+ "_ALL_CAPS_")
(defparameter +last-cap+ "_LAST_CAP_")
(setf +stop-tag+ "STOP")

(defun word->class (word rare)
  (if (eql :multi rare)
      (cond
        ((some #'digit-char-p word) +numeric+)
        ((re:scan "^[A-Z]+$" word) +all-caps+)
        ((re:scan "[A-Z]$" word) +last-cap+)
        (t +rare-word+))
      +rare-word+))

(defun read-freqs (file &key rare)
  "Read frequencies from FILE.
   Iff RARE substitute low-frequency words (count < 5) with _RARE_."
  (let ((tfs1 #{equal})
        (tfs2 #{equal})
        (tfs3 #{equal})
        (efs #{equal})
        (counts #{equal}))
    (dolines (line file)
      (ds-bind (freq type &rest parts) (split #\Space line)
        (setf freq (parse-integer freq))
        (ecase (mkeyw type)
          (:wordtag (set# parts efs freq))
          (:1-gram (set# parts tfs1 freq))
          (:2-gram (set# (substitute nil "*" parts :test 'string=) tfs2 freq))
          (:3-gram (set# (substitute nil "*" parts :test 'string=) tfs3 freq)))))
    (when rare
      (dotable (2gram freq efs)
        (incf (get# (second 2gram) counts 0) freq))
      (dotable (word count counts)
        (when (< count 5)
          (dolist (tag *tags*)
            (when-it (get# (list tag word) efs)
              (rem# (list tag word) efs)
              (incf (get# (list tag (word->class word rare))
                          efs 0)
                    it))))))
    (list efs tfs1 tfs2 tfs3)))

(defun read-hmm (file &key (rare t)
                           (transition-lm-class 'plain-lm)
                           (emission-lm-class 'plain-lm))
  "Create HMM from frequencies from FILE."
  (ds-bind (efs tfs1 tfs2 tfs3) (read-freqs file :rare rare)
    (make 'hmm-tagger :order 3 :tags *tags*
          :transition-lm (make-lm transition-lm-class
                                  :1g (make 'table-ngrams :order 1
                                            :table tfs1)
                                  :2g (make 'table-ngrams :order 2
                                            :table tfs2)
                                  :3g (make 'table-ngrams :order 2
                                            :table tfs3))
          :emission-lm (make-lm emission-lm-class
                                :1g (make 'table-ngrams :order 1
                                          :table tfs1)
                                :2g (make 'table-ngrams :order 2
                                          :table efs)))))

(defun part1 (infile outfile)
  (with-out-file (out outfile)
    (let ((hmm (read-hmm "/mnt/ext4/nlp/task/gene.counts")))
      (flet ((cprobs (word)
               (sort (mapcar #`(cons %
                                     (cond-prob (hmm-emission-lm hmm)
                                                (list % word)))
                             (model-tags hmm))
                     '> :key 'cdr)))
        (let ((default-tag (caar (cprobs +rare-word+)))
              words
              end)
          (dolines (line infile)
            (unless (setf end (blankp line))
              (push line words))
            (when (and end words)
              (dolist (word (reverse words))
                (format out "~A ~A~%" word
                        (let ((probs (cprobs word)))
                          (if (every #'zerop (mapcar #'cdr probs))
                              default-tag
                              (caar (cprobs word))))))
              (terpri out)
              (setf words nil)))
          (terpri out))))))

(defclass rare-words-lm (language-model)
  ((rare :initform t :initarg :rare :accessor lm-rare))
  (:documentation
   "Language model with a special class of rare words."))

(defmethod cond-prob ((model rare-words-lm) (ngram list))
  (let ((rez (call-next-method)))
    (if (zerop rez)
        (cond-prob model (append (butlast ngram)
                                 (list (word->class (last1 ngram)
                                                    (lm-rare model)))))
        rez)))

(defun part2 (infile outfile)
  (hmm-tag infile outfile :rare t))

(defun part3 (infile outfile)
  (hmm-tag infile outfile :rare :multi))

(defun hmm-tag (infile outfile &key rare hmm)
  (with-out-file (out outfile)
    (let* ((hmm (or hmm (read-hmm "/mnt/ext4/nlp/task1/gene.counts" :rare rare
                                  :emission-lm-class 'plain-lm)))
           (all-words #{equal})
           words
           end)
      (maphash #`(set# (second %) all-words t)
               (ngrams-table (elt (model-ngrams (hmm-emission-lm hmm)) 2)))
      (dolines (line infile)
        (unless (setf end (blankp line))
          (push line words))
        (when (and end words)
          (loop :for word :in (reverse words)
                :and tag :in (tag hmm (mapcar #`(if (get# % all-words)
                                                    %
                                                    (word->class % rare))
                                              (reverse words)))
                :do
             (format out "~A ~A~%" word tag))
          (terpri out)
          (setf words nil)))
      (terpri out))))

