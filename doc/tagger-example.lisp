;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.tagging)
(named-readtables:in-readtable rutils-readtable)


(defpar *train* ())
(defpar *dev* ())
(defpar *test* ())

(defun extract-sents (text)
  (mapcar #`(make 'sentence :tokens (ncorp:remove-dummy-tokens %))
          (text-tokens text)))

(map-corpus :treebank "~/ext4/ontonotes/nw/wsj/"
            #`(when (< (parse-integer (substr (text-name %) -4))
                       1900)
                (appendf *train* (extract-sents %)))
            :ext "parse")

;; (map-corpus :treebank "~/ext4/ontonotes/nw/wsj/"
;;             #`(when (<= 1900
;;                         (parse-integer (substr (text-name %) -4))
;;                         (1- 2200))
;;                 (appendf *dev* (mapcar #'ncorp:remove-dummy-tokens
;;                                        (text-tokens %))))
;;             :ext "parse")

(map-corpus :treebank "~/ext4/ontonotes/nw/wsj/"
            #`(when (>= (parse-integer (substr (text-name %) -4))
                        2200)
                (appendf *test* (extract-sents %)))
            :ext "parse")

(defpar *tagger* (make 'greedy-ap-tagger))
(train *tagger* *train* :verbose t :epochs 5)
(defpar *gold-test* (extract-gold *tagger* *test*))
(accuracy *tagger* *gold-test*)

(save-model *tagger* "/tmp/m1.zip")

;(defpar *gold-train* (extract-gold *tagger* *train*))
;(defpar *gold-dev* (extract-gold *tagger* *dev*))

(defun process-ptb-tagged (path &optional (beg 0) (end 24))
  (let (rez)
    (fad:walk-directory
     path
     #`(when (and (string= "POS" (pathname-type %))
                  (<= beg (parse-integer (slice (pathname-name %) 4 6)) end))
         (princ ".")
         (let (s)
           (dolines (line %)
             (if (starts-with "=====" line)
                 (when s
                   (push (reverse s) rez)
                   (void s))
                 (dolist (word (split #\Space line))
                   (when-it (position #\/ word :from-end t)
                     (push (make-token :word (slice word 0 it)
                                       :tag (mksym (slice word (1+ it)
                                                          (position #\/ word
                                                                    :start (1+ it)))
                                                   :package :tag))
                           s)))))
           (when s
             (push (reverse s) rez)))))
    (reverse rez)))

(defpar *train* (process-ptb-tagged "~/ext4/ptb/TAGGED/POS/WSJ/" 0 18))
(defpar *test* (process-ptb-tagged "~/ext4/ptb/TAGGED/POS/WSJ/" 22 24))


#+nil  ; compare taggers
(let ((sents *test*)
      (fs (reverse (mapcar #'rt *gold-test*)))
      (matched 0) (total 0))
  (dolines (line "/tmp/out")
    (do ((toks (car sents) (cdr toks))
         (tags (split #\Space line :remove-empty-subseqs t) (cdr tags)))
        ((null toks))
      (:+ total)
      (if (or (member (token-tag (car toks)) '(tag:|)| tag:|(|))
              (string= (car tags)
                       (symbol-name (classify *tagger* (car fs)))))
          (:+ matched)
          (print (list (car toks)
                       (car tags)
                       (classify *tagger* (car fs)))))
      (:= fs (cdr fs)))
    (:= sents (cdr sents)))
  (* 100.0 (/ matched total)))
