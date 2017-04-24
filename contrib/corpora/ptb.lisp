;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct (ptb-tagged-text (:include text))
  "A single text from the tagged section of the Penn Treebank corpus."
  nps)


(defmethod read-corpus-file ((type (eql :ptb-tagged)) file &key)
  (let ((*package* (find-package :tag))
        pars nps)
    (dolist (par (remove-if
                   ^(starts-with "*x*" %) ; remove headers from Brown corpus files
                   (mapcar (lambda (par)
                             (mapcar ^(strjoin #\Space %)
                                     par))
                           (mapcar ^(split-if 'blankp
                                              (split #\Newline %))
                                   (remove-if 'blankp
                                              (mapcar
                                               ^(string-trim +white-chars+ %)
                                               (re:split "={38}"
                                                         (read-file file))))))))
      (let (cur-parag cur-parag-nps)
        (dolist (sent par)
          ;; account for tokens that have square brackets in them 1/2
          (:= sent (re:regex-replace-all "(\\w)\\]" sent "\\1}}"))
          (let (cur-sent cur-nps)
            (dolist (phrs (split #\] sent :remove-empty-subseqs t))
              (let (cur-np)
                (dolist (tok (split #\Space phrs :remove-empty-subseqs t))
                  (unless (string= "[" tok)
                    ;; account for tokens that have square brackets in them 2/2
                    (:= tok (re:regex-replace-all "(\\w)}}" tok "\\1]"))
                    (:= tok (make-tagged-token tok))
                    (push tok cur-np)
                    (push tok cur-sent)))
                (push (reverse cur-np) cur-nps)))
            (push (reverse cur-nps) cur-parag-nps)
            (push (reverse cur-sent) cur-parag)))
        (push (reverse cur-parag-nps) nps)
        (push (reverse cur-parag) pars)))
    (reversef pars)
    (make-ptb-tagged-text :name (pathname-name file)
                          :clean (parags->text pars)
                          :tokenized pars
                          :nps (reverse nps))))

(defmethod read-corpus ((type (eql :ptb-tagged)) path &key (ext "POS"))
  (let ((rez (make-corpus :desc "Penn Treebank Tagged")))
    (dofiles (file path :ext ext)
      (push (read-corpus-file :ptb-tagged path)
            @rez.texts))
    rez))

(defmethod map-corpus ((type (eql :ptb-tagged)) path fn &key (ext "POS"))
  (dofiles (file path :ext ext)
    (call fn (read-corpus-file :ptb-tagged file))))


;;; util

(defun make-tagged-token (str)
  (let ((/-pos (position #\/ str :from-end t)))
    (make-tok :word (slice str 0 /-pos)
              :pos (mksym (slice str (1+ /-pos))))))
