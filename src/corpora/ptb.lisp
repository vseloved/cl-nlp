;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct (ptb-tagged-text (:include text))
  "A single text from the tagged section of the Penn Treebank corpus."
  nps)

(defun make-tagged-token (str)
  (let ((/-pos (position #\/ str :from-end t)))
    (make-token :word (slice str 0 /-pos)
                :tag (mksym (slice str (1+ /-pos))))))

(defmethod read-corpus-file ((type (eql :ptb-tagged)) file)
  (let ((*package* (find-package :nlp.tags))
        (sents (remove-if
                #`(starts-with "*x*" %)  ; remove headers from Brown corpus files
                (mapcar #`(strjoin #\Space %)
                        (split-if #'blankp
                                  (mapcar #`(string-trim (cons #\= +white-chars+)
                                                         %)
                                          (split #\Newline (read-file file)))
                                  :remove-empty-subseqs t))))
        nps)
    (dolist (sent sents)
      ;; account for tokens that have square brackets in them 1/2
      (setf sent (re:regex-replace-all "(\\w)\\]" sent "\\1}}"))
      (dolist (phrs (split #\] sent :remove-empty-subseqs t))
        (let (cur)
          (dolist (tok (split #\Space phrs :remove-empty-subseqs t))
            (unless (string= "[" tok)
              ;; account for tokens that have square brackets in them 2/2
              (setf tok (re:regex-replace-all "(\\w)}}" sent "\\1]"))
              (push (make-tagged-token tok) cur)))
          (push (reverse cur) nps))))
    (reversef nps)
    (let ((tokens (mapcar #'flatten nps)))
      (values (strjoin #\Newline sents)
              (strjoin #\Newline
                       (mapcar #`(strjoin #\Space (mapcar #'token-word %))
                               tokens))
              tokens
              nps))))

(defmethod read-corpus ((type (eql :ptb-tagged)) path &key ext)
  (let ((rez (make-corpus :desc "Penn Treebank Tagged")))
    (walk-corpus-dir
     path ext
     #`(mv-bind (raw clean tokens nps) (read-corpus-file :ptb-tagged %)
         (push (make-ptb-tagged-text :name (pathname-name %)
                                     :raw raw :clean clean
                                     :tokens tokens :nps nps)
               (corpus-texts rez))))
    rez))

(defmethod map-corpus ((type (eql :ptb-tagged)) path fn &key ext)
  (walk-corpus-dir
   path ext
   #`(mv-bind (raw clean tokens nps) (read-corpus-file :ptb-tagged %)
       (format *debug-io* ".")
       (funcall fn (make-ptb-tagged-text :name (pathname-name %)
                                         :raw raw :clean clean
                                         :tokens tokens :nps nps)))))
