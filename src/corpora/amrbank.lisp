;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defmethod read-corpus-file ((type (eql :amrbank)) file &key)
  (flet ((post-process (sent amr)
           (pair (tokenize <word-chunker> (re:regex-replace-all
                                           " - ([^-])" sent "-\\1"))
                 (read-from-string
                  (strjoin " " (mapcar #`(re:regex-replace-all
                                          "/ ([^(\"|\\s)]+)" % "\"\\1\"")
                                       (reverse amr)))))))
    (let (rez sent amr in-amr)
      (dolines (line file)
        (cond
          ((blankp (string-trim +white-chars+ line))
           (void in-amr)
           (when sent
             (push (post-process sent amr)
                   rez)))
          ((char= #\( (char line 0))
           (:= in-amr t
               amr (list line)))
          ((starts-with "# ::snt" line)
           (:= sent (slice line #.(1+ (length "# ::snt")))))
          (in-amr
           (push line amr))))
      (when in-amr
        (push (post-process sent amr)
              rez))
      (reversef rez)
      (make-text :name (pathname-name file)
                 :tokenized (mapcar #'lt rez)
                 :clean (mapcar #'rt rez)))))

(defmethod read-corpus ((type (eql :amrbank)) path &key ext)
  (let ((rez (make-corpus :desc "Treebank")))
    (if (uiop:directory-pathname-p path)
        (dofiles (file path :ext ext)
          (push (read-corpus-file :amrbank file) (corpus-texts rez)))
        (push (read-corpus-file :amrbank path) (corpus-texts rez)))
    rez))

(defmethod map-corpus ((type (eql :amrbank)) path fn &key ext)
  (dofiles (file path :ext ext)
    (funcall fn (read-corpus-file :amrbank file))))
