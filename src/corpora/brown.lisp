;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defmethod read-corpus ((type (eql :brown)) path)
  (let* ((topic-mapping #{#\a :press-reportage
                          #\b :press-editorial
                          #\c :press-reviews
                          #\d :religion
                          #\e :skill-and-hobbies
                          #\f :popular-lore
                          #\g :belles-lettres
                          #\h :miscellaneous-government-house-organs
                          #\j :learned
                          #\k :fiction-general
                          #\l :fiction-mystery
                          #\m :fiction-science
                          #\n :fiction-adventure
                          #\p :fiction-romance
                          #\r :humor
                         })
         (rez (make-corpus :name (fmt "Brown Corpus - ~A" topic)
                           :groups #{})))
    (dotable (_ topic topic-mapping)
      (set# topic (corpus-groups rez) ()))
    (dolist (file (directory path))
      (when (= 4 (length (pathname-name file)))
        (mv-bind (raw clean tokens) (read-corpus-file :brown file)
        (let ((text (make-text :name (filename-name file)
                               :raw raw :clean clean :tokens tokens)))
          (push text (corpus-texts rez))
          (push text (get# (get# (char (pathname-name file) 1) topic-mapping)
                           (corpus-groups rez)))))))))

(defmethod read-corpus-file ((type (eql :brown)) file)
  "Read individual file from the Brown corpus."
  (let ((text (string-trim +white-chars+ (read-file file)))
        (offset 0)
        tokens)
    (loop :for (beg end) :on (re:all-matches "[^\\s]+" text) :by #'cddr :do
       (let* ((tok (subseq text beg end))
              (/-pos (position #\/ tok :from-end t))
              (word (subseq tok 0 /-pos))
              (tag (subseq tok (1+ /-pos))))
         (when (member word '("``" "''") :test #'string=)
           (setf word "\""
                 end (1- end)))
         (push (make-token :beg (- beg offset)
                           :end (- end (incf offset (1+ (length tag))))
                           :word word :tag tag)
               tokens)))
    (values text
            (let ((clean (make-string (token-end (first tokens))
                                      :initial-element #\Space)))
              (dolist (token (reverse tokens) clean)
                (with-slots (beg end word) token
                  (setf (subseq clean beg end) word))))
            (reverse tokens))))