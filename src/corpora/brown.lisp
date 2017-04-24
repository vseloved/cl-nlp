;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutilsx-readtable)


(defmethod read-corpus-file ((type (eql :brown)) file &key)
  "Read individual file from the Brown corpus."
  (let ((text (string-trim +white-chars+ (read-file file)))
        (offset 0)
        (nl-nl-nl (fmt "~%~%~%"))
        parag-sent-toks)
    (dolist (parag (split nl-nl-nl text :test 'string= :remove-empty-subseqs t))
      (let (cur-parag)
        (dolist (sent (split #\Newline parag :remove-empty-subseqs t))
          (let (cur-sent)
            (loop :for (beg end) :on (re:all-matches "[^\\s]+" text) :by #'cddr :do
               (let* ((split (position #\/ text :start beg :end end :from-end t))
                      (word (slice text beg split))
                      (pos (slice text (1+ split) end)))
                 (when (member word '("``" "''") :test #'string=)
                   (:= word "\""
                       end (1- end)))
                 (push (make-tok :beg (- beg offset)
                                 :end (- end (:+ offset (1+ (length pos))))
                                 :word word :pos pos)
                       cur-sent)))
            (push (reverse cur-sent) cur-parag)))
        (push (reverse cur-parag) parag-sent-toks)))
    (reversef parag-sent-toks)
    (make-text :name (pathname-name file)
               :raw text
               :clean (parags->text parag-sent-toks)
               :parag-sent-toks parag-sent-toks)))

(defmethod read-corpus ((type (eql :brown)) path &key (ext ""))
  (with ((path (namestring path))
         (topic-mapping #h(#\a :press-reportage
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
                           #\r :humor))
         (rez (make-corpus :desc "Brown Corpus" :groups #h())))
    (dofiles (file path :ext ext)
      (when (= 4 (length (pathname-name file)))
        (let ((text (read-corpus-file :brown file)))
          (push text @rez.texts)
          (push text (? @rez.groups (? topic-mapping
                                       (char (pathname-name file) 1))))))
      (reversef @rez.texts)
      (dotable (topic text-group @rez.groups)
        (:= (? groups topic) (reverse text-group))))
    rez))

(defmethod map-corpus ((type (eql :brown)) path fn &key (ext ""))
  (dofiles (file (directory path) :ext ext)
    (when (= 4 (length (pathname-name file)))
      (call fn (read-corpus-file :brown file)))))
