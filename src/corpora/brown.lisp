;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


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
            (let ((clean-text (make-string (token-end (first tokens))
                                           :initial-element #\Space)))
              (dolist (token (reverse tokens) clean-text)
                (with-slots (beg end word) token
                  (setf (subseq clean-text beg end) word))))
            (reverse tokens))))

#+manually
(defparameter +brown-corpus+
  (let ((brown-topics-mapping #{#\a :press-reportage
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
        (raw-texts #{})
        (clean-texts #{})
        (text-tokens #{})
        (corpus #{}))
    (dolist (path (directory (merge-pathnames "corpora/brown/*" +project-root+)))
      (when (= 4 (length (pathname-name path)))
        (let ((topic (get# (char (pathname-name path) 1) brown-topics-mapping)))
          (unless (get# topic raw-texts)
            (set# topic raw-texts '(nil))
            (set# topic clean-texts '(nil))
            (set# topic text-tokens '(nil)))
          (mv-bind (raw text tokens) (read-corpus-file :brown path)
            (push raw (get# topic raw-texts))
            (push text (get# topic clean-texts))
            (push tokens (get# topic text-tokens))))))
    (dolist (topic (ht-keys raw-texts))
      (set# topic corpus
            (make-corpus
             :name (fmt "Brown Corpus - ~A" topic)
             :lang :en-us
             :raw-texts (rest (reverse (get# topic raw-texts)))
             :clean-texts (rest (reverse (get# topic clean-texts)))
             :text-tokens (rest (reverse (get# topic text-tokens))))))
    corpus)
  "Brown University Standard Corpus of Present-Day American English.")