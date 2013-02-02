(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defstruct corpus
  "A structure to cache a unilingual corpus in raw and processed forms."
  name
  lang
  raw-texts
  clean-texts
  text-tokens)

(defstruct token
  "A corpus token with postition and possibly tag."
  beg
  end
  word
  tag)


(defgeneric read-corpus (type file)
  (:documentation
   "Read corpus data of a certain TYPE (a keyword) from file.
    Returns as values:
    - raw text data
    - cleaned-up text
    - list of tokens from the text"))

(defmethod read-corpus-file ((type (eql :brown)) file)
  "Read the Brown corpus."
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

#+nil
(defparameter +brown-corpus+
  (let (raw-texts clean-texts text-tokens)
    (dolist (path (directory (merge-pathnames "corpora/brown/*" +project-root+)))
      (when (= 4 (length (pathname-name path)))
        (mv-bind (raw text tokens) (read-corpus-file :brown path)
          (push raw raw-texts)
          (push text clean-texts)
          (push tokens text-tokens))))
    (make-corpus :name "brown"
                 :lang :en-us
                 :raw-texts (reverse raw-texts)
                 :clean-texts (reverse clean-texts)
                 :text-tokens (reverse text-tokens)))
  "Brown University Standard Corpus of Present-Day American English.")
