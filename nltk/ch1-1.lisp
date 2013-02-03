;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nltk)
(named-readtables:in-readtable rutils-readtable)


;;; Basic tools to load and cache texts, we'll be working with

(defstruct (text (:print-function print-text))
  name
  raw
  words
  ctxs
  transitions
  dispersion)

(defun print-text (text stream depth)
  (with-slots (name raw) text
    (let ((len (length raw)))
      (format stream "#<~A ~A ... ~A>"
              name (subseq raw 0 (min len 20)) len))))

(defun nltk-text-file (name)
  (merge-pathnames (fmt "nltk/data/~(~A~).txt" name)
                   +project-root+))

(defvar *texts* (make-hash-table))

(defun load-nltk-texts ()
  "Load and cache texts, that we'll use in the examples."
  (dolist (text '(:moby :sense :genesis :inaugural))
    (set# text *texts*
          (make-text :name text
                     :raw (string-trim +white-chars+
                                       (read-file (nltk-text-file text))))))
  (maphash #`(print %%) *texts*))


;;; Main functions

(defun concordance (word text &key (width 25) pass-newlines)
  "Print contexts (up to WIDTH chars) of WORD usage in TEXT.
   If PASS-NEWLINES isn't set, the context will be shown
   up to the closest newline."
  (declare (type (integer 0) width))
  (loop :for (s e) :on (re:all-matches (fmt "\\b~A\\b" word) text) :by #'cddr :do
     (let ((s- (- s width))
           (e+ (+ e width)))
       (if pass-newlines
           (format t "~A~%"
                   (substitute-if #\Space #'white-char-p (subseq text s- e+)))
           (let ((l-pos (max s- (1+ (position #\Newline text :end s :from-end t))))
                 (r-pos (min e+ (1- (position #\Newline text :start e)))))
             (format t "~@[~A~]~A~@[~A~]~%"
                     (unless (= s- l-pos) (filler (- l-pos s-)))
                     (subseq text l-pos r-pos)
                     (unless (= e+ r-pos) (filler (- e+ r-pos)))))))))

(defun similar (word text &key (n 20))
  "Find N most similar words to WORD in TEXT."
  (maybe-index-contexts text)
  (let* ((ctxs (text-ctxs text))
         (ctx (get# word ctxs))
         (rez (make-hash-table :test 'equal)))
    (maphash #`(let ((common (match-ctxs ctx %%)))
                 (unless (or (string= word %)
                             (zerop (hash-table-count common)))
;                   (set# % rez (hash-table-count common))))
                   (set# % rez (reduce #'+ (ht-vals common)))))
             ctxs)
    (take n (sorted-ht-keys rez '>))))

(defun common-contexts (word1 word2 text &key (n 20))
  "Find N most similar contexts between WORD1 and WORD2 in TEXT."
  (maybe-index-contexts text)
  (take n (sorted-ht-keys (match-contexts word1 word2 text) '>)))

(defun generate (text)
  "Generate random text of 20 words, based on TEXT."
  (maybe-index-transitions text)
  (generate-text <mark-v-shaney> (text-transitions text) 20))

(defun dispersion-plot (text &rest words)
  "Plot dispersion of WORDS in TEXT."
  (maybe-index-dispersion text)
  (plot-dispersion words (dump-data words (text-dispersion text))))


;;; Helper functions

(defun sorted-ht-keys (ht test)
  "Return hash-table keys of HT in sorted order accroding to TEST."
  (sort (ht-keys ht) test :key #`(get# % ht)))

(defun match-contexts (word1 word2 text)
  "Find the intersection between contexts of WORD1 and WORD2 in TEXT
   and for each common context calculate the commonality weight."
  (maybe-index-contexts text)
  (match-ctxs (get# word1 (text-ctxs text))
              (get# word2 (text-ctxs text))))

(defun match-ctxs (word1-ctx word2-ctx)
  "Find the intersection between WORD1-CTX and WORD2-CTX tables
   and for each common context calculate the commonality weight."
  (let ((rez (make-hash-table :test 'equal)))
    (when (and word1-ctx word2-ctx)
      (dolist (k (intersection (ht-keys word1-ctx) (ht-keys word2-ctx)
                               :test 'string=))
        (set# k rez (* (get# k word1-ctx)
                       (get# k word2-ctx)))))
    rez))

(defun maybe-tokenize (text &key preserve-newlines)
  "If TEXT doesn't have WORDS, build them and store in TEXT."
  (with-slots (raw words) text
    (unless words
      (format t "~&Tokenizing text...~%")
      (setf words (cons "¶" (tokenize
                             <word-tokenizer>
                             (let ((text (substitute #\¶ #\Newline raw)))
                               (re:regex-replace-all
                                "¶(\\s*¶)*"
                                (if preserve-newlines
                                    text
                                    (re:regex-replace-all "¶([^¶])" text " \\1"))
                                " ¶ ")))))
      (format t "Number of words: ~A~%" (length words)))))

(defun maybe-index-contexts (text)
  "If TEXT doesn't have CTXS, build them and store in TEXT."
  (with-slots (words ctxs) text
    (unless ctxs
      (maybe-tokenize text)
      (format t "~&Building word contexts...~%")
      (setf ctxs (index-context-freqs words))
      (format t "Number of words: ~A~%" (length (ht-keys ctxs))))))

(defun maybe-index-transitions (text)
  "If TEXT doesn't have TRANSITIONS, build them and store in TEXT."
  (with-slots (words transitions) text
    (unless transitions
      (maybe-tokenize text)
      (format t "~&Building word transitions index...~%")
      (setf transitions (index-word-transition-freqs words))
      (format t "Number of words: ~A~%" (length (ht-keys transitions))))))

(defun maybe-index-dispersion (text)
  "If TEXT doesn't have DISPERSION table, build it and store in TEXT."
  (with-slots (words dispersion) text
    (unless dispersion
      (maybe-tokenize text)
      (format t "~&Building dispersion table...~%")
      (let ((ht (make-hash-table :test 'equal)))
        (doindex (idx token words)
          (set# token ht (cons idx (get# token ht))))
        (setf dispersion ht))
      (format t "Number of words: ~A~%" (length (ht-keys dispersion))))))


(defun dump-data (words dispersion-table)
  "Dump data from DISPERSION-TABLE for WORDS into a temporary file
   and return its name."
  (let ((filename (fmt "/tmp/~A" (gensym))))
    (with-out-file (out filename)
      (format out "0~t0~t~%")
      (doindex (i word words)
        (dolist (idx (get# word dispersion-table))
          (format out "~A~t~A~t~A~%" idx (1+ i) word)))
      (format out "0~t~A~t~%" (1+ (length words))))
    filename))

(defun plot-dispersion (words file)
  "Plot WORDS dispersion data from FILE."
  (cgn:start-gnuplot)
  (cgn:format-gnuplot "set title \"Lexical Dispersion Plot\"")
  (cgn:format-gnuplot "plot \"~A\" using 1:2:yticlabels(3) title \"\"" file))