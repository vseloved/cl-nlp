;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nltk)
(named-readtables:in-readtable rutils-readtable)


;;; Texts, we'll be working with

(defclass text ()
  ((name :initarg :name)
   (raw :initarg :raw :accessor text-raw)
   (words :accessor text-words)
   (ctxs :accessor text-ctxs)
   (transitions :accessor text-transitions)
   (dispersion :accessor text-dispersion)))

(defmethod print-object ((text text) stream)
  (with-slots (name raw) text
    (let ((len (length raw)))
      (format stream "#<~A ~A... ~A>"
              name (subseq raw 0 (min len 20)) len))))

(defmethod slot-unbound (class (obj text) (slot (eql 'words)))
  (with-slots (raw words) obj
    (format t "~&Tokenizing text...~%")
    (prog1 (setf words (mapcan #`(cons "¶" (tokenize <word-tokenizer> %))
                               (tokenize <paragraph-splitter> raw)))
      (format t "Number of words: ~A~%" (length words)))))

(defmethod slot-unbound (class (obj text) (slot (eql 'ctxs)))
  (with-slots (words ctxs) obj
    (format t "~&Building word contexts...~%")
    (prog1 (setf ctxs (index-context-freqs words))
      (format t "Number of unique words: ~A~%" (length (ht-keys ctxs))))))

(defmethod slot-unbound (class (obj text) (slot (eql 'transitions)))
  (rebuild-transitions obj 2))

(defmethod slot-unbound (class (obj text) (slot (eql 'dispersion)))
  (with-slots (words dispersion) obj
    (format t "~&Building dispersion table...~%")
    (let ((ht (make-hash-table :test 'equal)))
      (doindex (idx word words)
        (set# word ht (cons idx (get# word ht))))
      (prog1 (setf dispersion ht)
        (format t "Number of words: ~A~%" (length (ht-keys dispersion)))))))

(defun nltk-text-file (name)
  (merge-pathnames (fmt "nltk/data/~(~A~).txt" name)
                   nutil:+project-root+))

(defvar *texts* (make-hash-table))

(defun load-nltk-texts (&optional (dir "data/"))
  "Load and cache texts, that we'll use in the examples."
  (dolist (text '(:moby :sense :genesis :inaugural :nps-chat))
    (set# text *texts*
          (make 'text :name text
                :raw (string-trim +white-chars+
                                  (read-file (nltk-text-file text))))))
  (maphash #`(print %%) *texts*))


;;; Main functions

(defun concordance (text word &key (width 25) pass-newlines)
  "Print contexts (up to WIDTH chars) of WORD usage in TEXT.
   If PASS-NEWLINES isn't set, the context will be shown
   up to the closest newline."
  (print-word-in-contexts word (text-raw text)
                          :width 30 :pass-newlines pass-newlines))

(defun similar (text word &key (n 20))
  "Find N most similar words to WORD in TEXT."
  (let* ((ctxs (text-ctxs text))
         (ctx (get# word ctxs))
         (rez (make-hash-table :test 'equal)))
    (maphash #`(let ((common (match-ctxs ctx %%)))
                 (unless (or (string= word %)
                             (zerop (hash-table-count common)))
                   (set# % rez (reduce #'+ (ht-vals common)))))
             ctxs)
    (take n (sorted-ht-keys '> rez))))

(defun common-contexts (text &rest words)
  "Find matching contexts between WORDS in TEXT."
  (with-slots (ctxs) text
    (when-it (reduce #`(match-ctxs (get# %% ctxs) %)
                     (rest words)
                     :initial-value (get# (car words) ctxs))
      (sorted-ht-keys '> it))))

(defun generate (text &key (n 20) (order 2))
  "Generate random text of N words, based on TEXT."
  (with-slots (transitions) text
    (string-trim (append +white-chars+ +newline-chars+)
                 (fmt "~{~A ~}"
                      (generate-text (make 'markov-chain-generator :order order)
                                     (if (= order
                                            (length (car (ht-keys transitions))))
                                         transitions
                                         (rebuild-transitions text order))
                                     n)))))

(defun rebuild-transitions (text n)
  "Rebuild transition index"
  (with-slots (words transitions) text
    (format t "~&Building prefix transitions index of length ~A...~%" n)
    (prog1 (setf transitions (index-prefix-transition-freqs words :n n))
      (format t "Number of prefixes: ~A~%" (length (ht-keys transitions))))))


(defun dispersion-plot (text &rest words)
  "Plot dispersion of WORDS in TEXT."
  (plot-dispersion words (dump-data-for-plot (reverse words)
                                             (text-dispersion text))))

(defun lexical-diversity (text)
  "Calculate lexical diversity measure of TEXT."
  (with-slots (words) text
    (/ (float (length words))
       (hash-table-count (uniq words :raw t)))))

(defun percentage (count total)
  "Return percentage of count / total."
  (/ (* 100.0 count) total))


;;; Helper functions

(defun uniq (list &key raw case-insensitive)
  "Return only unique elements from LIST either as a new list
   or as hash-table if RAW is set. Can be CASE-INSENSITIVE."
  (let ((uniqs (make-hash-table :test (if case-insensitive 'equalp 'equal))))
    (dolist (elt list)
      (set# elt uniqs t))
    (if raw uniqs (ht-keys uniqs))))

(defun sorted-ht-keys (test ht)
  "Return hash-table keys of HT in sorted order accroding to TEST."
  (sort (ht-keys ht) test :key #`(get# % ht)))

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

(defun dump-data-for-plot (words dispersion-table)
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


;;; Brown corpus information

#+manually (
(format t "       Genre        |  Tokens  |  Types  |  Lexical Diversity")
(format t "~%--------------------+----------+---------+--------------------")
(maphash #`(with-accessors ((tokens (corpus-text-tokens +brow-corpus))) %%
             (let* ((words (mapcar #'ncorp::token-word (flatten tokens)))
                    (words-count (length words))
                    (types-count (hash-table-count (uniq words :raw t))))
               (format t "~&~20A|  ~7A |  ~6A | ~6,1F~%"
                       (subseq (symbol-name %)
                               0 (min 19 (length (symbol-name %))))
                       words-count
                       types-count
                       (/ (* 1.0 words-count) types-count))))
         +brown-corpus+)
)