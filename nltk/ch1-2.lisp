;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nltk)
(named-readtables:in-readtable rutils-readtable)

(defclass text ()
  ((name :initarg :name)
   (raw :initarg :raw :accessor text-raw)
   (words :accessor text-words)
   (ctxs :accessor text-ctxs)
   (transitions :accessor text-transitions)
   (dispersion :accessor text-dispersion)
   (ugrams :accessor text-ugrams)
   (bigrams :accessor text-bigrams)
   (trigrams :accessor text-trigrams)))

(defmethod slot-unbound (class (obj text) (slot (eql 'ugrams)))
  (with-slots (words ugrams) obj
    (format t "~&Indexing unigrams...~%")
    (prog1 (setf ugrams (index-ngrams 1 words))
      (format t "Number of ugrams: ~A~%" (ngrams-count ugrams)))))

(defmethod slot-unbound (class (obj text) (slot (eql 'bigrams)))
  (with-slots (words bigrams) obj
    (format t "~&Indexing bigrams...~%")
    (prog1 (setf bigrams (index-ngrams 2 words))
      (format t "Number of bigrams: ~A~%" (ngrams-count bigrams)))))

(defmethod slot-unbound (class (obj text) (slot (eql 'trigrams)))
  (with-slots (words trigrams) obj
    (format t "~&Indexing trigrams...~%")
    (prog1 (setf trigrams (index-ngrams 3 words))
      (format t "Number of trigrams: ~A~%" (ngrams-count trigrams)))))

(defun collocations (text)
  (find-collocations (text-bigrams text) :n 30))

(defun generate (text &key (n 20) (order 2))
  "Generate random text of N words, based on TEXT."
  (with-slots (transitions) text
    (string-trim (append +white-chars+ +newline-chars+)
                 (fmt "~{~A ~}"
                      (generate-text (make 'markov-chain-generator :order order)
                                     (make-lm 'stupid-backoff-lm
                                              :1g (text-ugrams text)
                                              :2g (when (> order 1)
                                                    (text-bigrams text))
                                              :3g (when (> order 2)
                                                    (text-trigrams text)))
                                     n)))))

;; Plotting

(defun dump-counts (ngrams n order-by cumulative)
  "Dump N NGRAMS counts (or CUMULATIVE counts) orderd by ORDER-BY."
  (let ((filename (fmt "/tmp/~A" (gensym)))
        (total 0))
    (with-out-file (out filename)
      (doindex (i pair (ngrams-pairs ngrams :order-by order-by))
        (when (and n (> i n))
          (return))
        (format out "~A~t~S~t~A~%" (1+ i) (car pair)
                (if cumulative
                    (incf total (cdr pair))
                    (cdr pair))))
    filename)))

(defun plot (ngrams &key n (order-by '>) cumulative)
  "Plot NGRAMS counts."
  (cgn:with-gnuplot (t)
    (cgn:format-gnuplot "set xtics rotate 90")
    (cgn:format-gnuplot "set ylabel \"~@[Cumulative ~]Counts\"" cumulative)
    (cgn:format-gnuplot
     "plot \"~A\" using 1:3:xtic(2) with lines title \"\""
     (dump-cumulative-counts ngrams n order-by cumulative))))