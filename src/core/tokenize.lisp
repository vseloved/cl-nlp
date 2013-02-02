(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)


(defgeneric tokenize (tokenizer string)
  (:documentation
   "Tokenize STRING with TOKENIZER. Outputs 2 values:
    - list of words
    - list of spans as beg-end cons pairs"))

;; (defgeneric stream-tokenize (tokenizer input output &optional span-output)
;;   (:documentation
;;    "Tokenize INPUT with TOKENIZER and writes to OUTPUT one word per line.
;;     Also can write to SPAN-OUTPUT beg-end cons pairs related to words.
;;
;;     Usage example:
;;
;;         (let ((pipe (make-two-way-stream
;;                       (make-input-string-stream) (make-output-string-stream))))
;;           (bt:make-thread #`(stream-tokenize *standard-input* pipe))
;;           (loop :for line := (read-line pipe nil) :while line :do
;;              (print line)))
;;    "))


(defclass tokenizer ()
  ((offset :accessor tokenizer-offset :initarg :offset :initform 0))
  (:documentation
   "Base class for tokenizers."))

(defmethod tokenize :around ((tokenizer tokenizer) string)
  "Pre-split text into lines and tokenize each line separately."
  (let ((offset 0)
        words spans)
    (loop :for line :in (split-sequence #\Newline string) :do
       (mv-bind (ts ss) (call-next-method tokenizer line)
         (setf words (nconc words ts)
               spans (nconc spans (mapcar #`(cons (+ (car %) offset)
                                                  (+ (cdr %) offset))
                                          ss)))
         (incf offset (1+ (length line)))))
    (values words
            spans)))


;;; Word tokenization

(defclass regex-word-tokenizer (tokenizer)
  ((regex :accessor tokenizer-regex :initarg :regex
          :initform
          (re:create-scanner
           "(\\w+|[!\"#$%&'*+,./:;<=>?@^`~…\\(\\)⟨⟩{}\\[\\|\\]‒–—―«»“”‘’¶-]+)")
          :documentation
          "A simpler variant would be [^\\s]+
           — it doesn't split punctuation, yet sometimes it's desirable."))
  (:documentation
   "Regex-based word tokenizer."))

(defmethod tokenize ((tokenizer regex-word-tokenizer) string)
  (loop :for (beg end) :on (re:all-matches (tokenizer-regex tokenizer) string)
                       :by #'cddr
     :collect (subseq string beg end) :into wordss
     :collect (cons beg end) :into spans
     :finally (return (values words
                              spans))))

(define-lazy-singleton <word-tokenizer> (make 'regex-word-tokenizer)
  "Basic word tokenizer.")

(define-lazy-singleton <word-chunker>
    (make 'regex-word-tokenizer :regex (re:create-scanner "[^\\s]+"))
  "Dumb word tokenizer, that will not split punctuation from words.")


;;; Sentence splitting

(defclass baseline-sentence-tokenizer (tokenizer)
  ()
  (:documentation
   "Basic tokenizer for sentence splitting."))

(defparameter +abbrevs-with-dot+
  (list-from-file (data-file "abbrevs-with-dot.txt"))
  "Widely-used abbreviations ending in dot.")

(defmethod tokenize ((tokenizer baseline-sentence-tokenizer) string)
  (mv-bind (words word-spans)
      (tokenize (make 'regex-word-tokenizer :regex "[^\\s]+")
                (substitue #\¶ #\Newline string))
    (let ((beg 0)
          sentences spans)
      (loop :for ws :on words :and ss :on word-spans :do
         (let ((word (first ts))
               (span (first ss)))
           (when (or (null (rest ts))
                     (and (member (char word (1- (length word)))
                                  '(#\. #\? #\! #\¶))
                          (not (member word +abbrevs-with-dot+
                                       :test #'string-equal))
                          (and-it (second ts)
                                  (upper-case-p (char it 0)))))
             (push (subseq string beg (cdr span)) sentences)
             (push (cons beg (cdr span)) spans)
             (setf beg (car (second ss))))))
      (values (reverse sentences)
              (reverse spans)))))

(define-lazy-singleton <sentence-splitter> (make 'baseline-sentence-tokenizer)
  "Basic sentence splitter.")