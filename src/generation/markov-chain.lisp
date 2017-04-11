;;; (c) 2013-2017 Vsevolod Dyomkin

(in-package #:nlp.generation)
(named-readtables:in-readtable rutilsx-readtable)


(defgeneric generate-text (generator data length &key)
  (:documentation
   "Generate random text of LENGTH words based on some DATA
    (usually, table of transition probabilities between tokens).
    May not return period at the end."))


(defclass text-generator ()
  ()
  (:documentation
   "Base class for text generators."))

(defclass markov-chain-generator (text-generator)
  ((order :accessor markov-order :initarg :order))
  (:documentation
   "Markov chain generator of the given ORDER."))

(defclass mark-v-shaney-generator (markov-chain-generator)
  ((order :reader markov-order :initform 2))
  (:documentation
   "Markov chain generator of the 1st order — it is defined, because:
    - this is the general and most useful case
    - it allows to use optimized data-structures and a simpler algorithm
    - the name is well-known"))

(defmethod generate-text ((generator markov-chain-generator)
                          (transitions hash-table)
                          length &key skip-parags &allow-other-keys)
  "Generate text of LENGTH with a markov model of some MARKOV-ORDER described
   by the table TRANSITIONS of transition probabilities between reverse prefixes
   of MARKOV-ORDER length and words.
   Unless SKIP-PARAGS is set, the text may include newlines."
  (let* ((order (markov-order generator))
         (initial-prefix (if (> order 1)
                             (cons "¶" (make-list (1- order)))
                             (list "¶")))
         (prefix initial-prefix)
         rez)
    (loop :for i :from 1 :to length :do
      (let ((r (random 1.0)))
        (dotable (word prob
                  (or (get# prefix transitions)
                      ;; no continuation - start anew
                      (prog1 (get# (setf prefix initial-prefix) transitions)
                        ;; add period unless one is already there
                        (unless (every 'period-char-p (car rez))
                          (push "." rez)
                          (incf i)))))
          (when (<= (decf r prob) 0)
            (if (string= "¶" word)
                (if skip-parags
                    (decf i)  ; don't count newline if we don't include it
                    (push +newline+ rez))
                (push word rez))
            (setf prefix (cons word (butlast prefix)))
            (return)))))
    (reverse rez)))

(defmethod generate-text ((generator markov-chain-generator)
                          (model language-model)
                          length &key &allow-other-keys)
  "Generate text of LENGTH with a markov model of some MARKOV-ORDER
   with the given language MODEL.
   May not return period at the end."
  (assert (<= @generator.order @model.order))
  (let ((len (length @model.vocab))
        (ngram (list "<S>"))
        rez)
    (loop :for i :from 1 :to length :do
       (when (= (length ngram) @generator.order)
         (setf ngram (rest ngram)))
       (let ((total 0)
             (cond-probs (list (cons "<S>" 0))))
         (dolist (word @model.vocab)
           (unless (string= "<S>" word)
             (push (cons word
                         (incf total (cond-prob model (append ngram
                                                              (list word)))))
                   cond-probs)))
         (let ((word (car (bin-search (random total)
                                      (make-array len
                                                  :initial-contents cond-probs)
                                      '> :key 'cdr))))
           (if (string= "</S>" word)
               (progn (if (every 'period-char-p (car rez))
                          (unless (= i length)
                            (:- i))  ; just skip
                          (push "." rez))
                      (:= ngram (list "<S>")))
               (:= rez (cons word rez)
                   ngram (append ngram (list word)))))))
    (reverse rez)))


(def-lang-var <mark-v-shaney> (make 'markov-chain-generator :order 2)
  "The infamous Mark V. Shaney.")
