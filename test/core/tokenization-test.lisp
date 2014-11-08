(in-package #:nlp.core)
(named-readtables:in-readtable rutils-readtable)

;;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  
;;; These tests are referenced from
;;; https://github.com/nltk/nltk/blob/develop/nltk/test/tokenize.doctest
;;; The word tokenizer tests are placed in a text file word-tests.txt under
;;; test/core. The string to be tokenized and the tokens are separated by [>>>].
;;; The tokens which are read in as a string are transformed into a list of
;;; string tokens.
;;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  

(defparameter *word-tokenization-tests*
  "Load the word tokenizer tests."
  (dict-from-file (test-file "core/word-tests.txt")
                  :separator "[>>>]"
                  :val-transform #'(lambda (v)
                                     (split-sequence #\Space
                                                     v))))

(deftest word-tokenizer ()
  "Test each target string in a collection of word tokenizer tests"
  (maphash #'(lambda (str tokens)
                (should be equal
                        tokens
                        (multiple-value-bind (tokens spans)
                            (tokenize <word-tokenizer>
                                      str)
                          tokens)))
           *word-tokenization-tests*))

(deftest regex-tokenizer ()
  "Test custom regex based tokenizers."
  (should be equal
          '("," "." "," "," "?")
          (multiple-value-bind (tokens spans)
              (tokenize (make 'regex-word-tokenizer
                              :regex "[,\.\?!\"]\s*")
                        "Alas, it has not rained today. When, do you think, will it rain again?")
            tokens))
  (should be equal
          '("<p>" "<b>" "</b>" "</p>")
          (multiple-value-bind (tokens spans)
              (tokenize (make 'regex-word-tokenizer
                              :regex "</?(b|p)>")
                        "<p>Although this is <b>not</b> the case here, we must not relax our vigilance!</p>")
            tokens))
  (should be equal
          '("las" "has" "rai" "rai")
          (multiple-value-bind (tokens spans)
              (tokenize (make 'regex-word-tokenizer
                              :regex "(h|r|l)a(s|(i|n0))")
                        "Alas, it has not rained today. When, do you think, will it rain again?")
            tokens)))

(deftest sentence-tokenizer ()
  "Test the simple sentence splitter tokenizer."
  (should be equal
            '("Good muffins cost $3.88  in New York." "Please buy me  two of them." "Thanks.")
            (multiple-value-bind (tokens spans)
                (tokenize <sentence-splitter>
                          "Good muffins cost $3.88  in New York.  Please buy me  two of them.    Thanks.")
              tokens)))
