;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutil)
  (:export #:cl-nlp-error
           #:not-implemented-error

           #:+newline+
           #:+newline-chars+
           #:newline-char-p
           #:+white-chars+
           #:white-char-p
           #:+period-chars+
           #:period-char-p

           #:*stopwords-en*
           #:ending-word-p

           #:filler

           #:+project-root+
           #:data-file
           #:list-from-file

           #:bin-search

           #:define-lazy-singleton
           #:sorted-ht-keys
           ))

(cl:defpackage #:nlp.corpora
  (:nicknames #:ncorp)
  (:use #:common-lisp #:rutil #:nlp.util)
  (:export #:corpus
           #:make-corpus

           #:corpus-name
           #:corpus-lang
           #:corpus-raw-texts
           #:corpus-clean-texts
           #:corpus-text-tokens

           #:read-corpus
           #:read-corpus-file

           #:token
           #:token-word
           #:token-beg
           #:token-end
           #:token-tag

           #:+brown-corpus+
           #:+nps-chat-corpus+
           ))

(cl:defpackage #:nlp.test-util
  (:nicknames #:ntest)
  (:use #:common-lisp #:rutil #:nlp.util #:nlp.corpora)
  (:export ))

(cl:defpackage #:nlp.core
  (:nicknames #:ncore)
  (:use #:common-lisp #:rutil #:nlp.util)
  (:export #:ngrams
           #:ngrams-eq
           #:ngrams-order
           #:ngrams-count
           #:ngrams-total-freq
           #:ngrams-max-freq
           #:ngrams-min-freq
           #:ngrams-pairs
           #:vocab
           #:freq
           #:prob
           #:logprob
           #:cond-prob
           #:cond-logprob
           #:freqs
           #:probs
           #:logprobs
           #:cond-probs
           #:cond-logprobs
           #:top-ngram
           #:hapaxes
           #:table-ngrams
           #:ngrams-table

           #:language-model
           #:lm-order
           #:lm-ngrams
           #:make-lm
           #:perplexity
           #:stupid-backoff-lm
           #:lm-backoff

           #:index-ngrams
           #:index-context-freqs
           #:index-prefix-transition-freqs
           #:index-word-transition-freqs
           #:normalize-freqs

           #:tokenize
           ;; #:stream-tokenize
           #:tokenizer
           #:regex-word-tokenizer
           #:baseline-sentence-tokenizer
           #:<word-tokenizer>
           #:<word-chnuker>
           #:<sentence-tokenizer>

           #:doublenewline-paragraph-splitter
           #:<paragraph-splitter>

           #:find-collocations
           ))

;; (cl:defpackage #:nlp.phonetics
;;   (:nicknames #:npho)
;;   (:use #:common-lisp #:rutil)
;;   (:export #:phonetic-transform
;;            ))

;; (cl:defpackage #:nlp.syntax
;;   (:nicknames #:nsyn)
;;   (:use #:common-lisp #:rutil)
;;   (:export #:pos-tag
;;            #:parse
;;            #:parse-deps
;;            ))

(cl:defpackage #:nlp.generation
  (:nicknames #:ngen)
  (:use #:common-lisp #:rutil #:nlp.util #:nlp.core)
  (:export #:generate-text

           #:text-generator

           #:markov-chain-generator
           #:mark-v-shaney-generator
           #:<mark-v-shaney>
           #:markov-order
           ))

;; (cl:defpackage #:nlp.learning
;;   (:nicknames #:nlearn)
;;   (:use #:common-lisp #:rutil)
;;   (:export #:cluster
;;            #:classify
;;            #:train
;;            ))


(cl:defpackage #:nlp-user
  (:nicknames #:nlp)
  (:use #:common-lisp #:rutil
        #:nlp.util #:nlp.corpora #:nlp.core #:nlp.generation))

(re-export-symbols '#:nutil '#:nlp-user)
(re-export-symbols '#:ncore '#:nlp-user)
(re-export-symbols '#:ncorp '#:nlp-user)
(re-export-symbols '#:ngen  '#:nlp-user)