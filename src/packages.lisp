;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutil)
  (:export #:+newline+
           #:+newline-chars+
           #:newline-char-p
           #:+white-chars+
           #:white-char-p
           #:+period-chars+
           #:period-char-p
           #:ending-word-p

           #:filler

           #:+project-root+
           #:data-file

           #:list-from-file
           ;; #:alist-from-file
           ;; #:table-from-file

           #:define-lazy-singleton
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
  (:export ;; #:ngram-freq
           ;; #:ngram-prob

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
  (:use #:common-lisp #:rutil #:nlp.util)
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
  (:use #:common-lisp #:rutil
        #:nlp.util #:nlp.corpora #:nlp.core #:nlp.generation)
  (:export #:+newline+
           #:+newline-chars+
           #:+white-chars+
           #:+period-chars+
           #:white-char-p
           #:period-char-p
           #:newline-char-p
           #:ending-word-p
           #:filler

           #:corpus-name
           #:corpus-lang
           #:corpus-raw-texts
           #:corpus-clean-texts
           #:corpus-text-tokens
           #:token
           #:token-word
           #:token-beg
           #:token-end
           #:token-tag

           #:index-context-freqs
           #:index-prefix-transition-freqs
           #:index-word-transition-freqs
           #:normalize-freqs
           #:tokenize
           #:regex-word-tokenizer
           #:baseline-sentence-tokenizer
           #:doublenewline-paragraph-splitter
           #:<word-tokenizer>
           #:<word-chnuker>
           #:<sentence-tokenizer>
           #:<paragraph-splitter>

           #:generate-text
           #:text-generator
           #:markov-chain-generator
           #:mark-v-shaney-generator
           #:<mark-v-shaney>
           #:markov-order

           #:print-word-in-contexts
           ))
