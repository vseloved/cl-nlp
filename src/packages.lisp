;;; (c) 2013-2017 Vsevolod Dyomkin

(cl:defpackage #:nlp.util
  (:nicknames #:nutil)
  (:use #:common-lisp #:rutilsx
        #+dev #:should-test)
  (:export ;; errors
           #:cl-nlp-error
           #:not-implemented-error

           ;; characters
           #:+newline+
           #:+newline-chars+
           #:newline-char-p
           #:+white-chars+
           #:white-char-p
           #:+period-chars+
           #:period-char-p
           #:+punct-chars+
           #:punct-char-p
           #:+quote-chars+
           #:quote-char-p
           #:+open-quote-chars+
           #:open-quote-char-p
           #:+close-quote-chars+
           #:close-quote-char-p
           #:ending-word-p

           ;; files
           #:corpus-file
           #:data-file
           #:model-file
           #:lang-file
           #:src-file
           #:test-file
           #:dolines*
           #:dofiles
           #:with-tmp-file
           #:write-bin-file
           #:write-tsv
           #:download
           #:download-file

           #:write-dict
           #:list-from-file
           #:dict-from-file
           #:regex-from-file

           ;; zip
           #:zipped-file-data
           #:zip-add-text-file
           #:do-zip-entries
           #:with-zip
           #:with-zipped-zip

           ;; progress
           #:princ-progress
           #:progress-bar           

           ;; math
           #:argmax
           #:keymax
           #:~=
           #:sum
           #:frobenius-norm
           #:bin-search
           #:log-likelihood-ratio
           #:sample
           #:normal-random

           ;; misc
           #:ss
           #:filler
           #:uniq
           #:shorter?
           #:bound-equal
           #:timestamp           

           #:+inf
           ))

(cl:defpackage #:nlp.core
  (:nicknames #:ncore)
  (:use #:common-lisp #:rutilsx #:nlp.util
        #+dev #:should-test)
  (:export #:+iso-639-1+
           #:*lang*
           #:*lang-profiles*
           #:*lang-vars*
           #:iso-lang
           #:lang-iso
           #:in-lang
           #:init-lang
           #:with-lang
           #:def-lang-var
           #:def-lang-profile

           #:tok
           #:make-tok
           #:tok-id
           #:tok-word
           #:tok-lemma
           #:tok-beg
           #:tok-end
           #:tok-pos
           #:ent
           #:make-ent
           #:ent-ner
           #:tok->ent
           
           #:sent
           #:sent-toks
           
           #:ngrams
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

           #:count-ngram-freqs
           #:index-ngrams
           #:index-context-freqs
           #:index-prefix-transition-freqs
           #:index-word-transition-freqs
           #:normalize-freqs

           #:language-model
           #:lm-order
           #:lm-ngrams
           #:make-lm
           #:perplexity
           #:plain-lm
           #:stupid-backoff-lm
           #:lm-backoff
           
           #:tokenize
           #:tokenizer
           #:regex-word-tokenizer
           #:postprocessing-regex-word-tokenizer
           #:punct-sent-tokenizer
           #:full-text-tokenizer
           #:parag-splitter
           #:*full-text-tokenizer*
           #:*parag-splitter*
           #:parags->text

           #:find-collocations

           #:normalize
           #:denormalize

           #:*number-regex*
           #:*url-regex*
           #:*email-regex*
           ))

(cl:defpackage #:nlp.corpora
  (:nicknames #:ncorp)
  (:use #:common-lisp #:rutilsx #:nutil #:nlp.core
        #+dev #:should-test)
  (:export #:corpus
           #:make-corpus
           #:make-corpus-from-dir
           #:corpus-desc
           #:corpus-texts
           #:corpus-groups

           #:text
           #:make-text
           #:text-name
           #:text-raw
           #:text-clean
           #:text-parag-sent-toks
           #:text-trees

           #:read-corpus
           #:read-corpus-file
           #:map-corpus

           #:walk-corpus-dir

           #:sax-progress
           #:sax-progress-count
           #:sax-progress-report-rate
           #:sax-progress-tracking-tag
           #:xml-corpus-sax
           #:xml-progress-mixin
           #:xml-attr

           #:prepare-tree-for-reading
           ))

(cl:defpackage #:nlp.lexics
  (:nicknames #:nlex)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:dict
           #:dict-words
           #:dict-forms
           #:dict-pos-precedence
           #:precedence

           #:lookup
           #:pos-tags
           #:word/pos
           #:base-pos

           #:stemmer
           #:stem
           #:porter-stemmer

           #:lemmatizer
           #:lemmatize
           #:morph
           #:lem-dict

           #:mem-dict
           #:load-mem-dict))

(cl:defpackage #:nlp.embeddings
  (:nicknames #:nemb)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:2vec
           #:vecs
           #:mem-vecs
           #:lazy-mem-vecs
           #:unk
           #:vecs-dict
           #:vect-default
           #:vecs-understream
           
           #:init-vecs
           #:load-vecs))

(cl:defpackage #:nlp.learning
  (:nicknames #:nlearn)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:%=

           #:ex
           #:make-ex
           #:ex-fs
           #:ex-gold
           #:ex-raw
           
           #:init-model
           #:save-model
           #:load-model

           #:score
           #:rank
           #:classify
           #:train
           #:train1
           #:update1

           #:accuracy
           #:f1
           #:f_
           #:conf-mat
           #:cost

           #:make-fs
           #:extract-fs
           #:extract-gold
           #:ensure-fs-init
           #:fs-importance

           #:categorical-model
           #:ensemble-model
           
           #:m-weights
           #:m-random-state

           #:perceptron
           #:avg-perceptron
           #:training-perceptron
           #:ap-step
           #:ap-timestamps
           #:ap-totals

           #:decision-tree
           #:c4.5-tree
           #:cart-tree
           #:tree-decision-fn
           #:tree-decision-fn-dbg
           #:tree-repr
           #:tree-classes
           #:tree-max-depth
           #:tree-min-size
           #:*dtree-debug*
           #:*dtree-max-depth*

           #:info-gain
           #:weighted-info-gain
           #:gini-idx
           #:gini-split-idx

           #:random-forest
           #:forest-trees
           #:forest-tree-type

           #:softmax

           #:sgd
           #:grad1
           ))

(cl:defpackage #:nlp.tagging
  (:nicknames #:ntag)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore #:nlearn
        #+dev #:should-test)
  (:export #:tag
           #:tagger
           #:tagger-dict
           #:tagger-single-pos-words
           #:greedy-ap-dict-postagger
           ))

(cl:defpackage #:nlp.parsing
  (:nicknames #:nparse)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore #:nlearn
        #+dev #:should-test)
  (:shadow #:merge)
  (:export #:parse
           #:parser

           #:oracle
           #:ranking-oracle
           #:better-oracle
           #:best-oracle

           #:dep
           #:dep-rel
           #:dep-head
           #:dep-child
           #:make-dep
           #:print-dep
           #:print-stanford-dep
           #:print-conll-dep
           #:read-dep
           #:read-deps
           #:deps->tree

           #:parsed-sent
           #:sent-tree
           #:sent-deps
           #:sent-amr

           #:gr-ts
           #:gr-nts
           #:gr-nts-idx
           #:gr-root
           #:gr-unary-rules
           #:gr-binary-rules
           #:gr-root-rules
           #:gr-iurules
           #:gr-ibrules

           #:pprint-tags
           #:pprint-tree
           #:pprint-deps
           #:pprint-deps+tags
           #:pprint-syntax

           #:stack-buffer-parser
           #:parser-transitions
           #:parser-stack
           #:parser-buffer
           #:parser-ctx
           #:parser-toks

           #:deftransition
           #:select-transition
           #:judge-transitions
           #:list-transitions
           #:transition=

           ;; #:conparser
           ;; #:depparser
           ;; #:amrparser
           ))

(cl:defpackage #:nlp.generation
  (:nicknames #:ngen)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core
        #+dev #:should-test)
  (:export #:generate-text
           #:text-generator

           #:markov-chain-generator
           #:mark-v-shaney-generator
           #:markov-order
           ))

(cl:defpackage #:nlp-user
  (:nicknames #:nlp)
  (:use #:common-lisp #:rutilsx
        #:nlp.util #:nlp.corpora #:nlp.core #:nlp.lexics
        #:nlp.generation #:nlp.learning #:nlp.tagging #:nlp.parsing
        #+dev #:should-test)
  (:export ;; util
           #:grep
           #:tabulate
           ))

(rutils:re-export-symbols '#:nutil    '#:nlp-user)
(rutils:re-export-symbols '#:ncorp    '#:nlp-user)
(rutils:re-export-symbols '#:ncore    '#:nlp-user)
(rutils:re-export-symbols '#:nlex     '#:nlp-user)
(rutils:re-export-symbols '#:nlearn   '#:nlp-user)
(rutils:re-export-symbols '#:ngen     '#:nlp-user)
(rutils:re-export-symbols '#:ntag     '#:nlp-user)
(rutils:re-export-symbols '#:nparse   '#:nlp-user)
(rutils:re-export-symbols '#:nemb   '#:nlp-user)


;;; special namespaces

(cl:defpackage #:nlp.tags
  (:nicknames #:tag)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore)
  (:export #:export-tag
           #:*ner-tags*
           ))

(cl:defpackage #:nlp.deps
  (:nicknames #:dep)
  (:use #:common-lisp #:rutilsx #:nutil #:ncore)
  (:export #:+root+
           ))

;; (cl:defpackage #:nlp.amr
;;   (:nicknames #:amr)
;;   (:use #:common-lisp #:rutil #:nutil #:ncore)
;;   (:export #:<amr-tags>
;;            ))


;;; renamings

;; (rename-package "MGL-MAT" "MGL-MAT" '("MAT"))
