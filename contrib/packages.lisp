;;; (c) 2013-2015 Vsevolod Dyomkin

(cl:defpackage #:nlp.contrib.wordnet
  (:nicknames #:wordnet)
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core)
  (:export #:wordnet
           #:wordnet-uri
           #:sql-wordnet3
           #:<wordnet>

           #:with-wordnet
           #:connect-wordnet

           #:lemma
           #:lemma-word
           #:lemma-id
           #:lemmas

           #:synset
           #:synset-id
           #:synset-pos
           #:synset-def
           #:synset-name
           #:synsets

           #:sense
           #:sense-id
           #:senses

           #:sample
           #:sample-id
           #:samples

           #:link-type
           #:link-types
           #:link-id
           #:link-word1
           #:link-word2
           #:link-synset1
           #:link-synset2
           #:lexlink
           #:semlink

           #:words
           #:examples
           #:related

           #:lowest-common-hypernyms
           #:min-depth
           #:hypernym-paths
           #:path-similarity
           #:lch-similarity
           #:wup-similarity
           #:res-similarity
           #:jcn-similarity
           #:lin-similarity))

(cl:defpackage #:nlp.contrib.wn
  (:nicknames #:wn)
  (:use #:common-lisp #:rutilsx)
  (:export #:lemma
           #:lemmas
           #:synset
           #:synsets
           #:sense
           #:senses
           #:samples
           #:words
           #:examples
           #:related
           #:lch
           #:min-depth
           #:hypernym-paths
           #:path-similarity
           #:lch-similarity
           #:wup-similarity
           #:res-similarity
           #:jcn-similarity
           #:lin-similarity))

(cl:defpackage #:nlp.contrib.ms-ngrams
  (:use #:common-lisp #:rutilsx #:nlp
        #+dev #:should-test)
  (:export #:ms-ngrams
           #:ms-ngrams-url
           #:ms-ngrams-user-token
           #:ms-ngrams-catalog))

(cl:defpackage #:nlp.contrib.corpora
  (:use #:common-lisp #:rutilsx #:nlp #:ncore #:ncorp #:nutil
        #+dev #:should-test)
  (:export #:ptb-tagged-text
           #:reuters-text
           #:semcor-token))

(cl:defpackage #:nlp.contrib.embeddings
  (:use #:common-lisp #:rutilsx #:nlp))

(cl:defpackage #:nlp.lexics
  (:use #:common-lisp #:rutilsx #:nlp.util #:nlp.core #:nlp.lexics #:ncorp #:nutil
        #+dev #:should-test)
  (:export #:wordnet-lemmatizer
           #:<wordnet-lemmatizer>))
