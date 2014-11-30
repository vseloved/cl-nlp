;;; (c) 2013 Vsevolod Dyomkin

(cl:defpackage #:nlp.contrib.ms-ngrams
  (:use #:common-lisp #:rutil #:nlp)
  (:export #:ms-ngrams
           #:ms-ngrams-url
           #:ms-ngrams-user-token
           #:ms-ngrams-catalog))

(cl:defpackage #:nlp.contrib.corpora
  (:use #:common-lisp #:rutil #:nlp)
  (:export #:ptb-tagged-text
           #:reuters-text
           #:semcor-token))
