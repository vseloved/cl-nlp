;;; (c) 2017 Vsevolod Dyomkin

(defpackage #:nlp.api
  (:nicknames :napi)
  (:use :common-lisp #:rutilsx
        #+dev #:should-test)
  (:export #:napi
           #:api-show
           #:api-process))
