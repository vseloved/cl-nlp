;;; (c) 2017 Vsevolod Dyomkin

(in-package #:napi)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *lemmatize-limit* 1000000)


(defmethod api-show ((endpoint (eql :lemmatize)))
  (api-page ("Lemmatization" :script "lemmatization")
            (:h1 "Lemmatization")
            (:div :style "margin-bottom: 20px;"
                  "Please enter a tokenized text to lemmatize:")
            (:form :action "#" :onsubmit "return submit_lemma()"
                   (:textarea :id "input" :rows "20" :cols "100"
                              "Hello World ! This is a test .

Best regards ,
NAPI")
                   (:br)
                   (:input :type "submit"))
            (:div :id "output" "")))

(defmethod api-process ((endpoint (eql :lemmatize)) data)
  (with ((((text "raw")) ? data))
    (if (> (length text) *lemmatize-limit*)
        (list 400 '(:content-type "text/plain")
              (list (fmt "Request exceeds API limit of ~A chars: ~A"
                         *lemmatize-limit* (length text))))
        `(200 (:content-type "application/json;charset=utf-8")
              (,(with-output-to-string (out)
                  (yason:encode
                   #h("result"
                      (strjoin #\Space
                               (mapcar ^(or (nlp:lemmatize nlp:<wikt-dict> %)
                                            %)
                                       (nlp:tokenize nlp:<word-chunker>
                                                     text))))
                   out)))))))
