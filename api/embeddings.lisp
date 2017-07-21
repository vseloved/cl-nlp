;;; (c) 2017 Vsevolod Dyomkin

(in-package #:napi)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *embedding-limit* 1000000)

(defparameter *vecs* (list '*pubmed*))

(defmethod api-show ((endpoint (eql :embed)))
  (api-page ("Embeddings" :script "embeddings")
            (:h1 "Embeddings")
            (:div :style "margin-bottom: 20px;"
                  "Please enter a tokenized text to embed:")
            (:form :action "#" :onsubmit "return submit_embed()"
                   (:textarea :id "input" :rows "20" :cols "100"
                              "Hello World ! This is a test .

Best regards ,
NAPI")
                   (:br)
                   (:label :for "vecs" "Vectors: ")
                   (:select :id "vecs"
                            (dolist (vecs *vecs*)
                              (let ((name (substr (symbol-name vecs) 1 -1)))
                                (who:htm
                                 (:option :value name
                                          (who:fmt "~A~@[ - ~A~]" name
                                                   (documentation vecs 'variable)))))))
                   (:br)
                   (:input :type "submit"))
            (:pre :style "overflow: scroll" :id "output" "")))

(defmethod api-process ((endpoint (eql :embed)) data)
  (with ((((text "raw") (vecs-name "vecs")) ? data)
         (vecs (mksym vecs-name :package :nemb :format "*~A*")))
    (cond ((> (length text) *embedding-limit*)
           (list 400 '(:content-type "text/plain")
                 (list (fmt "Request exceeds API limit of ~A chars: ~A"
                            *embedding-limit* (length text)))))
          ((not (boundp vecs))
           (list 400 '(:content-type "text/plain")
                 (list (fmt "Unrecognized vecs ~A" vecs-name))))
          (t `(200 (:content-type "application/json;charset=utf-8")
                   (,(with-output-to-string (out)
                       (with ((words (nlp:tokenize nlp:<word-chunker> text))
                              (rez (map* ^(nlp:2vec (symbol-value vecs) %)
                                         words)))
                         (yason:encode
                          #h("result" rez
                             "html" (strjoin "<br/>"
                                             (map 'list ^(fmt "~A~{ ~A~}" %
                                                              (coerce %% 'list))
                                                  words rez)))
                          out)))))))))
