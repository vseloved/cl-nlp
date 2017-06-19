;;; (c) 2017 Vsevolod Dyomkin

(in-package #:napi)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *tokenize-limit* 1000000)

(defmethod api-show ((endpoint (eql :tokenize)))
  (api-page ("Tokenization" :script "tokenization")
    (:h1 "Tokenization")
    (:div :style "margin-bottom: 20px;"
          "Please enter a text to tokenize:")
    (:form :action "#" :onsubmit "return submit_tok()"
           (:textarea :id "input" :rows "20" :cols "100"
                      "Hello World! This is a test.

Best regards,
NAPI")
           (:br)
           (:select :id "tok-type"
                    (:option :value "full" "paragraph-sentence-token")
                    (:option :value "sent" "sentence-token")
                    (:option :value "tok" "token"))
           " " (:input :type "submit"))
    (:div :id "output" "")))

(defmethod api-process ((endpoint (eql :tokenize)) data)
  (with ((((text "raw") (op "op")) ? data))
    (if (> (length text) *tokenize-limit*)
        (list 400 '(:content-type "text/plain")
              (list (fmt "Request exceeds API limit of ~A chars: ~A"
                         *tokenize-limit* (length text))))
        (list 200 '(:content-type "application/json;charset=utf-8")
              (with ((rez spans (nlp:tokenize (ecase (mkeyw op)
                                                ((:full :sent)
                                                 nlp:*full-text-tokenizer*)
                                                (:tok nlp:<word-tokenizer>))
                                              (ecase (mkeyw op)
                                                (:sent (substitute
                                                        #\Space #\Newline text))
                                                ((:full :tok) text)))))
                (:= rez (ecase (mkeyw op)
                          ((:full :sent) rez)
                          (:tok (list (list (make 'nlp:sent
                                                  :toks (mapcar ^(nlp:make-tok
                                                                  :word %)
                                                                rez)))))))
                (list (with-output-to-string (out)
                        (yason:encode
                         #h("html" (who:with-html-output-to-string (out)
                                     (dolist (parag rez)
                                       (who:htm
                                        (:p (dolist (sent parag)
                                              (dolist (tok @sent.toks)
                                                (who:str (strcat @tok.word " ")))
                                              (who:htm (:br)))))))
                            "parags" (mapcar (lambda (parag)
                                               (mapcar (lambda (sent)
                                                         (mapcar 'nlp:tok-word
                                                                 @sent.toks))
                                                       parag))
                                             rez)
                            "spans" spans)
                         out))))))))
