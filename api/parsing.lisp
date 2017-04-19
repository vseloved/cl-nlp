;;; (c) 2017 Vsevolod Dyomkin

(in-package #:napi)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *pprint-limit* 10000)

(defmethod api-show ((endpoint (eql :pprint)))
  (api-page ("Pretty-printing trees" :script "parsing")
    (:h1 "Pretty-printing trees")
    (:div :style "margin-bottom: 20px;"
          (who:fmt
           "Currently, we can process consituency trees in Penn Treebank format
           and dependency trees in CoNLL & Stanford formats.
           The format is determined automatically.
           You can enter as many trees as you wish separated by empty lines,
           but there's a character limit of ~:D chars per request."
           *pprint-limit*))
    (:form :action "#" :onsubmit "return submit_trees()"
           (:textarea :id "input" :rows "20" :cols "100"
                      "
(TOP (S (NP (JJ Colorless) (JJ green) (NNS ideas))
        (VP (VB sleep) (ADVP (RB furiously)))
        (. .)))

amod(ideas-2, Colorless-0)
amod(ideas-2, green-1)
nsubj(sleep-3, ideas-2)
root(sleep-3, sleep-3)
advmod(sleep-3, furiously-4)
punct(sleep-3, .-5)")
           (:br)
           (:input :type "submit"))
    (:div :id "output" "")))

(defmethod api-process ((endpoint (eql :pprint)) data)
  (:= data (? data "raw"))
  (if (> (length data) *pprint-limit*)
      (list 400 '(:content-type "text/plain")
            (list (fmt "Request exceeds API limit of ~A chars: ~A"
                       *pprint-limit* (length data))))
      (list 200 '(:content-type "application/json;charset=utf-8")
            (with ((parts (remove-if 'blankp (re:split (fmt "~%~%")
                                                       (string-trim '(#\Newline)
                                                                    data))))
                   (rez #h(equal "trees" (make-array (length parts)) "html" nil)))
              (:= (? rez "html")
                  (who:with-html-output-to-string (out)
                    (doindex (i str parts)
                      (who:htm
                       (:pre (who:str
                              (:= (? rez "trees" i)
                                  (switch ((? str 0) :test ^(if (functionp %%)
                                                                (call %% %)
                                                                (eql %% %)))
                                    (#\( (pprint-contree str))
                                    (#'alpha-char-p (pprint-stanford-deptree str))
                                    (#'digit-char-p (pprint-conll-deptree str))))))))))
              (list (with-output-to-string (out)
                      (yason:encode rez out)))))))

(defun pprint-contree (str)
  (with-output-to-string (out)
    (nlp:pprint-tree (read-from-string (ncorp:prepare-tree-for-reading str))
                     :stream out)))

(defun pprint-stanford-deptree (str)
  (with-output-to-string (out)
    (nlp:pprint-deps (? (nparse:read-deps :stanford str) 0)
                     :stream out)))

(defun pprint-conll-deptree (str)
  (with-output-to-string (out)
    (nlp:pprint-deps (? (nparse:read-deps :conll str) 0)
                     :stream out)))
