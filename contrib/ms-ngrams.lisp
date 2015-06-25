;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.contrib.ms-ngrams)
(named-readtables:in-readtable rutils-readtable)


(defclass ms-ngrams (ngrams)
  ((count :initform -1)  ; special value to inidicate that we don't know it :)
   (url :initarg :url :accessor ms-ngrams-url
        :initform "http://web-ngram.research.microsoft.com/rest/lookup.svc")
   (user-token :initarg :user-token :accessor ms-ngrams-user-token)
   (catalog :initarg :catalog :initform "bing-body/apr10"
            :accessor ms-ngrams-catalog))
  (:documentation
   "Frontend to Microsoft Ngrams service.
    See http://web-ngram.research.microsoft.com/info/"))

(defmethod ngrams-eq ((ngrams ms-ngrams))
  #'equalp)

(defun ngram-string (ngram)
  "If NGRAM is a list, convert it to string."
  (if (listp ngram) (strjoin " " ngram) ngram))

(macrolet ((query-ngrams (op)
             `(with-slots (url user-token catalog order) ms-ngrams
                (let ((*read-eval* nil))
                  (read-from-string
                   (drakma:http-request
                    (fmt "~A/~A/~A/~A?u=~A&p=~A"
                         url catalog order ,op user-token
                         (ngram-string ngram))))))))

(defmethod logprob ((ngrams ms-ngrams) ngram)
  (query-ngrams "jp"))

(defmethod cond-logprob ((ngrams ms-ngrams) ngram)
  (query-ngrams "cp"))

) ; end of marolet

(macrolet ((query-ngrams (op)
             `(with-slots (url user-token catalog order) ms-ngrams
                (let ((*read-eval* nil))
                  (mapcar #'read-from-string
                          (split #\Newline
                                 (drakma:http-request
                                  (fmt "~A/~A/~A/~A?u=~A"
                                       url catalog order ,op user-token)
                                  :method :post
                                  :content (fmt "~{~A~%~}"
                                                (mapcar #'ngram-string
                                                        ngrams-list)))))))))

(defmethod logprobs ((ngrams ms-ngrams) &rest ngrams-list)
  (query-ngrams "jp"))

(defmethod cond-logprobs ((ngrams ms-ngrams) &rest ngrams-list)
  (query-ngrams "cp"))

) ; end of marolet
