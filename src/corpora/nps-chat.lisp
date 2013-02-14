;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.corpora)
(named-readtables:in-readtable rutils-readtable)


(defmethod read-corpus-file ((type (eql :nps-chat)) file)
  "Read individual file from the NPS Chat Corpus."
  (cxml:parse-file file (make 'nps-chat-sax)))

#+manually
(defparameter *nps-chat-corpus*
  (let ((corpus (make-corpus :name "NPS Chat Corpus" :lang :en-us)))
    (fad:walk-directory
     (merge-pathnames "corpora/nps_chat/" +project-root+)
     #`(when (string= "xml" (pathname-type %))
         (mv-bind (text tokens) (read-corpus-file :nps-chat %)
           (push (mapcar #'car text) (corpus-raw-texts corpus))
           (push tokens (corpus-text-tokens corpus)))))
    corpus)
  "NPS Chat Corpus, Release 1.0 (July 2008).")


;; sax parsing

(defun attr (name attributes)
  (sax::standard-attribute-value
   (find name attributes :test 'string=
         :key #'sax::standard-attribute-local-name)))

(defclass nps-chat-sax (sax:sax-parser-mixin)
  ((posts :initform nil)
   (tokens :initform nil)))

(defmethod sax:start-element ((sax nps-chat-sax)
                              namespace-uri local-name qname attributes)
  (with-slots (posts tokens) sax
    (case (mkeyw local-name)
      (:post (push (attr "class" attributes) posts))
      (:terminals (push (list nil) tokens))
      (:t (push (make-token :word (attr "word" attributes)
                            :tag (mkeyw (attr "pos" attributes)))
                (car tokens))))))

(defmethod sax:characters ((sax nps-chat-sax) data)
  (with-slots (posts) sax
    (when (stringp (car posts))
      (setf (car posts) (cons data (car posts))))))

(defmethod sax:end-element ((sax nps-chat-sax) namespace-uri local-name qname)
  ;; to avoid warnings
  )

(defmethod sax:end-document ((sax nps-chat-sax))
  (values (reverse (slot-value sax 'posts))
          (reverse (mapcar #`(rest (reverse %))
                           (slot-value sax 'tokens)))))