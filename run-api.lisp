(push "../rutils/" asdf:*central-registry*)
(push "../cl-nlp/" asdf:*central-registry*)
(push "./" asdf:*central-registry*)

(ql:quickload :cl-nlp-api)

(woo:run 'napi:napi :port 7778)
