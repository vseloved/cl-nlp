;;; (c) 2017 Vsevolod Dyomkin

(in-package #:nlp.embeddings)
(named-readtables:in-readtable rutilsx-readtable)

#+sbcl
(defmethod init-vecs ((vecs mem-vecs) (format (eql :wvlib)) file &key prolog)
  (let ((dict #h(equal)))
    (block reading
      (with-open-file (in file :element-type 'unsigned-byte)
        (when prolog
          (loop :for ch := (read-byte in) :until (char= #\Newline (code-char ch))))
        (loop :for cc :from 0 :do
          (let ((word (loop :for ch := (read-byte in nil)
                            :unless ch :do (return-from reading)
                            :until (char= #\Space (code-char ch))
                            :collect (code-char ch) :into rez
                            :finally (return (coerce rez 'string))))
                (vec (make-array @vecs.order :element-type 'single-float)))
            (file-position in (file-position in))  ; sync file position for unix-read
            (sb-unix:unix-read (sb-sys:fd-stream-fd in)
                               (sb-sys:vector-sap vec)
                               (* 4 @vecs.order))
            (:= (? dict (normalize vecs word)) vec)
            (read-byte in nil) ; skip newline
            (when (zerop (rem cc 1000)) (format *debug-io* "."))))))
    (:= @vecs.dict dict)
    vecs))
    
