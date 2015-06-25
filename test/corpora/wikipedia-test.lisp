;;; (c) 2015 Vsevolod Dyomkin

(in-package #:nlp.contrib.corpora)
(named-readtables:in-readtable rutils-readtable)

(deftest read-simplewiki ()
  (should be string=
          "{{monththisyear|4}}"
          (slice #1=(text-clean (first (read-corpus-file
                                        :wikipedia
                                        (corpus-file "samples/simplewiki.xml"))))
                 0 (position #\Newline #1#))))
