(in-package #:nlp.lexics)
(named-readtables:in-readtable rutilsx-readtable)

(deftest stem-porter ()
  (with-open-file (test-file (test-file "lexics/porter/voc.txt"))
    (with-open-file (gold-file (test-file "lexics/porter/output.txt"))
      (loop :for test-line := (read-line test-file nil)
            :for gold-line := (read-line gold-file nil)
            :while (and test-line gold-line) :do
         (should be string= gold-line
                 (stem <porter-stemmer> test-line))))))
