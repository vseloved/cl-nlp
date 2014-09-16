;;; (c) 2013-2014 Vsevolod Dyomkin

(in-package #:nlp.deps)
(named-readtables:in-readtable rutils-readtable)


(defun export-dep (str)
  "Intern and export DEP in package, then return it."
  (let ((dep (intern dep)))
    (export dep)
    dep))

(defparameter *deps* (dict-from-file (src-file "syntax/deps.txt")
                                     :key-transform #'export-dep)
  "Dependencies.")
