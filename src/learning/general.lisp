;;; (c) 2014-2017 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


(defstruct (ex (:print-object (lambda (ex stream)
                                (format stream ">>~A:~A" @ex.gold @ex.fs))))
  fs gold raw)

(defclass categorical-model ()
  ((weights :initarg :weights :initform #h() :accessor m-weights))
  (:documentation
   "A categorical model has some way of distinguishing different categories
    based on the WEIGHTS mapping categories to some models."))

(defclass ensemble-model ()
  ()
  (:documentation
   "An ensemble model aggregates the results of many weak classifiers."))


(defgeneric score (model fs class)
  (:documentation
   "Score a selected CLASS with a MODEL given current FS."))

(defgeneric rank (model fs &key classes)
  (:documentation
   "Score all or selected CLASSES of a MODEL in a table
    for a given feature set FS."))

(defgeneric classify (model fs &key classes)
  (:documentation
   "Classify a feature set FS with a MODEL.
    A selected set of CLASSES can be provided.")
  (:method (model fs &key classes)
    (keymax (rank model fs :classes classes))))

(defgeneric train (model data &key)
  (:documentation
   "Train some MODEL with the provided DATA.")
  (:method :after ((model categorical-model) data &key)
    (rem# nil (m-weights model))))

(defgeneric train1 (model gold guess gold-fs &optional guess-fs)
  (:documentation
   "Perform one step of MODEL training with GOLD and GUESS variants
    and correspondng GOLD-FS and optionally GUESS-FS features."))

(defgeneric cost (model data &key)
  (:documentation
   "Calculate the cost of the current DATA set for the given MODEL."))

(defgeneric save-model (model path)
  (:documentation
   "Save MODEL data into PATH (which will be overwritten).")
  (:method :around ((model categorical-model) path)
    (with-open-file (out path :direction :output :element-type 'flex:octet
                         :if-does-not-exist :create)
      (:= out (flex:make-flexi-stream (gzip-stream:make-gzip-output-stream out)
                                      :external-format :utf8))
      (call-next-method model out)
      path))
  (:method ((model categorical-model) out)
    (let ((total (sum 'ht-count (vals @model.weights)))
          (i 0))
      (format out "~A~%" (ht-count @model.weights))
      (dotable (c fw @model.weights)
        (format out "~S ~A~%" c (ht-count fw))
        (dotable (f w fw)
          (format out "~S ~A " (string f) (float w))
          (princ-progress (:+ i) total))
        (terpri out)))))

(defgeneric load-model (model path &key)
  (:documentation
   "Load MODEL data from PATH (overwriting existing model).
    Keyword arg CLASSES-PACKAGE determines the package where class names
    will be interned (default: keyword).")
  (:method :around ((model categorical-model) path &key class-package)
    (format *debug-io* "~&Loading model from file: ~A - " path)
    (with-open-file (in path :element-type 'flex:octet)
      (:= in (flex:make-flexi-stream (gzip-stream:make-gzip-input-stream in)
                                     :external-format :utf8))
      (call-next-method model in :class-package (find-package class-package)))
    (format *debug-io* "done.~%")
    model)
  (:method ((model categorical-model) in &key class-package)
    (let ((total (read in))
          (i 0))
      (loop :repeat total :do
        (with ((class (let ((*package* (or class-package *package*)))
                        (read in)))
               (count (read in))
               (weights (set# class (m-weights model) #h(equal))))
          (loop :repeat count :do
            (setf (? weights (read in)) (read in)))
          (princ-progress (:+ i) total))))
    (rem# nil (m-weights model))
    model))


;;; quality measurement

(defgeneric accuracy (model gold-fs &key verbose)
  (:documentation
   "Measure MODEL's performance on GOLD-FS gold resultx features.")
  (:method (model gold-fs &key verbose)
    (let ((matched 0)
          (total 0)
          (len (length gold-fs)))
      (map nil ^(let ((guess (classify model @%.fs)))
                  (if (equal @%.gold guess)
                      (:+ matched)
                      (when verbose
                        (format *debug-io*
                                "~%guess: ~A~%gold:  ~A~@[~%raw: ~A~]~%fs: ~A~%"
                                guess @%.gold @%.raw @%.fs)))
                  (:+ total)
                  (unless verbose (princ-progress total len)))
           gold-fs)
      (* 100.0 (/ matched total)))))

(defgeneric fs-importance (model &key)
  (:documentation
   "Calculate MODEL's feature importance."))

(defun conf-mat (model gold-fs &key (verbose t))
  (let ((rez #h())
        (len (length gold-fs))
        (cc 0))
    (map nil ^(prog1 (:+ (get# (classify model @%.fs)
                               (getset# @%.gold rez #h())
                               0))
                (princ-progress (:+ cc) len))
         gold-fs)
    (when verbose
      (dolist (row (sort (ht->pairs rez) '>
                         :key ^(sum 'just (vals (rt %)))))
        (with (((cat counts) row)
               (total (sum 'just (vals counts))))
          (format t "~(~A~) (~A): ~$%  |"
                  cat total
                  (float (/ (get# cat counts 0) total 0.01)))
          (dolist (entry (sort (ht->pairs counts) '> :key 'rt))
            (unless (eql (lt entry) cat)
              (format t "  ~$% ~(~A~)"
                      (float (/ (rt entry) total 0.01))
                      (lt entry))))
          (terpri))))
    rez))
