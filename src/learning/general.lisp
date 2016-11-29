;;; (c) 2014-2016 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutilsx-readtable)


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

(defgeneric save-model (model path)
  (:documentation
   "Save MODEL data into PATH (which will be overwritten).")
  (:method :around ((model categorical-model) path)
    (with-open-file (out path :direction :output :element-type 'flex:octet
                         :if-does-not-exist :create)
      (:= out (flex:make-flexi-stream (gzip-stream:make-gzip-output-stream out)
                                      :external-format +utf-8+))
      (call-next-method model out)
      path))
  (:method ((model categorical-model) (out stream))
    (let* ((weights (m-weights model))
           (total (reduce '+ (mapcar #'ht-count (vals weights))))
           (i 0))
      (format out "~A~%" (ht-count weights))
      (dotable (c fw weights)
        (format out "~S ~A~%" c (ht-count fw))
        (dotable (f w fw)
          (format out "~S ~A " (string f) w)
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
            (:= (? weights (read in)) (read in)))
          (princ-progress (:+ i) total))))
    (rem# nil (m-weights model))
    model))

(defgeneric accuracy (model gold-fs &key verbose)
  (:documentation
   "Measure MODEL's performance on GOLD-FS gold resultx features.")
  (:method (model gold-fs &key verbose)
    (let ((matched 0) (total 0) (len (length gold-fs)))
      (loop :for (gold fs) :in gold-fs :do
         (let ((guess (classify model fs)))
           (if (equal gold guess)
               (:+ matched)
               (when verbose
                 (format *debug-io* "guess: ~A   gold: ~A    fs: ~A~%" guess gold fs))))
        (:+ total)
        (unless verbose (princ-progress total len)))
      (* 100.0 (/ matched total)))))

(defgeneric feature-importance (model)
  (:documentation
   "Display MODEL's feature importance."))


;; (defun precision (model gold-corpus &key verbose)
;;   (let ((matched 0) (total 0) (len (length gold-corpus)))
;;     (loop :for (t/f gold &rest fs) :in gold-corpus :do
;;        (let ((guess (classify model fs)))
;;          (if (equal gold guess)
;;              (:+ matched)
;;              (when verbose
;;                   (format *debug-io* "~A ~A ~A~%" guess gold fs)))))
;;         (:+ total)
;;         (unless verbose (princ-progress total len)))
