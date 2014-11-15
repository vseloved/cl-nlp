;;; (c) 2014 Vsevolod Dyomkin

(in-package #:nlp.learning)
(named-readtables:in-readtable rutils-readtable)


(defclass categorical-model ()
  ((weights :initarg :weights :initform #h() :accessor m-weights))
  (:documentation
   "A categorical model has some way of distinguishing different categories
    based on the WEIGHTS mapping categories to some models."))

(defgeneric classify (model fs)
  (:documentation
   "Classify a feature set FS with a MODEL."))

(defgeneric train (model data &key)
  (:documentation
   "Train some MODEL with the provided DATA."))

(defgeneric train1 (model fs gold guess)
  (:documentation
   "Perform one step of MODEL training with features FS
    with GOLD and GUESS variants."))

(defgeneric save-model (model &optional path)
  (:documentation
   "Save MODEL data into PATH (which will be overwritten).")
  (:method :around (model &optional path)
    (with-open-file (out path :direction :output :element-type 'flex:octet
                         :if-does-not-exist :create)
      (:= out (flex:make-flexi-stream (gzip-stream:make-gzip-output-stream out)
                                      :external-format +utf-8+))
      (call-next-method model out)
      path))
  (:method ((model categorical-model) &optional out)
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
  (:method :around (model path &key)
    (format *debug-io* "~&Loading model from file: ~A - " path)
    (with-open-file (in path :element-type 'flex:octet)
      (:= in (flex:make-flexi-stream (gzip-stream:make-gzip-input-stream in)
                                     :external-format +utf-8+))
      (call-next-method model in))
    (format *debug-io* "done.~%")
    model)
  (:method ((model categorical-model) in &key)
    (let ((total (read in))
          (i 0))
      (loop :repeat total :do
         (let* ((class (read in))
                (count (read in))
                (weights (set# class (m-weights model) #h())))
           (loop :repeat count :do
              (:= (? weights (intern (read in) :f)) (read in)))
           (princ-progress (:+ i) total))))))

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
                 (format *debug-io* "~A ~A ~A~%" guess gold fs))))
        (:+ total)
        (unless verbose (princ-progress total len)))
      (* 100.0 (/ matched total)))))

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
