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
     (zip:with-output-to-zipfile (zip (or path (fmt "model-~A.zip" (timestamp))))
       (call-next-method model zip)
       path))
  (:method ((model categorical-model) &optional zip)
    (let ((weights (m-weights model))
          (total-fs 0)
          (fs #h()))
      ;; classes
      (zip-as-text-file zip "classes.txt"
                        (strjoin #\Newline
                                 (mapcar #`(fmt "~A	~A"
                                                (lt %)
                                                (:+ total-fs (ht-count (rt %))))
                                         (ht->pairs weights))))
      (let ((fbuf (userial:make-buffer total-fs))
            (wbuf (userial:make-buffer total-fs))
            (i 0))
        ;; features
        (dotable (_ fw weights)
          (princ-progress (:+ i) (* 2 total-fs))
          (dotable (f _ fw)
            (userial:with-buffer fbuf
              (userial:serialize :int32 (coerce (fs-idx f fs) 'fixnum)))))
        (zip-as-text-file zip "fs.txt" (strjoin #\Newline (keys fs)))
        (zip:write-zipentry zip "fs.bin"
                            (flex:make-in-memory-input-stream fbuf)
                            :file-write-date (get-universal-time))
        ;; weights
        (dotable (_ fw weights)
          (princ-progress (:+ i) (* 2 total-fs))
          (dotable (_ weight fw)
            (userial:with-buffer wbuf
              (userial:serialize :float32 (coerce weight 'single-float)))))
        (zip:write-zipentry zip "weights.bin"
                            (flex:make-in-memory-input-stream wbuf)
                            :file-write-date (get-universal-time))))))

(defgeneric load-model (model path &key)
  (:documentation
   "Load MODEL data from PATH (overwriting existing model).
    Keyword arg CLASSES-PACKAGE determines the package where class names
    will be interned (default: keyword).")
  (:method :around (model path &key)
    (call-next-method)
    model)
  (:method ((model categorical-model) path &key (classes-package :keyword))
    (zip:with-zipfile (zip path)
      (flet ((unserialize-buf (type raw)
               (userial:with-buffer (make-array (length raw) :displaced-to raw
                                                :element-type 'flex:octet
                                                :adjustable t :fill-pointer t)
                 (userial:buffer-rewind)
                 (let ((total (/ (length raw) 4)))
                   (coerce (loop :for i :from 0 :below total
                              :do (princ-progress i (* 2 total))
                              :collect (userial:unserialize type))
                           'vector)))))
        (let* ((classes (pairs->ht
                         (mapcar #`(ds-bind (class count) (split #\Tab %)
                                     (pair (intern class classes-package)
                                           (parse-integer count)))
                                 (split #\Newline
                                        (zipped-file-data zip "classes.txt")
                                        :remove-empty-subseqs t))))
               (fsx (pairs->ht (mapindex #`(pair % (when %% (intern %% :f)))
                                         (split #\Newline
                                                (zipped-file-data zip "fs.txt")
                                                :remove-empty-subseqs t))))
               (raw-fs (unserialize-buf :int32
                                        (zipped-file-data zip "fs.bin"
                                                          :encoding nil)))
               (raw-ws (unserialize-buf :float32
                                        (zipped-file-data zip "weights.bin"
                                                          :encoding nil)))
               (i 0))
          (dotable (class count classes)
            (princ-progress i (length raw-fs))
            (let ((weights (set# class (m-weights model) #h())))
              (loop :while (< i count) :do
                 (set# (get# (svref raw-fs i) fsx) weights (svref raw-ws i))
                 (:+ i)))))))
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
