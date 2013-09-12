;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


;;; generic element access protocol

(defgeneric generic-elt (obj key &rest keys)
  (:documentation
   "Generic element access in OBJ by KEY.
    Supports chaining with KEYS.")
  (:method :around (obj key &rest keys)
    (reduce #'generic-elt keys :initial-value (call-next-method obj key))))

(defmethod generic-elt ((obj list) key &rest keys)
  (declare (ignore keys))
  (nth key obj))

(defmethod generic-elt ((obj vector) key &rest keys)
  (declare (ignore keys))
  (svref obj key))

(defmethod generic-elt ((obj sequence) key &rest keys)
  (declare (ignore keys))
  (elt obj key))

(defmethod generic-elt ((obj hash-table) key &rest keys)
  (declare (ignore keys))
  (get# key obj))


;;; pair

(defstruct (pair (:type list) (:conc-name nil))
  l r)

(declaim (inline pair print-pair))

(defun pair (x y)
  (make-pair :l x :r y))

(defun ht->pairs (ht)
  "Dump hash-table HT to list of pairs."
  (with-hash-table-iterator (gen-fn ht)
    (loop
       :for (valid key val) := (multiple-value-list (gen-fn))
       :unless valid :do (return rez)
       :collect (pair key val) :into rez)))

(defun pairs->ht (pairs &rest hash-table-initargs)
  "Create hash-table from the list of PAIRS.
   Hash table is initialized using the HASH-TABLE-INITARGS."
  (loop
     :with ht := (apply #'make-hash-table hash-table-initargs)
     :for pair :in pairs
     :do (set# (l pair) ht (r pair))
     :finally (return ht)))


;;; generic sequence iteration protocol

(defgeneric seq (seq &optional key)
  (:documentation
   ""))

(defmethod seq ((seq list) &optional (key 0))
  (when seq
    (values (first seq)
            key
            (lambda () (seq (rest seq) (1+ key))))))

(defmethod seq ((seq vector) &optional (key 0))
  (when (< key (1- (length seq)))
    (values (elt seq key)
            key
            (lambda () (seq seq (1+ key))))))

(defmethod seq ((seq hash-table)
                &optional (gen-fn
                           (with-hash-table-iterator (gen-fn seq) gen-fn)))
  (mv-bind (valid key val) (funcall gen-fn)
    (when valid
      (values val
              key
              (lambda () (seq seq gen-fn))))))

(defmacro donext ((elt seq &optional result) &body body)
  (with-gensyms (val key next)
    `(loop (mv-bind (,val ,key ,next) (funcall (if (boundp ',next)
                                                   ,next
                                                   (lambda () (seq ,seq))))
             (unless ,next (return ,result))
             (ds-bind ,(mklist elt) ,cur
               ,@body)))))


;;; generic table access and iteration protocol

(defgeneric keys (table)
  (:documentation
   "Return a list of all keys in a TABLE.
    Order is unspecified.")
  (:method ((table hash-table))
    (ht-keys table)))

(defgeneric vals (table)
  (:documentation
   "Return a list of all values in a TABLE.
    Order is unspecified.")
  (:method ((table hash-table))
    (ht-vals table)))

(defgeneric pairs (table)
  (:documentation
   "Return a list of all key-value pairs in a TABLE.
    Order is unspecified.")
  (:method ((table hash-table))
    (with-hash-table-iterator (gen-fn table)
      (let (rez)
        (loop
           (mv-bind (valid key val) (gen-fn)
             (unless valid (return rez))
             (push (pair key val) rez)))
        (reverse rez)))))

(defgeneric eq-test (table)
  (:documentation
   "Return an equality test predicate of the TABLE.")
  (:method ((table hash-table))
    (hash-table-test table)))

(defgeneric maptable (fn table)
  (:documentation
   "Like MAPCAR but for a single table.")
  (:method (fn (table hash-table))
    (with-hash-table-iterator (gen-fn table)
      (let ((rez (make-hash-table :test (hash-table-test table))))
        (loop
           (mv-bind (valid key val) (gen-fn)
             (unless valid (return rez))
             (set# key rez (funcall fn key val))))
        rez))))
