;;; (c) 2015-2016 Vsevolod Dyomkin

(in-package #:nlp.parsing)
(named-readtables:in-readtable rutilsx-readtable)

(defclass stack-buffer-parser (parser)
  ((transitions :initarg :transitions :accessor parser-transitions)
   (stack :initarg :stack :accessor parser-stack)
   (buffer :initarg :buffer :accessor parser-buffer)
   (ctx :initarg :ctx :accessor parser-ctx)
   (tokens :initarg :tokens :accessor parser-tokens))
  (:documentation
   "A stack-buffer transition-based parser."))

(defstruct (transition (:constructor make-trans) (:conc-name trans-))
  fn node fs)

(defmacro deftransition (name (&optional label) &body body)
  "Define a stack-buffer parser transition function with a given NAME
   and that may possibly accept arguments LABEL.
   It alters the state of the parser and intermediate result.
   It's other arguments are PARSER state and intermediate state INTERM.
   Returns the difference between old and new state."
  (let ((label (or label (gensym)))
        (parser (gensym)))
    `(defun ,name (,parser interm &optional ,label)
       (declare (ignorable ,label ctx))
       (with (((stack buffer ctx) @ ,parser)
              (diff #h(equalp)))
         (symbol-macrolet ((s0 (first stack))
                           (b0 (first buffer)))
           ,@body
           diff)))))

(defgeneric select-transition (parser iterm)
  (:documentation
   "Select the best transition according to the current PARSER state
    and intermediate result ITERM.
    Return all transitions as a second value."))

(defgeneric judge-transitions (oracle parser transitions iterm gold
                               &key &allow-other-keys)
  (:documentation
   "Sort TRANSITIONS that are allowed for the current intermediate
    result ITERM and GOLD parse according to the ORACLE and PARSER state."))

(defgeneric list-transitions (parser iterm &optional gold &key &allow-other-keys)
  (:documentation
   ""))

(defun transition= (t1 t2 &optional (with-node t))
  "Compare if transitions T1 and T2 are the same
   (when WITH-NODE also having the same node)"
  (and (eql @t1.fn @t2.fn)
       (or (not with-node)
           (some 'null (list @t1.node @t2.node))
           (equalp @t1.node @t2.node))))
