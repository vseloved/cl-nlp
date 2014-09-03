;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nlp.util)
(named-readtables:in-readtable rutils-readtable)


(defparameter +newline+
  (fmt "~%")
  "A string with a single newline.")

(defparameter +white-chars+
  '(#\Space #\No-Break_Space #\Tab #\Newline #\Return #\Linefeed)
  "Chars considered WHITESPACE.")

(defun white-char-p (char)
  "Test if CHAR is in +WHITE-CHARS+."
  (member char +white-chars+))

(defparameter +period-chars+
  '(#\. #\? #\!)
  "Chars considered legitimate sentence endings.")

(defun period-char-p (char)
  "Test if CHAR is in +PERIOD-CHARS+."
  (member char +period-chars+))

(defparameter +newline-chars+
  '(#\Newline #\Return #\Linefeed)
  "Chars considered legitimate paragraph endings.")

(defun newline-char-p (char)
  "Test if CHAR is in +PERIOD-CHARS+."
  (member char +newline-chars+))

(defparameter +quote-chars+
  '(#\" #\‘ #\’ #\« #\» #\“ #\” #\')
  "Chars considered legitimate quotation marks.")

(defun quote-char-p (char)
  "Test if CHAR is in +QUOTE-CHARS+."
  (member char +quote-chars+))

(defparameter +open-quote-chars+
  '(#\" #\‘ #\« #\“ #\')
  "Chars considered legitimate closing quotation marks.")

(defun open-quote-char-p (char)
  "Test if CHAR is in +OPEN-QUOTE-CHARS+."
  (member char +open-quote-chars+))

(defparameter +close-quote-chars+
  '(#\" #\’ #\» #\” #\')
  "Chars considered legitimate closing quotation marks.")

(defun close-quote-char-p (char)
  "Test if CHAR is in +CLOSE-QUOTE-CHARS+."
  (member char +close-quote-chars+))
