(in-package :UTILITIES)

;;;**************************************************************************
;;; STRING UTILITY FUNCTION
;;;**************************************************************************
;;;
;;; REMOVE-LEADING-NON-ALPHA-CHARS
;;;
;;;**************************************************************************

;;;---------------------------------------------------------------------------
;;; *SPECIAL-CHARACTERS*
;;;---------------------------------------------------------------------------

(defparameter *SPECIAL-CHARACTERS*
    '(#\. #\( #\) #\, #\; #\' #\" #\: #\` #\~ #\# #\@ #\! #\? #\| #\\ #\{ #\}
      #\[ #\] #\$ #\% #\^ #\& #\* #\+ #\= #\< #\> #\/ #\- #\_))

;;;---------------------------------------------------------------------------
;;; *BALANCED-SPECIAL-CHARACTERS*
;;;---------------------------------------------------------------------------

(defparameter *BALANCED-SPECIAL-CHARACTERS*
    '(#\( #\) #\[ #\] #\{ #\}))

;;;---------------------------------------------------------------------------
;;; SPECIAL-CHAR-P
;;;---------------------------------------------------------------------------

(defun SPECIAL-CHAR-P (char)
  (and (member char *SPECIAL-CHARACTERS* :test #'char=) t))

;;;---------------------------------------------------------------------------
;;; BALANCED-SPECIAL-CHAR-P
;;;---------------------------------------------------------------------------

(defun BALANCED-SPECIAL-CHAR-P (char)
  (and (member char  *BALANCED-SPECIAL-CHARACTERS* :test #'char=) t))
   
;;;-----------------------------------------------------------------------------
;;; PUNTUATION-CHARACTER-P
;;;-----------------------------------------------------------------------------

(defvar *punctuation-characters* '(#\, #\; #\. #\? #\! #\:))

;;;----------------------------------------------------------------------------

(defun PUNCTUATION-CHARACTER-P (token)
  (and (characterp token)
       (member token *punctuation-characters* :test #'char=)))

;;;---------------------------------------------------------------------------
;;;  CHARACTER-TYPE
;;;---------------------------------------------------------------------------

(defmethod CHARACTER-TYPE ((char CHARACTER))
  (cond ((alpha-char-p char)
	 :alphabetic)
	((digit-char-p char)
	 :numeric)
	(t
	 :special)))

;;;---------------------------------------------------------------------------
;;; ALPHABETIC-STRING-P
;;;---------------------------------------------------------------------------

(defun ALPHABETIC-STRING-P (string
			    &optional
			    (allow-delimiter nil)
			    (delimiter #\-))
  (and (not (empty-string-p string))
       (if (not allow-delimiter)
	   (every #'alpha-char-p string)
	 (and (some #'alpha-char-p string)
	      (every #'(lambda (x)
			 (or (alpha-char-p x)(char= x delimiter)))
		     string)))))

;;;---------------------------------------------------------------------------
;;; DIGIT-STRING-P
;;;---------------------------------------------------------------------------

(defun DIGIT-STRING-P (string)
  (and (not (empty-string-p string))
       (every #'digit-char-p string)))

;;;---------------------------------------------------------------------------
;;; SPECIAL-STRING-P
;;;---------------------------------------------------------------------------

(defun SPECIAL-STRING-P (string)
  (every #'special-char-p  string))

;;;---------------------------------------------------------------------------
;;; ALPHA-NUMERIC-STRING-P
;;;---------------------------------------------------------------------------

(defun ALPHA-NUMERIC-STRING-P (string)
  (and (not (alphabetic-string-p string))
       (not (digit-string-p string))
       (every #'alphanumericp string)))

;;;---------------------------------------------------------------------------
;;; ALPHA-SPECIAL-STRING-P
;;;---------------------------------------------------------------------------

(defun ALPHA-SPECIAL-STRING-P (string)
   (and (not (alphabetic-string-p string))
	(not (special-string-p string))
	(every #'(lambda (char)
		   (or (alpha-char-p char)
		       (special-char-p char)))
	       string)))

;;;---------------------------------------------------------------------------
;;; NUMERIC-SPECIAL-STRING-P
;;;---------------------------------------------------------------------------

(defun NUMERIC-SPECIAL-STRING-P (string)
  (and 	(not (digit-string-p string))
	(not (special-string-p string))
     	(every #'(lambda (char)
		   (or (digit-char-p char)
		       (special-char-p char)))
	       string)))

;;;---------------------------------------------------------------------------
;;; ALPHA-NUMERIC-SPECIAL-STRING-P
;;;---------------------------------------------------------------------------

;;; Note: Rewrite using some!

(defun ALPHA-NUMERIC-SPECIAL-STRING-P (string)
  (and (not (alphabetic-string-p string))
       (not (digit-string-p string))
       (not (special-string-p string))
       (not (alpha-numeric-string-p string))
       (not (alpha-special-string-p string))
       (not (numeric-special-string-p string))))

;;;----------------------------------------------------------------------------
;;; COUNT-DIGITS-IN-STRING
;;;----------------------------------------------------------------------------

(defmethod COUNT-DIGITS-IN-STRING ((thing string))
  (let ((count 0))
    (dotimes (i (length thing))
      (when (digit-char-p (elt thing i))
	(incf count)))
    count))

;;;---------------------------------------------------------------------------
;;; EMPTY-STRING-P
;;;---------------------------------------------------------------------------

(defun EMPTY-STRING-P (s)
  "Is s the empty string or nil?"
  (or (null s)
      (equal s *null-string*)))

;;;------------------------------------------------------------------------------
;;; MAKE-DESCRIPTION-STRING
;;;------------------------------------------------------------------------------


(defmethod MAKE-DESCRIPTION-STRING ((token NUMBER) &key (delimiter #\space)
							 (padded nil))
  (format nil "~d" token))


;;;-----------------------------------------------------------------------------

(defmethod MAKE-DESCRIPTION-STRING ((thing SYMBOL)
				    &key (delimiter #\space)
					 (padded nil))
  (declare (ignore delimiter padded))
  (symbol-name thing))
  
;;;------------------------------------------------------------------------------

(defmethod MAKE-DESCRIPTION-STRING ((tokens NULL) &key (delimiter #\space)
						       (padded nil))
  (declare (ignore delimiter padded))
  *null-string*)

;;;----------------------------------------------------------------------------

(defmethod MAKE-DESCRIPTION-STRING ((string STRING) &key (delimiter #\space)
						       (padded nil))
  (declare (ignore delimiter padded))
  string)
  
;;;----------------------------------------------------------------------------

(defmethod MAKE-DESCRIPTION-STRING ((character CHARACTER)
				    &key (delimiter #\space)
					 (padded nil))
  (declare (ignore delimiter padded))
  (format nil "~a" character))

;;;------------------------------------------------------------------------------

;;; We just build a string by interspercing tokens and " " characters.
;;; avoid consing by using a string buffer directly instead of
;;; outsourcing to seq-list-to-seq. The code is sufficiently simple to
;;; not worry about code duplication (and we save that consing).  Don't
;;; use format.

(defmethod MAKE-DESCRIPTION-STRING ((tokens CONS) &key (delimiter #\space)
						       (padded nil))
  (setf delimiter (string delimiter))
  (cond ((atom (cdr tokens))
	 (make-description-string-cons tokens))
	(t
	 (with-output-to-string (*standard-output*)
	   (let ((*print-pretty* nil))
	     (loop for token in tokens
		 for space = nil then t
		 do (when space 
		      (when padded (write-char #\space))
		      (write-string delimiter)
		      (when padded (write-char #\space)))
		    (when (typep token 'value-mixin)
		      (setf token (make-description-string (object-value token))))
		    (princ token)))))))

;;;------------------------------------------------------------------------------
;;; MAKE-DESCRIPTION-STRING-CONS
;;;------------------------------------------------------------------------------

(defmethod MAKE-DESCRIPTION-STRING-CONS ((token VALUE-MIXIN)
					 &key (delimiter #\space)
					      (padded nil))
  (make-description-string (object-value token) 
			   :delimiter delimiter :padded padded))

 
;;;----------------------------------------------------------------------------

(defmethod MAKE-DESCRIPTION-STRING-CONS ((tokens CONS) &key (delimiter #\space)
						       (padded nil))
  (concatenate 'string
    (make-description-string  (car tokens) :delimiter delimiter :padded padded)
    (make-description-string (cdr tokens) :delimiter delimiter :padded padded)))


;;;---------------------------------------------------------------------------
;;; ALPHA-STRING-HYPHENATED-P
;;;---------------------------------------------------------------------------

#-SBCL
(defmethod ALPHA-STRING-HYPHENATED-P ((string STRING))
  (and (some #'alpha-char-p string)
       (some #'(lambda (x)(char= x #\-)) string)
       (every #'(lambda (char)
		  (or (alpha-char-p char)(char= char #\-)))
	      string)))


;;;---------------------------------------------------------------------------
;;; ALPHA-STRING-SHASHED-P
;;;---------------------------------------------------------------------------

#-SBCL
(defmethod ALPHA-STRING-SLASHED-P ((string STRING))
  (and (some #'alpha-char-p string)
       (some #'(lambda (x)(char= x #\/)) string)
       (every #'(lambda (char)
		  (or (alpha-char-p char)(char= char #\/)))
	      string)))

;;;---------------------------------------------------------------------------
;;; ALPHA-STRING-UNDERSCORED-P
;;;---------------------------------------------------------------------------

#-SBCL
(defmethod ALPHA-STRING-UNDERSCORED-P ((string STRING))
  (and (some #'alpha-char-p string)
       (some #'(lambda (x)(char= x #\_)) string)
       (every #'(lambda (char)
		  (or (alpha-char-p char)(char= char #\_)))
	      string)))

;;;---------------------------------------------------------------------------
;;; SPLIT-SPECIAL-STRING
;;;---------------------------------------------------------------------------

;;; Splits string into substrings based on character type.
;;;
;;; 5/4/2010 mrh: Now using CONSTRUCT-STRING instead of FILL-STRING.
;;; 5/4/2010 mrh: Rewriting for efficiency -- no CONSTRUCT-STRING, only one NREVERSE.
;;;               30-60% faster, depending on string length.

(defmethod SPLIT-SPECIAL-STRING ((string STRING))
  (let ((sub-strings '())
	(cur-char-type nil)
	(next-char-type nil)
	(last-boundary 0)
	(cur-position -1))
    (when (not (empty-string-p string))
      (loop for c across string
	  do
	    (incf cur-position)
	    (cond ((null cur-char-type)
		   ;; initial case
		   (setf cur-char-type (character-type c)))
		  (t
		   (setf next-char-type (character-type c))
		   (when (not (eq cur-char-type next-char-type))
		     ;; type has changed; extract range and switch to new type, 
		     ;; current position is start of new string
		     (push (subseq string last-boundary cur-position) sub-strings)
		     (setf cur-char-type next-char-type)
		     (setf last-boundary cur-position)))))
      ;; don't forget last element
      (push (subseq string last-boundary) sub-strings))
    (nreverse sub-strings)))
		   

#+IGNORE
(defmethod SPLIT-SPECIAL-STRING ((string STRING))
  (let* ((characters (explode-string1 string))
	 (sub-strings nil)
	 (next-chars nil)
	 (char-state (and characters (character-type (first characters)))))
    (dolist (char characters)
      (cond ((eq (character-type char) char-state)
	     (push char next-chars))
	    (t
	     (push (construct-string (nreverse next-chars)) sub-strings)
	     (setf char-state (character-type char))
	     (setf next-chars (list char)))))
    (when next-chars
      (push (construct-string (nreverse next-chars)) sub-strings))
    (nreverse sub-strings)))

;;;---------------------------------------------------------------------------
;;; STRING-CONTAINS-BALANCED-SPECIAL-CHARS-P
;;;---------------------------------------------------------------------------

;;; Splits string into substrings based on character type.

(defmethod STRING-CONTAINS-BALANCED-SPECIAL-CHARS-P ((string STRING))
  (some #'balanced-special-char-p string))

;;;---------------------------------------------------------------------------
;;; SPLIT-STRING-BY-BALANCED-CHARS
;;;---------------------------------------------------------------------------

;;; Splits string into substrings based on character type.

(defmethod SPLIT-STRING-BY-BALANCED-SPECIAL-CHARS ((string STRING))
  (split-string-by-chars string *balanced-special-characters*))

;;;---------------------------------------------------------------------------
;;; STRING-CONTAINS-PUNCTUATION-CHARS-P
;;;---------------------------------------------------------------------------

;;; Splits string into substrings based on character type.

(defmethod STRING-CONTAINS-PUNCTUATION-CHARS-P ((string STRING)
						&optional
						(punctuation-chars  '(#\; #\,)))
  (some #'identity
	(mapcar #'(lambda (char)
		    (some #'(lambda (x)(char= x char)) string))
		punctuation-chars)))

;;;---------------------------------------------------------------------------
;;; SPLIT-STRING-BY-PUNCTUATION-CHARS
;;;---------------------------------------------------------------------------

;;; Splits string into substrings based on character type.

(defmethod SPLIT-STRING-BY-PUNCTUATION-CHARS ((string STRING)
					      &optional 
					      (punctuation-chars '(#\; #\,)))
  (split-string-by-chars string punctuation-chars))

;;;---------------------------------------------------------------------------
;;; SPLIT-STRING-BY-CHARS
;;;---------------------------------------------------------------------------

;;; Splits string into substrings based on list of delimiter characters.
;;; 
;;; 5/4/2010 mrh: Doc comment seems wrong -- character _type_ not used.
;;; 5/4/2010 mrh: Now uses CONSTRUCT-STRING instead of FILL-STRING.

(defmethod SPLIT-STRING-BY-CHARS ((string STRING)(chars LIST))
  (let* ((characters (explode-string1 string))
	 (sub-strings nil)
	 (next-chars nil)
	 #+IGNORE
	 (char-state (and characters (character-type (first characters)))))
    (dolist (char characters)
      (cond ((member char chars :test #'char=)
	     (when next-chars
	       (push (construct-string (nreverse next-chars)) sub-strings))
	     (push (make-string 1 :initial-element char) sub-strings)
	     (setf next-chars nil))
	    (t
	     (push char next-chars))))
    (when next-chars
      (push (construct-string (nreverse next-chars)) sub-strings))
    (nreverse sub-strings)))

;;;---------------------------------------------------------------------------
;;; FILL-STRING
;;;---------------------------------------------------------------------------

;;; <Elements> should be a list of characters.

;;; 5/4/2010 mrh: Replaced by calls to CONSTRUCT-STRING on observation that all
;;;               elements of list were always used.

(defmethod FILL-STRING ((string STRING)(elements LIST))
  (warn "FILL-STRING is deprecated -- use (CONSTRUCT-STRING <list>) instead.")
  (dotimes (i (min (length string)(length elements)))
    (setf (elt string i)(elt elements i)))
  string)

;;;---------------------------------------------------------------------------
;;; CONSTRUCT-STRING
;;;---------------------------------------------------------------------------

;;; Replaces FILL-STRING.  1/3rd the time, on Synthes-length strings.  Should
;;; be even more dramatic with longer ones.  Note, function version is 16% faster
;;; than method version.

(defun CONSTRUCT-STRING (elements)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let* ((elements-length (length elements))
	 (s (make-string elements-length))
	 (cur-pos 0))
    (dolist (element elements)
      (setf (schar s cur-pos) element)
      (incf cur-pos))
    s))


;;;---------------------------------------------------------------------------
;;; EXPLODE-STRING1
;;;---------------------------------------------------------------------------

;;; This returns a list of the characters that makeup a string.
;;; Note: This redefines the PQ Version in PQ-Sequences which implants
;;; something else.

(defmethod EXPLODE-STRING1 ((string STRING))
  (let ((char-list nil))
    (dotimes (i (length string))
      (push (schar string i) char-list))
    (nreverse char-list)))


;;;---------------------------------------------------------------------------
;;; OBJECT-STRING-VALUE
;;;---------------------------------------------------------------------------

;;; The method on string need only return the string.

(defmethod OBJECT-STRING-VALUE ((string STRING))
  string)

;;;----------------------------------------------------------------------------
;;; CAPITALIZED-WORD-P
;;;----------------------------------------------------------------------------

(defmethod CAPITALIZED-WORD-P ((word SYMBOL))
   (capitalized-word-p (symbol-name word)))
  
;;;----------------------------------------------------------------------------

(defmethod CAPITALIZED-WORD-P ((word STRING))
  (and (<= (char-code #\A) (char-code (elt word 0))))
       (<= (char-code (elt word 0)) (char-code #\Z)))
  
;;;----------------------------------------------------------------------------

(defmethod CAPITALIZED-WORD-P ((word T))
  nil)

;;;----------------------------------------------------------------------------
;;; WORD-ENDS-IN-??-P
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-??-P ((word SYMBOL))
   (word-ends-in-??-p (symbol-name word)))
  
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-??-P ((word STRING))
  (word-ends-in-xx-p word "??"))

;;;----------------------------------------------------------------------------
;;; WORD-ENDS-IN-TM-P
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-TM-P ((word SYMBOL))
   (word-ends-in-tm-p (symbol-name word)))
  
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-TM-P ((word STRING))
  (word-ends-in-xx-p word "TM"))

;;;----------------------------------------------------------------------------
;;; WORD-ENDS-IN-ING-P
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-ING-P ((word SYMBOL))
   (word-ends-in-ing-p (symbol-name word)))
  
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-ING-P ((word STRING))
  (or (word-ends-in-xx-p word "ing")
      (word-ends-in-xx-p word "ING")))

;;;----------------------------------------------------------------------------
;;; WORD-ENDS-IN-ER-P
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-ER-P ((word SYMBOL))
   (word-ends-in-er-p (symbol-name word)))
  
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-ER-P ((word STRING))
  (or (word-ends-in-xx-p word "er")
      (word-ends-in-xx-p word "ER")))

;;;----------------------------------------------------------------------------
;;; WORD-ENDS-IN-ED-P
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-ED-P ((word SYMBOL))
   (word-ends-in-er-p (symbol-name word)))
  
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-ED-P ((word STRING))
  (or (word-ends-in-xx-p word "ed")
      (word-ends-in-xx-p word "ED")))

;;;----------------------------------------------------------------------------
;;; WORD-ENDS-IN-XX-P
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-XX-P ((word SYMBOL)(XX string))
   (word-ends-in-xx-p (symbol-name word) xx))
  
;;;----------------------------------------------------------------------------

(defmethod WORD-ENDS-IN-XX-P ((word STRING)(XX string))
  (let* ((word-size (length word))
	 (xx-size (length xx))
	 (ends-in-xx-p (> word-size xx-size)))
    (when ends-in-xx-p
      (dotimes (i xx-size)
	(unless (char= (elt word (- word-size (1+ i)))
		       (elt xx (- xx-size (1+ i))))
	  (setf ends-in-xx-p nil)
	  (return nil)))
      ends-in-xx-p)))


;;;---------------------------------------------------------------------------
;;; REMOVE-LEADING-NON-ALPHA-CHARS
;;;---------------------------------------------------------------------------
		    
(defun REMOVE-LEADING-NON-ALPHA-CHARS (string)
  (let ((string (copy-seq string)))
  (loop
    (when (alpha-char-p (aref string 0))
      (return t))
    (setf string (subseq string 1)))
  string))

;;;---------------------------------------------------------------------------
;;; STRIP-WHITESPACE
;;;---------------------------------------------------------------------------

(defmethod STRIP-WHITESPACE ((s string))
  "Returns s trimmed of 'whitespace' on either end."
  (string-trim *whitespace* s))

;;;---------------------------------------------------------------------------
;;; CLEANUP-TOKEN
;;;---------------------------------------------------------------------------

(defmethod CLEANUP-TOKEN ((string STRING) &optional (remove-parentheses nil))
  (let ((start-index 0)
	(end-index (length string)))
    (declare (fixnum start-index end-index))
    (let ((new-string 
	   (if (and (> (- end-index start-index) 2)
		    (char= (elt string start-index) #\")
		    (char= (elt string (1- end-index)) #\"))
	     (subseq string (1+ start-index) (1- end-index))
	     (subseq string start-index end-index))))
      (setf new-string (strip-whitespace new-string))
      ;; Deal with parens
      (when remove-parentheses
	(when (and (> (length new-string) 1)
		   (char= (elt new-string 0) #\()
		   (char= (elt new-string (1- (length new-string))) #\)))
	  (when (> (length new-string) 1)
	    (setf new-string (subseq new-string 1 (- (length new-string) 2))))))
      ;; Return new-string
      new-string)))

;;;---------------------------------------------------------------------------

(defmethod CLEANUP-TOKEN ((thing T) &optional (remove-parentheses nil))
  (declare (ignore remove-parentheses))
  thing)

;;;---------------------------------------------------------------------------
;;; OBJECT-DISPLAY-VALUE
;;;---------------------------------------------------------------------------

(defmethod OBJECT-DISPLAY-VALUE ((list LIST))
  (cond ((atom list)
	 (object-display-value list))
	(t
	 (mapcar #'object-display-value list))))

;;;---------------------------------------------------------------------------

(defmethod OBJECT-DISPLAY-VALUE ((list NULL))
  nil)

;;;-----------------------------------------------------------------------------
;;; MAKE-TIMESTAMP-STRING
;;;-----------------------------------------------------------------------------

(defun MAKE-TIMESTAMP-STRING ()
  (multiple-value-bind (sec min hr day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec))
    (format nil "-~a-~a-~a-~a-~a" month day year hr min)))

;;;-----------------------------------------------------------------------------
;;; STRING-TO-KEYWORD
;;;-----------------------------------------------------------------------------

(defmethod STRING-TO-KEYWORD ((str STRING))
  (intern (string-upcase  (make-description-string (split-sequence #\space str)
						   :delimiter #\-))
	  :keyword))

;;;------------------------------------------------------------------------------
;;; CONVERT-TEXT-FILE-TO-LISP-FILE
;;;------------------------------------------------------------------------------

#+IGNORE
(defun CONVERT-TEXT-FILE-TO-LISP-FILE (filename &optional (split-fields-p nil))
  (let ((input-file (format nil "~a.txt" filename))
	(output-file  (format nil "~a.lisp" filename)))
    (setf input-file (make-data-pathname input-file))
    (setf output-file (make-data-pathname output-file))
    (with-open-file (file-1 input-file :direction :input :external-format :latin1)
      (with-open-file (file-2 output-file :direction :output :if-exists :supersede
		       :external-format :latin1)
	(format file-2 "(~%")
	(loop
	  (let ((next-line (read-line file-1 nil :eof)))
	    (when (eq next-line :eof)
	      (return t))
	    (setf next-line (string-right-trim '(#\Newline #\Return) next-line))
	    (cond ((not split-fields-p)
		   (format file-2 "(~s)~%" next-line))
		  (t
		   (setf next-line (split-sequence #\tab next-line))
		   (format file-2 "(")
		   (dolist (element next-line)
		     (format file-2 "~s "  (cleanup-token element)))
		   (format file-2 ")~%")))))
	(format file-2 ")~%")))
    t))

;;;---------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------
