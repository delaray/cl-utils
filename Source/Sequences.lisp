(in-package :Utilities)


;;;***************************************************************************
;;;
;;; PART I:   WHITESPACE & PUNCTUATION
;;;
;;; PART II:  SEQUENCE PREDICATES
;;;
;;; PART III: MATCHING SEQUENCES
;;;           1. MATCH-SEQUENCES
;;;           2. DIFF-SEQ
;;;
;;; PART IV:  SET OPERATIONS
;;;
;;; PART V:   MISCELLANEOUS
;;;
;;;***************************************************************************


;;; 6/12/08 mrh: This was being called with a symbol in the PDSO merge, so just in case:

;;; This is now used ouside the DSO module. Probably belongs in a separate VCN base utilities
;;; along wit the VCN math stuff. RPL, 8/29/09

(defun STRIP-VENCATNUM-PUNCTUATION (vencatnum)
  (when (not (stringp vencatnum))
    (setf vencatnum (coerce-to-string vencatnum)))
  (remove-if-not #'alphanumericp vencatnum))

;;;***************************************************************************
;;; PART I:   WHITESPACE & PUNCTUATION
;;;***************************************************************************


(defvar *ALL-SPECIAL-CHARS*
    '(#\. #\( #\) #\, #\; #\' #\" #\: #\` #\~ #\# #\@ #\! #\| #\\ #\{ #\}
      #\[ #\] #\$ #\% #\^ #\& #\* #\+ #\= #\< #\> #\/))

(defvar *MOST-SPECIAL-CHARS*
    '(#\. #\( #\) #\, #\; #\' #\" #\: #\` #\~ #\# #\@ #\! #\| #\\ #\{ #\}
      #\[ #\] #\$ #\^ #\& #\* #\+ #\= #\< #\>))

(defparameter *VOWEL-LIST* '(#\a #\e #\i #\o #\u
			     #\A #\E #\I #\O #\U))

(defvar *WHITESPACE* '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page))

;;;---------------------------------------------------------------------------

(defun WHITESPACE-CHAR-P (c)
  "This predicate creates returns T if <char> is either a blank, space, tab, 
  return, newline, or page character."
  (member c *whitespace* :test #'char=))

;;; elides punctuation completely, i.e. "medi-tek" -> "meditek".
(defun STRIP-PUNCTUATION (string)
  "Return string with all non-alphanumeric and non-whitespace chars removed."
  (declare (simple-string string))
  (with-output-to-string (new-string)
    (loop for c across string
        when (or (alphanumericp c)
                 (whitespace-char-p c))
        do (write-char c new-string))))

;;;--------------------------------------------------------------------------

;;; Like strip-punctuation, but replaces with spaces, then strips out
;;; continguous whitespace.
(defun REPLACE-PUNCTUATION (string)
  "Return string with all non-alphanumeric and non-whitespace chars removed,
   these characters being replaced with a (not multiple) blank."
  (declare (simple-string string))
  (string-trim
   *whitespace*  
   (with-input-from-string (s string)
     (with-output-to-string (new-string)
       (loop for c = (read-char s nil nil)
           while c
           when (alphanumericp c)
           do (write-char c new-string)
           else do (let ((next-char (peek-char nil s nil :eof)))
                     (when (and (not (eq next-char :eof))
				(alphanumericp next-char))
                       (write-char #\space new-string))))))))

;;;---------------------------------------------------------------------------

(defun STRIP-EMBEDDED-CRLF (string &optional (replace-with-space-p nil))
  "Remove embedded carriage returns or line feeds from this string."
  ;; 03/12/09 mrh: Removing simple-string declaration, because it's not always true.
  ;;               (Could look into where strings with fill pointers are coming from if needed.)
  ;; (declare (simple-string string))
  (flet ((crlf-p (c)
	   (or (char= c #\return) (char= c #\linefeed))))
    (if (not (or (position #\return string)
                 (position #\linefeed string)))
      ;; No problems, just return the string itself...
      string
      (with-output-to-string (new-string)
        (loop for c across string
            unless (crlf-p c)
            do (write-char c new-string)
            else do (when replace-with-space-p
                      (write-char #\space new-string)))))))

;;;---------------------------------------------------------------------------

(defvar *text-delimiters* (list #\; #\( #\) #\,))

(defun TEXT-DELIMITER-P (char)
  "Is char a text-delimiter?"
  (member char *text-delimiters* :test #'eq))


;;;---------------------------------------------------------------------------

(defmethod DELIMITER-P ((char CHARACTER))
  "Is char a known delimiter?"
  (member char *known-delimiters* :test #'eq))

;;;---------------------------------------------------------------------------

(defmethod DELIMITER-P ((str STRING))
  "Are all chars in str know delimiters?"
  (declare (simple-string str))
  (every #'delimiter-p str))

;;;---------------------------------------------------------------------------

(defmethod CONTAINS-DELIMITER-P ((str STRING))
  "Does str contain a known delimiter?"
  (declare (simple-string str))
  (some #'delimiter-p str))

;;;---------------------------------------------------------------------------
;;; SORT-LIST-BY-FREQUENCY
;;;---------------------------------------------------------------------------

(defun SORT-LIST-BY-FREQUENCY (list &key (test #'equal))
  "Return a copy of list with multiple copies of elements removed, but
   sorted so that the element(s) with the greatest number of copies
   appears first."
  (loop with hash = (make-hash-table :test test)
      for ele in list do
        (incf (gethash ele hash 0))
      finally
        (return
          (let 
              ((counts
                (sort 
                 (loop for ele being the hash-key of hash using 
                       (hash-value count)
                     collect (cons ele count))
                 #'>= :key #'cdr)))
            (map-into counts #'first counts)))))
;;;---------------------------------------------------------------------------
;;; COMBINE-RATIONAL-NUMBERS
;;;---------------------------------------------------------------------------

(defun COMBINE-RATIONAL-NUMBERS (tokens)
  "Return list of tokens with consecutive representations of rational
   numbers combined into decimal numbers.  
   E.g., (2 1/2 3 3 1/2) -> (2.5 3 3.5)"
  (let ((current-token nil)
	(next-token nil)
	(new-tokens nil))
    (loop
      (when (<& (length tokens) 2)
	(when (=& (length tokens) 1)
	  (push (first tokens) new-tokens))
	(return t))
      (setf current-token (first tokens))
      (setf next-token (second tokens))
      (cond ((and (numberp current-token)
		  (typep next-token 'RATIO))
	     (push (+$ (float current-token) (float next-token))
		   new-tokens)
	     (setf tokens (cddr tokens)))
	    (t
	     (push current-token new-tokens)
	     (setf tokens (cdr tokens)))))
    (nreverse new-tokens)))

;;; Similar to combine-rational-numbers, but also combines
;;; string tokens if they represent rational numbers.
;;; E.g. ("1" "1/4") -> (1.25).
;; Note: Can be optimized as non-paired integers + rationals are
;;       often re-parsed more than once.
(defun COMBINE-RATIONAL-NUMBERS* (tokens)
  "Return list of tokens with CONSECUTIVE representations of rational
   numbers combined into decimal numbers.  Non-numbers are left as strings."
  ;; E.g., ("2" "1/2" "3" "3" "1/2") -> (2.5 3 3.5)
  ;;       ("a" "1/2" "3" "3.b" "3" "1/2")) -> ("a" 0.5 3 "3.b" 3.5)
  (if (not (consp tokens))
    tokens
    (loop with result = '() until (null tokens)
        as token = (first tokens)
        as next-token = (second tokens)
        as token-number = (parse-rational-or-number token)
        as next-token-number = (parse-rational-or-number next-token) do
          (cond ((and (numberp token-number) (numberp next-token-number))
                 ;; In this context possibly-rational-number-representation-p
                 ;; is somewhat of a misnomer, as we know next-token is a
                 ;; number, therefore if this test succeeds, it must be a
                 ;; rational number.
                 (cond ((possibly-rational-number-representation-p next-token)
                        (setf tokens (cddr tokens))
                        (push (+$ (float token-number) 
                                  (float next-token-number)) result))
                       ((possibly-rational-number-representation-p 
                         token-number)
                        (push (float token-number) result)
                        (setf tokens (cdr tokens)))
                       (t (push token-number result)
                          (setf tokens (cdr tokens)))))
                 ((not (numberp token-number))
                  (and token-number (push token-number result))
                  (setf tokens (cdr tokens)))
                 ((not (numberp next-token-number))
                  (push token-number result)
                  (and next-token-number (push next-token-number result))
                  (setf tokens (cddr tokens))))
          finally (return (nreverse result)))))

#+IGNORE
(defun COMBINE-RATIONAL-NUMBERS* (tokens)
  "Return list of tokens with consecutive representations of rational
   numbers combined into decimal numbers.  
   E.g., (2 1/2 3 3 1/2) -> (2.5 3 3.5)"
  (let ((current-token nil)
	(next-token nil)
	(new-tokens nil))
    (loop
      (when (<& (length tokens) 2)
	(when (=& (length tokens) 1)
	  (push (first tokens) new-tokens))
	(return t))
      (setf current-token (first tokens))
      (setf next-token (second tokens))
      (cond ((and (numberp current-token)
		  (typep next-token 'RATIO))
	     (push (+$ (float current-token) (float next-token))
		   new-tokens)
	     (setf tokens (cddr tokens)))
	    ((and (stringp current-token)
		  (stringp next-token)
		  (digit-seq-p current-token)
		  (digit-seq-p next-token '(#\/)))
	     (let ((current-numb (read-from-string current-token))
		   (next-numb    (read-from-string next-token)))
	       (cond ((and (numberp current-numb)
			   (numberp next-numb))
		      (push (+$ (float current-numb) (float next-numb))
			    new-tokens)
		      (setf tokens (cddr tokens)))
		     (t
		      (push current-token new-tokens)
		      (setf tokens (cdr tokens))))))
	    ((and (stringp current-token)
		  (typep next-token 'RATIO)
		  (digit-seq-p current-token))
	     (let ((current-numb (read-from-string current-token)))
	       (cond ((numberp current-numb)
		      (push (+$ (float current-numb) (float next-token))
			    new-tokens)
		      (setf tokens (cddr tokens)))
		     (t
		      (push current-token new-tokens)
		      (setf tokens (cdr tokens))))))
	    (t
	     (push current-token new-tokens)
	     (setf tokens (cdr tokens)))))
    (nreverse new-tokens)))


;;;****************************************************************************
;;;
;;; PART II:  SEQUENCE PREDICATES
;;;
;;;****************************************************************************

;;;----------------------------------------------------------------------------
;;; PROBABILITY-TO-PERCENT
;;;----------------------------------------------------------------------------

(defun PROBABILITY-TO-PERCENT (prob)
  "Convert the representation of a probability to its percentage 
  representation, E.g., 1/5 -> 20.0"
  (/$ (float (round$ (*$ 10000.0 (float prob)))) 100.0))

;;;---------------------------------------------------------------------------
;;; DIGITS & NUMBERS
;;;---------------------------------------------------------------------------

;; RENAME to NUMERIC-STRING-P.  Won't work on lists.
;;PQ(10): (numeric-sequence-p "12.3%")
;;1

(defun NUMERIC-SEQUENCE-P (seq &key (allow-scientific-notation nil))
  "Does seq look like a numeric string?  Specifically, we allow +, -, x, . ,
   as the first element (as well as numbers), and % or . as well as numbers
   in the last position.  Everything in between should be numbers and
   possibly one decimal point.  If allow-scentific-notation is T, we
   allow for an E embedded in the string, but don't allow trailing %."
  (let ((last-pos (1- (length seq)))) ;position of last char, not end of seq.
    (and (sequence-contains-digit-p seq)
         (or (zerop last-pos) ;single numeric digit, success!
             (and (char-occurs-at-most-once-p #\. seq)
                  (sequence-begins-with-numeric-leader seq)
                  (sequence-ends-with-numeric-ender seq :last-pos last-pos)
                  (if (plusp (1- last-pos)) ;already checked start/end chars.
                    (digit-seq-p (subseq seq 1 last-pos)
                                 (if allow-scientific-notation 
                                   '(#\. #\e #\E)
                                   '(#\.)))
                    t)
                  (if allow-scientific-notation
                    (%valid-scientific-notation-string-p seq)
                    t))))))

(defun %valid-scientific-notation-string-p (seq)
  "This predicate returns T if seq looks like a reasonable scientific notation
   representation. It assumes that other checks have already been performed on
   seq: that it has no more than one decimal point, that it contains at least
   one digit, and that it begins and ends with 'reasonable' characters.
   See NUMERIC-SEQUENCE-P for usage."
  (let* ((upcase-seq (string-upcase seq))
         (e-pos (position #\E upcase-seq)))
    (and (char-occurs-at-most-once-p #\E upcase-seq)
         (when e-pos
           (and (not (find #\. upcase-seq :start e-pos))
                (digit-seq-p (subseq upcase-seq (1+ e-pos))))))))

(defun sequence-begins-with-numeric-leader (seq)
  "Characters that it's reasonable for a numeric sequence to begin with"
  (let ((elt (elt seq 0)))
    (or (numeric-char-p elt) ;number or decimal point
        (char= elt #\+)
        (char= elt #\-)
        (char-equal elt #\x))))

(defun sequence-ends-with-numeric-ender (seq &key last-pos)
  (let ((elt (elt seq (or last-pos (1- (length seq))))))
    (or (numeric-char-p elt) ;number or decimal point
        (char= elt #\%))))

;;;---------------------------------------------------------------------------

;; RENAME to STRING-CONTAINS-DIGIT-P.  Won't work on lists.
(defun SEQUENCE-CONTAINS-DIGIT-P (sequence)
  "Does sequence (actually string) contain a digit?"
  (loop for c across sequence
      when (digit-char-p c)
      return c
      finally (return nil)))

;;;---------------------------------------------------------------------------

;; RENAME to LIST-CONTAINS-NUMBER-P; not correct for strings.

(defun SEQUENCE-CONTAINS-NUMBER-P (sequence)
  "Does sequence (actually list) contain a number?"
  (some #'numberp sequence))

;;;---------------------------------------------------------------------------
;;; DIGIT-STRING-P
;;;---------------------------------------------------------------------------

(defun DIGIT-STRING-STRING-P (string)
  (every #'alpha-char-p string))

;;;---------------------------------------------------------------------------
;;; ALPHABETIC-STRING-P
;;;---------------------------------------------------------------------------

#-MEPERIA
(defun  ALPHABETIC-STRING-P (string)
  (every #'alpha-char-p string))

;;;---------------------------------------------------------------------------
;;; ALPHANUMERIC-STRING-P
;;;---------------------------------------------------------------------------

(defun ALPHANUMERIC-STRING-P (string)
  (every #'alphanumericp string))

;;;---------------------------------------------------------------------------

(defun STRING-CONTAINS-ALPHA-CHAR-P (string)
  "Does string contains a non-punctuation, non-digit character?"
  (loop for c across string
      when (alpha-char-p c)
      return c
      finally (return nil)))

;; This is obviously more elegant, but suffers in terms of both space and time.
;;;(defun STRING-CONTAINS-ALPHA-CHAR-P (string)
;;;  "Does string contains a non-punctuation, non-digit character?"
;;;  (some #'alpha-char-p string))

;;;---------------------------------------------------------------------------

;; LAK: Not sure what multiple values buys us here.  Could just be nil or pos.
(defun FIND-NUMBER-IN-LIST (number list)
  "If number is a member of list return the values number and its position."
  (let ((pos 0)
        (found nil))
    (dolist (item list)
      (when (equal item number)
	(setf found number)
	(return t))
      (incf& pos))
    (values found pos)))

;;;---------------------------------------------------------------------------

(defun FIND-NUMBERS-IN-LIST (list)
  "Return a list of numbers, possibly with duplicates, found in list."
  (let ((numbers nil))
    (dolist (item list)
      (when (numberp item)
	(push item numbers)))
    (nreverse numbers)))

;;;---------------------------------------------------------------------------

(defun FIND-FIRST-NUMBER-IN-LIST (list)
  "Return the first number found and its position in list."
  (let ((pos 0)
        (found nil))
    (dolist (item list)
      (when (numberp item)
	(setf found item)
	(return t))
      (incf& pos))
    (values found pos)))

;;;---------------------------------------------------------------------------

(defun SOME-NUMBER-IN-LIST-P (numbers list)
  "Returns the first number in numbers found in list or nil."
  (loop for number in numbers
      when (member number list :test #'same-value-p)
      return number
      finally (return nil)))

;;;---------------------------------------------------------------------------

(defun COUNT-NUMBERS-IN-LIST (list)
  "Returns the count of (non-unique) numbers in list."
  (let ((count 0))
    (dolist (item list)
      (when (numberp item)
	(incf& count)))
    count))

;;;---------------------------------------------------------------------------

(defun COUNT-X-IN-LIST (list)
  "Return the number of times either X or x appear in list."
  (let ((count 0))
    (dolist (item list)
      (when (and (stringp item) (string-equal item "X"))
	(incf& count)))
    count))

;;;---------------------------------------------------------------------------

(defun LIST-CONTAINS-EXACTLY-N-NUMBERS (n list)
  "Is n = the number of numbers in list?"
  (loop for x in list
      with count = 0
      when (numberp x) do
        (incf& count)
      when (>& count n)
      return nil
      finally (return (=& count n))))

;;;---------------------------------------------------------------------------
;;; NUMERIC-RANGE-P
;;;
;;; Detects "22-49" as a range.
;;; N.B. Also detects "22-49-" as a range.
;;; Does not detect "-49" as a range, nor "L22-49".
;;;---------------------------------------------------------------------------

(defun numeric-range-p (string)
  "Does string look like a numeric range?"
  (declare (simple-string string))
  (loop with number-found = nil and hyphen-found = 0
      for c across string do
      (cond ((char= c #\-)
             (if (or (=& hyphen-found 2) (not number-found))
               (return nil)
               (incf& hyphen-found)))
            ((numeric-char-p c)
             (if (=& hyphen-found 2) 
               (return nil)
               (setf number-found t)))
            (t (return nil)))
      finally
        (if (and number-found (plusp hyphen-found))
          (return t)
          (return nil))))

(defun numeric-char-p (x)
  "Is x either a digit or a period?"
  (or (digit-char-p x)
      (char= x #\.)))


;;; TODO: This only finds the first range in the string.
;;;  There could be more than one range, and we should
;;;  probably capture them all.
(defun NUMERIC-RANGE-START-END (string)
  "Returns the start and end positions of the first numeric range in string."
  (declare (simple-string string))
  (let ((start nil)
        (end nil)
        (centers (numeric-range-centers string))
        (hyphen-pos nil)
        (len (1-& (length string)))
        (continue-right t)
        (continue-left t))
    (cond (centers
           (setf hyphen-pos (first centers))
           (setq start (1-& hyphen-pos))
           (setq end (1+& hyphen-pos))
           (loop while (and (or continue-right continue-left)
                            (>& start 0)
                            (<& end len))
                 when (<& end len)
                 do (if (numeric-char-p (schar string (1+& end)))
                        (incf& end)
                      (setf continue-right nil))
                 when (>& start 0)
                 do (if (numeric-char-p (schar string (1-& start)))
                        (decf& start)
                      (setf continue-left nil))) ))
    (values start end)))

;;; Returns the positions, centered at a hyphen, where there
;;; is a pattern of the form "n-n", where n is a digit.
;;;
;;; HASTY-BUG: For a string of the form "foo-22-33-44-55-bar",
;;;  this finds the centers at (6 9 12).  What we probably
;;;  should do is leave out the center at 9, since the hyphen
;;;  at location 9 is really separating 2 ranges (well, that's
;;;  one interpretation.
(defun NUMERIC-RANGE-CENTERS (string)
  "Returns the positions, centered at a hyphen, where there
   is a pattern of the form 'n-n', where n is a digit."
  (loop for position in (positions #\- string :test #'char=)
      when (%CONTAINS-NUMERIC-RANGE-P string position)
           collect position))

(defun CONTAINS-NUMERIC-RANGE-P (string)
  "Does string contain a numeric range?"
  (some #'(lambda (p) (%CONTAINS-NUMERIC-RANGE-P string p))
        (positions #\- string :test #'char=)))

;;; HASTY-BUG: this fails if the range has decimal numbers.
;;  LAK - Don't see the bug w.r.t. decimal numbers.
(defun %CONTAINS-NUMERIC-RANGE-P (string &optional hyphen-pos)
  "This finds the first pattern of digit-digit within
   the string, or verifies one if given the location of the hyphen."
  (declare (simple-string string))
  (let ((mid-pos (or hyphen-pos (position  #\- string))))
    (and mid-pos
         (>& mid-pos 0)
         (<& mid-pos (1-& (length string)))
         (numeric-char-p (schar string (1-& mid-pos)))
         (numeric-char-p (schar string (1+& mid-pos))))))

(defmethod POSITIONS (item (sequence LIST) &key (start 0) (test #'char=))
  "Returns a list of all the positions of item in the list."
  (loop for pos from 0
      for char in sequence
      when (and (>=& pos start) (funcall test item char))
      collect pos))

(defmethod POSITIONS (item (sequence VECTOR) &key (start 0)(test #'char=))
  "Returns a list of all the positions of item in the vector"
  (loop for pos from 0
      for char across sequence
      when (and (>=& pos start) (funcall test item char))
      collect pos))

;;;---------------------------------------------------------------------------
;;; CHARACTERS IN SEQUENCES
;;;---------------------------------------------------------------------------

(defmethod ALPHA-SEQ-P ((seq STRING) &optional (special-chars nil))
  "Is every character in seq an alpha char or in the list of special-chars?"
  (every #'(lambda (char)
	     (or (alpha-char-p char)
		 (member char special-chars :test #'char=)))
	 seq))


;;;-----------------------------------------------------------------------------
;;; DIGIT-SEQ-P
;;;-----------------------------------------------------------------------------
;;; This was in PQ-Sizes.lisp
;;; This also counts the number of special-chars while traversing
;;; the string, and returns this count as the truth value.

;; Returned T previously for empty string!!
(defmethod DIGIT-SEQ-P ((seq STRING) &optional (special-chars nil))
  "Is every character in seq a digit char or in the list of special-chars?"
  (and (not (empty-string-p seq))
       (let* ((special-count 0)
              (ans (every #'(lambda (char)
                              (or (digit-char-p char)
                                  (if (member char special-chars 
                                              :test #'char=)
                                    (incf special-count)
                                    nil)))
                          seq)))
         (if ans special-count nil))))

;;;---------------------------------------------------------------------------

(defun CHAR-OCCURS-AT-MOST-ONCE-P (char string)
  "Does char occur no more than once (possibly 0) times in string?"
  (loop for c across string
      with count = 0
      when (char= c char) do
        (incf& count)
      when (>& count 1)
      return nil
      finally (return t)))

;;;---------------------------------------------------------------------------

;; RENAME to STRING-CONTAINS-CHAR-P
(defun SEQUENCE-CONTAINS-CHAR-P (string char)
  "Is char found in string?"
  (find char string :test #'char=))

;;;---------------------------------------------------------------------------

;; RENAME to STRING-CONTAINS-SOME-CHARS-P
(defun SEQ-HAS-SOME-CHARS-P (string chars)
  "Is at least one of the characters in chars found in string?"
  (some #'(lambda (char) (find char string :test #'char=))
	chars))
  
;;;---------------------------------------------------------------------------
;;;  STRIP-LEADER
;;;---------------------------------------------------------------------------

;;; This is poorly named, as no stripping takes place.
;;; This bumps start to the first non-space or non-tab character in the token.

(defun STRIP-LEADER (token start)
  "Returns the starting position beginning of start of the first non-tab,
   non-blank character in token."
  (loop
    (cond ((>=& start (length token))
	   (return t))
	  ((char= (elt token start) #\tab)
	   (incf& start))
	  ((char= (elt token start) #\space)
	   (incf& start))
	  (t
	   (return t))))
  (min& start (length token)))

;; Should be the following, and called differently:
;; Commit this, only in conjunction with a change to
;; cleanup-token.  (But if we use "new" cleanup-token, this and next func.
;; become unnecessary.
(defun STRIP-LEADER-new (token start)
  ;; 2/6/10 mrh:  adding in newline and return to this.
  (string-left-trim '(#\tab #\space #\return #\newline)
		    (subseq token start)))

;;;---------------------------------------------------------------------------
;;;  STRIP-ENDER
;;;---------------------------------------------------------------------------

(defun STRIP-ENDER (token)
  "Strips tabs & spaces from the end of token."
  ;; 2/6/10 mrh:  adding in newline and return to this.
  (string-right-trim '(#\tab #\space #\return #\newline) token))

;;;---------------------------------------------------------------------------
;;;  REMOVE-DELIMITERS
;;;---------------------------------------------------------------------------

;; Doesn't look like this is called anywhere.  Can obj be a list?
(defmethod REMOVE-DELIMITERS ((obj SEQUENCE)
			      &optional
			      (delimiters *known-delimiters*)
			      (sequence-type 'STRING))
  "Return a new sequence with delimiters removed"
  (let ((new-sequence nil))
    (dotimes (i (length obj))
      (unless (member (elt obj i) delimiters :test #'eq)
	(push (format nil "~a" (elt obj i)) new-sequence)))
    (setf new-sequence (reverse new-sequence))
    (cond ((>& (length new-sequence) 1)
	   (seq-list-to-seq new-sequence sequence-type))
	  (t
	   (first new-sequence)))))

;;;---------------------------------------------------------------------------
;;; SUBSEQ-P
;;;---------------------------------------------------------------------------

(defmethod SUBSEQ-P ((seq1 SEQUENCE)(seq2 SEQUENCE))
  "Is seq1 a subsequence of seq2?  If so, return T and the start/end positions
   within seq2."
  (when (>& (length seq1) (length seq2))
    (psetf seq1 seq2 seq2 seq1))
  (let ((start (search seq1 seq2)))
    (if start
      (values t start (+& start (length seq1)))
      (values nil 0 0))))

;;;---------------------------------------------------------------------------
;;; STARTS-SEQ-P
;;;---------------------------------------------------------------------------

(defun STARTS-SEQ-P (item sequence)
  "Does item start sequence?  Case sensitive!"
  ;; 03/02/09 mrh: Checking to make sure "sequence" is longer than "item", to make SBCL happier.
  ;; 03/30/10 mrh: Changing argument names for clarity.
  (let ((seq-len (length sequence))
	(item-len (length item)))
    (when (>=& seq-len item-len)
      (search item sequence :test #'same-value-p :end2 item-len))))

;;;---------------------------------------------------------------------------
;;; ENDS-SEQ-P
;;;---------------------------------------------------------------------------

(defmethod ENDS-SEQ-P ((sub-sequence T) (full-sequence T))
  "Return two values: whether or not sub-sequence ends full-sequence, and the position if so."
  ;; mrh 2010-06-30 - Slightly modified from RADT reporting code in demo-utilties.lisp.
  (let ((full-len (length full-sequence))
	(sub-len (length sub-sequence)))
    (when (>= full-len sub-len)
      (let ((possible-position (- full-len sub-len)))
	(let ((match (equal (subseq full-sequence possible-position) sub-sequence)))
	  (values
	   match
	   (when match possible-position)))))))

(defmethod ENDS-SEQ-P ((sub-sequence STRING) (full-sequence STRING))
  "Return two values: whether or not sub-sequence ends full-sequence, and the position if so.  Case sensitive."
  ;; mrh 2010-06-30 - This started out as a copy of a function from RADT reporting code in
  ;;                  demo-utilties.lisp, but I've changed it to not use subseq (no returning new string).
  ;;                  In very limited tests, this seems to run about as quickly but create less garbage.
  (let ((full-len (length full-sequence))
	(sub-len (length sub-sequence)))
    (when (>= full-len sub-len)
      (let ((possible-position (- full-len sub-len))
	    (match? t))
	(dotimes (i sub-len)
	  (when (not (char= (schar sub-sequence i)
			    (schar full-sequence (+ i possible-position))))
	    (setf match? nil)
	    (return)))
	(values
	 match?
	 (when match? possible-position))))))

;;;---------------------------------------------------------------------------
;;; REMOVE-SUBLIST
;;;---------------------------------------------------------------------------

(defun REMOVE-SUBLIST (list sublist)
  (let ((start (search sublist list :test #'same-value-p)))
    (if start ; We know that sublist is a sublist of list...
      (let* ((end (+& start (length sublist)))
             (seq1 (subseq list 0 start))
             (seq2 (subseq list end)))
        ;; subseq always allocates a new sequence, so:
        (nconc seq1 seq2))
      list)))
            
;;;---------------------------------------------------------------------------
;;; REMOVE-SUBSEQ
;;;---------------------------------------------------------------------------

(defun REMOVE-SUBSTRING (seq start end)
  "Return a string representing the subsequence between start and end
   removed from the sequence seq."
  (concatenate 'STRING
    (subseq seq 0 start)
    (subseq seq end)))

;;;---------------------------------------------------------------------------
;;; STRING-LIST-TO-STRING
;;;---------------------------------------------------------------------------

(defun SEQ-LIST-TO-SEQ (seq-list &optional (seq-type 'STRING))
  "Concatenate list of sequences seq-list into a single sequence of type 
   seq-type."
  (if (eq seq-type 'string)
      (with-output-to-string (*standard-output*)
	(mapc #'efficient-print seq-list))
      (apply #'concatenate seq-type 
	     (mapcar #'coerce-to-string seq-list))))

;;;---------------------------------------------------------------------------
;;; BEST-SCORE
;;;---------------------------------------------------------------------------

(defun BEST-SCORE (list &key (key #'identity))
  "Return the object in list which scores best according to key, which
   defaults to #'identity."
  (when list
    (loop with best = (car list)
       with best-score = (funcall key best)
       for ele in (cdr list)
       for score = (funcall key ele)
       do (when (> score best-score)
	        (setf best ele best-score score))
       finally (return best))))

;;;***************************************************************************
;;; PART III: SEQUENCE MATCHING
;;;***************************************************************************

;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :BASIC (String)
;;;---------------------------------------------------------------------------

(defmethod MATCH-SEQUENCES ((string1 string) (string2 string)
                            (match-type (eql :basic)))
  (bigram-similarity string1 string2
		     :split-fn (lambda (x) (bigram-default-split x t))))

;; LH2010-09-22: This ad-hoc algorithm has been replaced by bigram-similarity 

;; This is just a cleaner rewrite of match-sequences below, specialised to
;; string.  The main speed improvement is removing an extraneous call
;; to find (when just one call to position would do).  position takes
;; about 80% of the time.
;; it passed a regression test of over 250000 test-units.

;; Lucas Hope (12/07/07)

#+ignore
(defmethod MATCH-SEQUENCES ((string1 string) (string2 string)
                            (match-type (eql :basic)))
  (declare (simple-string string1 string2))
  (loop with len1 = (length string1) and len2 = (length string2)
     and elt-matches = nil and match-count = 0
     for index1 below len1 
     for index2 below len2
     do (if (same-value-p (char string1 index1)
			  (char string2 index2))
	    (progn (push (list index1 index2) elt-matches)
		   (incf& match-count))
	    ;; Skip to the next elt in seq1 that matches an elt in seq2
	    (loop for i1 from index1 below len1
	       for match = (position (char string1 i1) string2
				     :start index2 :test #'same-value-p)
	       when match do
		 (setf index2 match)
		 (push (list i1 index2) elt-matches)
		 (incf& match-count)
		 (loop-finish)
	       finally (setf index1 i1)))
     ;; Return the probability & match-count
     finally 
        (return (values (*$ 0.5 (+$ (/$ (float match-count) (float len1))
                                    (/$ (float match-count) (float len2))))
		       match-count
		       (nreverse elt-matches)))))


;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :BASIC (Sequences)
;;;---------------------------------------------------------------------------

;; Could be sped up (see Luke's comment above), but probably little used
;; given his specialization on strings.
(defmethod MATCH-SEQUENCES ((seq1 SEQUENCE)(seq2 SEQUENCE)
                            (match-type (eql :basic)))
  ;; LH2010-09-22: Added deprecation warning due to a change to
  ;; LH2010-09-22: bigram-similarity for the string implementation.
  (warn "MATCH-SEQUENCES (seq seq :basic) is deprecated. Can you coerce to string?")
  (let ((elt-matches nil)
        (match-count 0)
        (match-prob 0.0)
        (index1 0)
        (index2 0)
	(len1 (length seq1))
	(len2 (length seq2)))
    (loop 
      (when (or (>=& index1 len1)
		(>=& index2 len2))
	(return t))
      (cond ((same-value-p (elt seq1 index1)(elt seq2 index2))
	     (push (list index1 index2) elt-matches)
	     (incf& index1)
	     (incf& index2)
	     (incf& match-count))
	    (t
	     ;; Skip to the next elt in seq1 that matches an elt in seq2
	     (loop
	       (when (or (>=& index1 len1)
			 (>=& index2 len2))
		 (return t))
	       (cond ((find (elt seq1 index1) seq2
			    :start index2
			    :test #'same-value-p)
		      (setf index2
			(position (elt seq1 index1) seq2
				  :start index2
				  :test #'same-value-p))
		      (push (list index1 index2) elt-matches)
		      (incf& index1)
		      (incf& index2)
		      (incf& match-count)
		      (return t))
		     (t
		      (incf& index1)))))))
    (setf match-prob
          (*$ 0.5 (+$ (/$ (float match-count) (float len1))
                      (/$ (float match-count) (float len2)))))
    ;; Restore order
    (setf elt-matches (nreverse elt-matches))
    ;; Return the probability & match-count
    (values match-prob match-count elt-matches)))

;;;---------------------------------------------------------------------------

;;; This :around method traps the cases where one or both of the sequences are
;;; empty. We return a match probability of 0 in this case.

(defmethod MATCH-SEQUENCES :around ((seq1 SEQUENCE)(seq2 SEQUENCE)
                                    (match-type (eql :basic)))
  (cond ((or (zerop& (length seq1))(zerop& (length seq2)))
	 (values 0.0 0))
	(t 
	 (call-next-method))))

;;;---------------------------------------------------------------------------

;;; This ensures case insensitive matching for strings. The only drawback is
;;; method runs twice for the match type :spelling, but we cannot
;;; confine it to :basic because of the use of 'equal' in the method on
;;; :spelling  match type. 

;;; For now this is ok and doesn't require sequence to be a string in the
;;; more general case.

(defmethod MATCH-SEQUENCES :around ((seq1 STRING)(seq2 STRING)(match-type t))
  #+IGNORE
  (setf seq1 (string-upcase seq1))
  #+IGNORE
  (setf seq2 (string-upcase seq2))
  (call-next-method seq1 seq2 match-type))

;;;----------------------------------------------------------------------------

(defmethod MATCH-SEQUENCES ((value1 VALUE)(value2 VALUE)(match-type t))
  (match-sequences (%object-value value1)(%object-value value2) match-type))

;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :VARIABLE-LENGTH
;;;---------------------------------------------------------------------------
;;; this is a variation on :basic which allows us to compare strings with no penalty if
;;; one is basically a substring of the other.
(defmethod MATCH-SEQUENCES ((string1 string) (string2 string)
                            (match-type (eql :variable-length)))
  (declare (simple-string string1 string2))
  (loop with len1 = (length string1) and len2 = (length string2)
     and elt-matches = nil and match-count = 0
     for index1 below len1 
     for index2 below len2
     do (if (same-value-p (char string1 index1)
			  (char string2 index2))
	    (progn (push (list index1 index2) elt-matches)
		   (incf& match-count))
	    ;; Skip to the next elt in seq1 that matches an elt in seq2
	    (loop for i1 from index1 below len1
	       for match = (position (char string1 i1) string2
				     :start index2 :test #'same-value-p)
	       when match do
		 (setf index2 match)
		 (push (list i1 index2) elt-matches)
		 (incf& match-count)
		 (loop-finish)
	       finally (setf index1 i1)))
     ;; Return the probability & match-count
     finally 
        (return (values (/$ (float match-count) (float (min len1 len2)))
		       match-count
		       (nreverse elt-matches)))))

;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :VARIABLE-LENGTH (Sequences)
;;;---------------------------------------------------------------------------

;; Could be sped up (see Luke's comment above), but probably little used
;; given his specialization on strings.
(defmethod MATCH-SEQUENCES ((seq1 SEQUENCE)(seq2 SEQUENCE)
                            (match-type (eql :variable-length)))
  (let ((elt-matches nil)
        (match-count 0)
        (match-prob 0.0)
        (index1 0)
        (index2 0)
	(len1 (length seq1))
	(len2 (length seq2)))
    (loop 
      (when (or (>=& index1 len1)
		(>=& index2 len2))
	(return t))
      (cond ((same-value-p (elt seq1 index1)(elt seq2 index2))
	     (push (list index1 index2) elt-matches)
	     (incf& index1)
	     (incf& index2)
	     (incf& match-count))
	    (t
	     ;; Skip to the next elt in seq1 that matches an elt in seq2
	     (loop
	       (when (or (>=& index1 len1)
			 (>=& index2 len2))
		 (return t))
	       (cond ((find (elt seq1 index1) seq2
			    :start index2
			    :test #'same-value-p)
		      (setf index2
			(position (elt seq1 index1) seq2
				  :start index2
				  :test #'same-value-p))
		      (push (list index1 index2) elt-matches)
		      (incf& index1)
		      (incf& index2)
		      (incf& match-count)
		      (return t))
		     (t
		      (incf& index1)))))))
    (setf match-prob
          (/$ (float match-count) (float (min len1 len2))))
    ;; Restore order
    (setf elt-matches (nreverse elt-matches))
    ;; Return the probability & match-count
    (values match-prob match-count elt-matches)))

;;;---------------------------------------------------------------------------

;;; This :around method traps the cases where one or both of the sequences are
;;; empty. We return a match probability of 0 in this case.

(defmethod MATCH-SEQUENCES :around ((seq1 SEQUENCE)(seq2 SEQUENCE)
                                    (match-type (eql :variable-length)))
  (cond ((or (zerop& (length seq1))(zerop& (length seq2)))
	 (values 0.0 0))
	(t 
	 (call-next-method))))
;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :SPELLING
;;;---------------------------------------------------------------------------

(defmethod MATCH-SEQUENCES ((seq1 SEQUENCE)(seq2 SEQUENCE)
                            (match-type (eql :spelling)))
  (cond ((or (zerop& (length seq1))(zerop& (length seq2)))
	 (values 0.0 0))
	(t
	 (multiple-value-bind (match-prob match-count)
	     (match-sequences seq1 seq2 :basic)
	   (let ((start-factor .5)
                 (subseq-factor .5))
	     (if (equal (elt seq1 0)(elt seq2 0))
               (setf start-factor 1.0)
	       (setf start-factor 0.0))
	     (when (starts-seq-p seq1 seq2)
	       (setf subseq-factor 1.0))
	     (setf match-prob 
               (*$ 0.25 (+$ (float match-prob)
                            (float match-prob)
                            start-factor subseq-factor)))
	     (values match-prob match-count))))))

;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES: :SEQUENCED
;;;---------------------------------------------------------------------------

;;; In this version of MATCH-SEQUENCES bias is given to the same ordering.
(defmethod MATCH-SEQUENCES ((seq1 SEQUENCE)(seq2 SEQUENCE)
                            (match-type (eql :sequenced)))
  (let ((match-count 0))
    (dotimes (i (min& (length seq1)(length seq2)))
      (when (equal (elt seq1 i)(elt seq2 i))
	(incf& match-count)))
    (values (/$ (float match-count) (float (min& (length seq1)(length seq2))))
	    match-count)))

;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :WEIGHTED
;;;---------------------------------------------------------------------------

;;; Let D = n + (n-1) + ... + 2 + 1, then for a sequence of length n, the
;;; ith element is weighted by: (n - i)/D
;;;
;;; Examples:
;;;
;;; Seq1 = "abcd" & Seq2 = "abcd" then match-prob = 1.0
;;; Seq1 = "abcx" & Seq2 = "abcd" then match-prob = 0.9
;;; Seq1 = "xbcx" & Seq2 = "abcd" then match-prob = 0.6

(defmethod MATCH-SEQUENCES ((seq1 SEQUENCE)(seq2 SEQUENCE)
                            (match-type (eql :weighted)))
  (let* ((size (min& (length seq1)(length seq2)))
	 ;; Sum of an arithmetic progression
	 (denominator (/$ (float (*& size (1+& size))) 2.0))
	 (match-count 0)
	 (match-prob 0.0))
    (dotimes (i size)
      (when (equal (elt seq1 i)(elt seq2 i))
	(incf& match-count)
	(incf$ match-prob (/$ (float (-& size i)) denominator))))
    (values match-prob match-count)))


;;;---------------------------------------------------------------------------
;;; MATCH-SEQUENCES :ALIAS
;;;---------------------------------------------------------------------------

(defmethod MATCH-SEQUENCES ((seq1 SEQUENCE) (seq2 SEQUENCE) 
                            (match-type (eql :alias)))
  "Perform a match by removing vowels from sequences, and then see how 
   the :spelling match works."
   (cond ((or (zerop& (length seq1)) (zerop& (length seq2)))
          (values 0.0 0))
         (t
          (match-sequences (remove-vowels seq1)
                           (remove-vowels seq2)
                           :spelling))))

;;;---------------------------------------------------------------------------

(defvar *very-similar-words-threshold* 0.95
  "This value seems in practice to pick up words that are the same but
   slightly misspelled.  It also picks up some singular/plural combinations,
   but in the context of repairing noun-types, they should be repaired
   before redundant noun-types.  Lower values than this picked up too
   many false positives in practice.")

(defun very-similar-words-p (word1 word2 
                             &key (threshold *very-similar-words-threshold*)
                                  (match-types '(:basic :spelling)))
  (loop for match-type in match-types
      when (> (match-sequences word1 word2 match-type) threshold)
      return t
      finally (return nil)))

;;;---------------------------------------------------------------------------

(defmethod vowel-p ((c character))
  "Is c a vowel?"
  (member c *vowel-list* :test #'char=))

(defmethod REMOVE-VOWELS ((seq STRING))
  "Return the string seq with the vowels removed."
  (declare (simple-string seq))
  (with-output-to-string (new-string)
    (loop for c across seq
        unless (vowel-p c)
        do (write-char c new-string))))

(defmethod REMOVE-VOWELS ((seq LIST))
  "Returns the list seq with the vowels removed from every string in list."
   (mapcar #'remove-vowels seq))

;;;---------------------------------------------------------------------------
;;; DIFF-SEQS
;;;---------------------------------------------------------------------------

(defun DIFF-SEQS (seq1 seq2)
  "Returns the first position in seq1 that differs from seq2"
  (let ((pos (min (length seq1)(length seq2))))
    (dotimes (i (min& (length seq1)(length seq2)))
      (unless (equal (elt seq1 i)(elt seq2 i))
	(setf pos i)
	(return pos)))
    pos))

;;; ONLY works for strings!!! Rule it out.
(defun diff-seq-strings (seq1 seq2)
  (loop with index = 0
      for c1 across seq1
      for c2 across seq2
      when (char/= c1 c2)
      return index
      else do (incf& index)
      finally (return index)))
;;;---------------------------------------------------------------------------
;;; EXPLODE-STRING
;;;---------------------------------------------------------------------------

;;;#-ALLEGRO
(defun explode-string (string &optional (delimiter #\Space))
  (declare (simple-string string))
  (let* ((pos nil)
         (result (loop while (setf pos (position delimiter string))
                     when (>& pos 0) ;; string is "<delimiter>something"
                     collect (subseq string 0 pos)
                     do (setq string (subseq string (1+& pos))))))
    ;; check case where string is "something<delimiter>"
    (unless (empty-string-p string)
      (setq result (nconc result (list string))))
    result))

;;; This version of explode-string has the option to split on the delimiter, 
;;; but keep the delimiter in the token sequence.  This may be used for
;;; processing numeric ranges.
;;;
;;; For example,
;;;
;;;  (explode-string "-foo-bar-blech-" #\- )
;;; ("foo" "bar" "blech")
;;;
;;;  (explode-string "-foo-bar-blech-" #\-  t)
;;; ("-" "foo" "-" "bar" "-" "blech" "-")

#+IGNORE
(defun explode-string (string &optional (delimiter #\Space)
                              (keep-delimiter-token-p nil))
  (declare (simple-string string))
  (let ((pos nil)
        (result (loop while (setq pos (position delimiter string))
                    when (>& pos 0) ;; string is "<delimiter>something"
                    collect (subseq string 0 pos)
                    when keep-delimiter-token-p
                    collect (subseq string pos (1+& pos))
                    do (setq string (subseq string (1+& pos))))))
    (if (not (string= *null-string* string))
      (setq result (nconc result (list string))))
    result))

;;;-----------------------------------------------------------------------------

#+IGNORE
;;; BUG: This errors out for (explode-string "L-" #\-)
(defun EXPLODE-STRING (string &optional (delimiter #\Space))
  (tpl::explode-string string delimiter))

;;;---------------------------------------------------------------------------
;;; COMPARE-STRINGS (Shweta)
;;;---------------------------------------------------------------------------

;;; LAK - see Open Question #5.

;;; Need to replace the char-code with meaningful symbols and need a 
;;; of exactly what this function does. (Ray 11/23/07)

;; This is seriously messed up, particularly the tests that something
;; is = to 58 AND 59!!!
;;;(defun COMPARE-STRINGS (str1 str2)
;;;  (declare (simple-string str1 str2))
;;;  (let ((index1 0)
;;;        (index2 0))
;;;    (loop
;;;      (when (and (>=& index1 (length str1)) (>=& index2 (length str2)))
;;;	(return t))
;;;      (when (or (>=& index1 (length str1)) (>=& index2 (length str2)))
;;;	(return-from compare-strings nil))
;;;      (let ((char1 (char-upcase (schar str1 index1)))
;;;            (char2 (char-upcase (schar str2 index2))))
;;;	(cond ((equal char1 char2)
;;;	       (incf& index1) 
;;;	       (incf& index2))
;;;	      ((or (and (>=& (char-code char1) 32)
;;;			(<=& (char-code char1) 47))
;;;		   (and (>=& (char-code char1) 91)
;;;			(<=& (char-code char1) 96))
;;;		   (and (=& (char-code char1) 58)
;;;			(=& (char-code char1) 59)))
;;;	       (incf& index1))
;;;	      ((or (and (>=& (char-code char2) 32)
;;;			(<=& (char-code char2) 47))
;;;		   (and (>=& (char-code char2) 91)
;;;			(<=& (char-code char2) 96))
;;;		   (and (=& (char-code char2) 58)
;;;			(=& (char-code char2) 59)))
;;;	       (incf& index2))
;;;	      (t
;;;	       (return-from compare-strings nil)))))))

;;;(defun COMPARE-STRINGS (str1 str2)
;;;  (declare (simple-string str1 str2))
;;;  (let ((index1 0)
;;;        (index2 0))
;;;    (loop
;;;      (when (and (>=& index1 (length str1)) (>=& index2 (length str2)))
;;;	(return t))
;;;      (when (or (>=& index1 (length str1)) (>=& index2 (length str2)))
;;;	(return-from compare-strings nil))
;;;      (let ((char1 (char-upcase (schar str1 index1)))
;;;            (char2 (char-upcase (schar str2 index2))))
;;;	(cond ((equal char1 char2)
;;;	       (incf& index1) 
;;;	       (incf& index2))
;;;	      ((or (and (char>= char1 #\Space)
;;;			(char<= char1 #\/))
;;;		   (and (char>= char1 #\[)
;;;			(char<= char1 #\`))
;;;		   (char= char1 #\:)
;;;                   (char= char1 #\;))
;;;	       (incf& index1))
;;;	      ((or (and (char>= char2 #\Space)
;;;			(char<= char2 #\/))
;;;		   (and (char>= char2 #\[)
;;;			(char<= char2 #\`))
;;;		   (char= char2 #\:)
;;;                   (char= char2 #\;))
;;;	       (incf& index2))
;;;	      (t
;;;	       (return-from compare-strings nil)))))))

(defun COMPARE-STRINGS (str1 str2)
  "Are str1 and str2 the same after punctuation is removed?"
  (string= (remove-all-punctuation str1)
           (remove-all-punctuation str2)))

(defun remove-all-punctuation (string)
  "Return string with all characters that are not alpaha-numeric removed."
  (remove-if-not #'alphanumericp string))

;;;***************************************************************************
;;; PART IV:  SET OPERATIONS
;;;***************************************************************************

;;;---------------------------------------------------------------------------
;;; SINGLETON-P
;;;---------------------------------------------------------------------------

(defun SINGLETON-P (set)
  "Does set contain exactly one element?"
  (list-length-1-p set))


;;;***************************************************************************
;;; PART V:  MISCELLANEOUS
;;;***************************************************************************

(defun ORDER-OF-MAGNITUDE (number)
  "This function returns the number of base 10 digits in <number>."
  ;; mrh: Why doesn't this use log10?
  (let ((magnitude 0))
    (loop
      (when (< number 1)
	(return t))
      (incf& magnitude)
      (setf number (truncate number 10)))
    magnitude))

;;;---------------------------------------------------------------------------
;;; FLATTEN-LIST
;;;---------------------------------------------------------------------------

(defun flatten-list (list)
  "This function 'flattens' all embedded list structure in list, returning
   a new list."
  (loop for element in list nconc
        (if (consp element) 
          (flatten-list element) 
          (list element))))

;;;---------------------------------------------------------------------------
;;; SPLIT-SEQUENCE

;;; Note: When this is committed, remove the KT file from svn !!!!!
;;;---------------------------------------------------------------------------
;;;
;;; This code was based on Arthur Lemmens' in
;;; <URL:http://groups.google.com/groups?as_umsgid=39F36F1A.B8F19D20%40simplex.nl>;
;;;
;;; changes include:
;;;
;;; * altering the behaviour of the :from-end keyword argument to
;;; return the subsequences in original order, for consistency with
;;; CL:REMOVE, CL:SUBSTITUTE et al. (:from-end being non-NIL only
;;; affects the answer if :count is less than the number of
;;; subsequences, by analogy with the above-referenced functions).
;;;   
;;; * changing the :maximum keyword argument to :count, by analogy
;;; with CL:REMOVE, CL:SUBSTITUTE, and so on.
;;;
;;; * naming the function SPLIT-SEQUENCE rather than PARTITION rather
;;; than SPLIT.
;;;
;;; * adding SPLIT-SEQUENCE-IF and SPLIT-SEQUENCE-IF-NOT.
;;;
;;; * The second return value is now an index rather than a copy of a
;;; portion of the sequence; this index is the `right' one to feed to
;;; CL:SUBSEQ for continued processing.

;;; There's a certain amount of code duplication here, which is kept
;;; to illustrate the relationship between the SPLIT-SEQUENCE
;;; functions and the CL:POSITION functions.

;;; Examples:
;;;
;;; * (split-sequence #\; "a;;b;c")
;;; -> ("a" "" "b" "c"), 6
;;;
;;; * (split-sequence #\; "a;;b;c" :from-end t)
;;; -> ("a" "" "b" "c"), 0
;;;
;;; * (split-sequence #\; "a;;b;c" :from-end t :count 1)
;;; -> ("c"), 4
;;;
;;; * (split-sequence #\; "a;;b;c" :remove-empty-subseqs t)
;;; -> ("a" "b" "c"), 6
;;;
;;; * (split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("" "" "r" "c" "d" "" "r" ""), 11
;;;
;;; * (split-sequence-if-not (lambda (x) (member x '(#\a #\b))) "abracadabra")
;;; -> ("ab" "a" "a" "ab" "a"), 11 
;;;
;;; * (split-sequence #\; ";oo;bar;ba;" :start 1 :end 9)
;;; -> ("oo" "bar" "b"), 9

;; cl-utilities note: the license of this file is unclear, and I don't
;; even know whom to contact to clarify it. If anybody objects to my
;; assumption that it is public domain, please contact me so I can do
;; something about it. Previously I required the split-sequence
 ; package as a dependency, but that was so unwieldy that it was *the*
;; sore spot sticking out in the design of cl-utilities. -Peter Scott

;;(in-package :utils-kt)

;;(export! split-sequence split-sequence-if split-sequence-if-not)

#+IGNORE
(defun split-sequence (delimiter seq &key (count nil) 
                                          (remove-empty-subseqs nil) 
                                          (from-end nil) 
                                          (start 0) (end nil) 
                                          (test nil test-supplied) 
                                          (test-not nil test-not-supplied) 
                                          (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (with-full-optimization ()
    (let ((len (length seq))
          (other-keys (nconc (when test-supplied 
                               (list :test test))
                             (when test-not-supplied 
                               (list :test-not test-not))
                             (when key-supplied 
                               (list :key key)))))
      (unless end (setq end len))
      (if from-end
          (loop 
              for right = end then left
              for left = (max& (or (apply #'position delimiter seq 
                                          :end right
                                          :from-end t
                                          other-keys)
                                   -1)
                               (1-& start))
              unless (and (=& right (1+& left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>=& nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+& left) right) into subseqs
              and sum 1 into nr-elts
              until (<& left start)
              finally (return (values (nreverse subseqs) (1+& left))))
          (loop
              for left = start then (+& right 1)
              for right = (min& (or (apply #'position delimiter seq 
                                           :start left
                                           other-keys)
                                    len)
                                end)
              unless (and (=& right left) 
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>=& nr-elts count))
              ;; We can't take any more. Return now.
              return (values subseqs left)
              else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
              until (>=& right end)
              finally (return (values subseqs right)))))))

#+IGNORE
(defun split-sequence-if (predicate seq &key (count nil) 
                                             (remove-empty-subseqs nil) 
                                             (from-end nil) 
                                             (start 0) (end nil) 
                                             (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (with-full-optimization ()
    (let ((len (length seq))
          (other-keys (when key-supplied 
                        (list :key key))))
      (unless end (setq end len))
      (if from-end
          (loop for right = end then left
                for left = (max& (or (apply #'position-if predicate seq 
                                            :end right
                                            :from-end t
                                            other-keys)
                                     -1)
                                 (1-& start))
                unless (and (=& right (1+& left))
                            remove-empty-subseqs) ; empty subseq we don't want
                if (and count (>=& nr-elts count))
                   ;; We can't take any more. Return now.
                return (values (nreverse subseqs) right)
                else 
                collect (subseq seq (1+& left) right) into subseqs
                and sum 1 into nr-elts
                until (<& left start)
                finally (return (values (nreverse subseqs) (1+& left))))
          (loop for left = start then (+& right 1)
                for right = (min& (or (apply #'position-if predicate seq 
                                             :start left
                                             other-keys)
                                      len)
                                  end)
                unless (and (=& right left) 
                            remove-empty-subseqs) ; empty subseq we don't want
                if (and count (>=& nr-elts count))
                   ;; We can't take any more. Return now.
                return (values subseqs left)
                else
                collect (subseq seq left right) into subseqs
                and sum 1 into nr-elts
                until (>=& right end)
                finally (return (values subseqs right)))))))

#+IGNORE
(defun split-sequence-if-not (predicate seq &key (count nil) 
                                                 (remove-empty-subseqs nil) 
                                                 (from-end nil) 
                                                 (start 0) (end nil) 
                                                 (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
 (CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."				; Emacs syntax highlighting is broken, and this helps: "
  (with-full-optimization ()
    (let ((len (length seq))
          (other-keys (when key-supplied 
                        (list :key key))))
      (unless end (setq end len))
      (if from-end
          (loop 
              for right = end then left
              for left = (max& (or (apply #'position-if-not predicate seq 
                                          :end right
                                          :from-end t
                                          other-keys)
                                   -1)
                               (1-& start))
              unless (and (=& right (1+& left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>=& nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+& left) right) into subseqs
              and sum 1 into nr-elts
              until (<& left start)
              finally (return (values (nreverse subseqs) (1+& left))))
          (loop 
              for left = start then (+& right 1)
              for right = (min& (or (apply #'position-if-not predicate seq 
                                           :start left
                                           other-keys)
                                    len)
                                end)
              unless (and (=& right left) 
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>=& nr-elts count))
              ;; We can't take any more. Return now.
              return (values subseqs left)
              else
              collect (subseq seq left right) into subseqs
              and sum 1 into nr-elts
              until (>=& right end)
              finally (return (values subseqs right)))))))


;;(pushnew :split-sequence *features*)
 
;;;-----------------------------------------------------------------------------
;;; These are used currently by the PPA parsing functions.
;;;-----------------------------------------------------------------------------

;;; HASTY-BUG: these should go away.  In-band null values was a bad
;;; idea in retrospect.  Lisp has NIL for a reason. For now, make
;;; them all NIL and make sure nothing breaks.  TP 4-7-09.

(defparameter *integer-null-value*      nil) ;;most-negative-fixnum)
(defparameter *single-float-null-value* nil) ;; most-negative-single-float)
(defparameter *double-float-null-value* nil) ;; most-negative-double-float)
(defparameter *string-null-value*       nil) 

;;;-----------------------------------------------------------------------------


(defun READ-INTEGER-FROM-STRING ( a-string )
  (read-from-string-safely a-string *integer-null-value*))

;;;-----------------------------------------------------------------------------

(defun READ-SINGLE-FLOAT-FROM-STRING ( a-string )
  (read-from-string-safely a-string *single-float-null-value*))

;;;-----------------------------------------------------------------------------

;;; Calls Ray's macro, safe-read-from-string which returns the original
;;; string if the read fails for some reason.

(defun READ-FROM-STRING-SAFELY ( a-string &optional (value-when-empty-string nil) )
  (cond ((empty-string-p a-string)
         value-when-empty-string)
        (t 
         (let ((ret-value (safe-read-from-string a-string)))
           (cond ((stringp ret-value)
                  value-when-empty-string)
                 (t ret-value))))))


;;; -----------------------------------------------------------------

(defmethod REMOVE-LEADING-COMMA ( (a-string STRING) )
  (remove-leading-char a-string #\,))

;;; -----------------------------------------------------------------

(defmethod REMOVE-LEADING-HYPHEN ( (a-string STRING) )
  (remove-leading-char a-string #\-))

;;; -----------------------------------------------------------------

(defmethod REMOVE-LEADING-CHAR ( (a-string STRING) (the-char CHARACTER) )
  (cond ((empty-string-p a-string) a-string)
        ((char= (aref a-string 0) the-char)
         (subseq a-string 1))
        (t a-string)))

;;;---------------------------------------------------------------------------
;;; SPLIT-SEQUENCE-WITH-DELIMITERS
;;;---------------------------------------------------------------------------

(defun SPLIT-SEQUENCE-WITH-DELIMITERS ( delimiters seq &rest args)
  (apply #'split-sequence-if `(,#'(lambda (x)(member x delimiters)) ,seq ,@args)))


;;;---------------------------------------------------------------------------
;;; MAPCAR* & MAPC*
;;;---------------------------------------------------------------------------

;;; It basically implements "the other semantics" CL good have adopted
;;; for the map-XX family of functions. Rather than stop when stop
;;; when the shortest list runs out, you simply supply null values and
;;; for the shorter lists when they run out so that all elements of
;;; the longest list get processed.

(defun MAPCAR* (fn list &rest rest)
  "Like mapcar but processes all elements of every list, using nil if short."
  (let* ((all-lists (cons list rest))
	 (max-length (apply #'max (mapcar #'(lambda (x)(length x)) all-lists)))
	 (args nil)
	 (result nil))
    (dotimes (i max-length)
      (setf args (mapcar #'(lambda (x)(nth i x)) all-lists))
      (push (apply fn (first args)(rest args)) result))
    (nreverse result)))

;;;---------------------------------------------------------------------------

(defun MAPC* (fn list &rest rest)
  "Like mapc but processes all elements of every list, using nil if short."
  (let* ((all-lists (cons list rest))
	 (max-length (apply #'max (mapcar #'(lambda (x)(length x)) all-lists)))
	 (args nil))
    (dotimes (i max-length)
      (setf args (mapcar #'(lambda (x)(nth i x)) all-lists))
      (apply fn (first args)(rest args)))))

;;;---------------------------------------------------------------------------
;;; SHOW-HASHTABLE-SYMBOLS
;;;---------------------------------------------------------------------------

;;; Moved Here from PQ-Initialization.lisp RdL 4/16/09

;;; TODO: Organzie our Tools & Utilities into a coherent set of Files.
;;; This should be a task on it's own and should consolidate util-mrh, utils-lh
;;; utils-kt, etc...

(defun SHOW-HASHTABLE-SYMBOLS (package &key (verbose t) (stream *standard-output*) (stats t))
  "Find symbols whose value is a hash table in package, and return them as a list.
   If verbose is t, show information about symbols whose value is a hash table, and the count for each.
   If stats is t, then show more detailed statistics about each hash table."
  (let ((hash-list nil)
	(pack (find-package package))
	(symb-count 0))
    (format stream "~2%Hash tables in package ~a" (package-name pack))
    (do-all-symbols (sym pack)
      (incf symb-count)
      (when (zerop (mod symb-count 100000))
	(dbg 2 "SHOW-HASHTABLE-SYMBOLS on symbol ~:D" symb-count))
      (when (and (boundp sym)
		 ;; double-check: is the _home package_ of this symbol the desired package?
		 (eq pack (symbol-package sym)))
	(let ((val (symbol-value sym)))
	  (when (hash-table-p val)
	    (push sym hash-list)
	    (when verbose
	      (format stream "~%Name: ~a   Count: ~:d" sym (hash-table-count val)))
	    #+ALLEGRO
	    (when stats
	      (fresh-line stream)
	      (excl::hash-table-stats val))))))
    (format stream "~2%Total hash tables: ~:d~%" (length hash-list))
    hash-list))

;;;---------------------------------------------------------------------------
;;; SHOW-SYMBOL-USE
;;;---------------------------------------------------------------------------

(defun SHOW-SYMBOL-USE (package &key (verbose t) (stream *standard-output*))
  "Find symbols in package with no value or function, and get statistics about their sizes.
  If verbose is true, then print out to stream.
  Returns as multiple values the number of such symbols, the average length, the maximum length, 
  the symbol of the maximum length, and a hash table with key of length and value of the number
  of symbols of that length."
  (let ((hash (make-hash-table :size 1000))
	(count 0)
	(length 0)
	(max-length 0)
	(max-size-symbol nil)
	(pack (find-package package)))
    (do-all-symbols (sym pack)
      (when (and (null (fboundp sym))
		 (null (boundp sym)))
	(let* ((name-length (length (symbol-name sym)))
	       (hash-value (gethash name-length hash)))
	  (incf count)
	  (setf length (+ name-length length))
	  (setf max-length (max name-length max-length))
	  (when (= name-length max-length)
	    (setf max-size-symbol sym))
	  (if hash-value
	    (setf (gethash name-length hash) (+ hash-value 1))
	    (setf (gethash name-length hash) 1)))))
    (let ((ave-length (truncate length count)))
      (when verbose
	(format stream "~%Symbols with no function or value in package ~a: ~%  Count: ~:d~%  Average name length: ~:d~%  Max name length: ~:d~%  Longest such symbol: ~a"
		(package-name pack)
		count
		ave-length
		max-length
		max-size-symbol))
      (values count ave-length max-length max-size-symbol hash))))
	      
;;;---------------------------------------------------------------------------

;; Speedy replacement for remove-duplicates on long lists.
(defun DE-DUP-FAST (list &key (test 'eql))
  (let ((tbl (make-hash-table :test test :size (length list))))
    (loop for e in list
        do (setf (gethash e tbl e) t)
        finally (return
                  (prog1
                      (loop for k being the hash-keys of tbl
                          collect k)
                    (clrhash tbl))))))

;;;---------------------------------------------------------------------------
;;; SPLIT-SEQUENCE-BY-POSITION
;;;---------------------------------------------------------------------------

(defmethod SPLIT-SEQUENCE-BY-POSITION ((seq LIST) (position INTEGER))
  (let ((first-seq nil)
	(second-seq nil)
	(seq-copy (copy-list seq))
	(seq-size (length seq))
	(count 0))
    ;; Generate the first list
    (loop 
      (when (or (> count position)(> count seq-size))
	(return first-seq))
      (push (first seq-copy) first-seq)
      (setf seq-copy (cdr seq-copy))
      (incf count))
   (setf first-seq (reverse first-seq))
    ;; Generate the second list
    (setf second-seq (nthcdr (1+ position) seq))
    ;; Return the split lists
    (values first-seq second-seq)))

;;;---------------------------------------------------------------------------
;;; POSITION-FN
;;;---------------------------------------------------------------------------

;;; Return the position of the element returned by applying <fn> to
;;; the sequrence of elements. If elements are non-unique the position
;;; of the first occurrence is returned

(defmethod POSITION-FN ((list LIST) fn)
  (let ((element (apply fn list)))
    (values (position element list) element)))

;;;---------------------------------------------------------------------------
;;; FIND-ALL
;;;---------------------------------------------------------------------------

(defun FIND-ALL (item sequence &key (key #'identity)(test #'eq))
  (let ((result nil))
    (dolist (x sequence)
      (when (funcall test (funcall key x) item)
	(push x result)))
    result))


;;;---------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------
