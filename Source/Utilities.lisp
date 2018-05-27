(in-package :Utilities)

(export '(SUBCLASSP))

;;;***************************************************************************
;;; PART I: UTILITIES
;;;***************************************************************************

(defvar *WHITESPACE* '(#\Space #\Tab #\Return #\Newline #\Linefeed #\Page))

;;;---------------------------------------------------------------------------
;;; COERCE-ARRAY-TO-LIST
;;;---------------------------------------------------------------------------

;;; This is a holdover from the earlier UOM work in April 2008.
;;; Should be moved since it's not called in the current (4-07-09 code).

;;; Commenting out since it is not called and it generates a compiler
;;; warning anyway.

#+IGNORE
(defun COERCE-ARRAY-TO-LIST (data-array)
  (let* ((dims (array-dimensions data-array))
         (n-rows (first dims))
         (n-cols (second dims))
         (rows nil)
	 (current-row nil))
    (cond ((= 1 n-cols)
           (loop for row below n-rows
                 collect (aref data-array row 0)))
          ((= 1 n-rows)
           (loop for col below n-cols
                 collect (aref data-array col 0)))
          (t 
           (loop for row below n-rows
                 do (loop for col below n-cols
			collect (aref data-array row col) into current-row)
		    (push current-row rows))
           (reverse rows)))))

 
;;;---------------------------------------------------------------------------
;;; COERCE-LIST-TO-ARRAY
;;;---------------------------------------------------------------------------

;;; This is a holdover from the earlier UOM work in
;;; April 2008.  Should be moved since it's not called
;;; in the current (4-07-09 code).

(defun COERCE-LIST-TO-ARRAY ( list-of-numbers )
  (let* ((data-vec (mapcar #'(lambda (x) (list (coerce x 'double-float)) )
                           list-of-numbers))
	 (n (length data-vec))
	 ;; put the vector of numbers into kmeans format (column-major array).
	 (data-array (make-array `(,n 1) :element-type 'double-float
				 :initial-contents data-vec)))
    data-array))


;;;---------------------------------------------------------------------------
;;; RANDOM-FROM-LIST
;;;---------------------------------------------------------------------------

;;; Pick an item at random from the-list.  n-list is
;;; the length of the-list or NIL.

(defun RANDOM-FROM-LIST ( the-list &optional n-list )
  (let* ((n-items (or n-list (length the-list)))
         (n-retrieve (random n-items)))
    (nth n-retrieve the-list)))

;;;---------------------------------------------------------------------------	   
;;; RANDOM-SAMPLE
;;;---------------------------------------------------------------------------

;;; Take a sample of n-sample items randomly from the-list.
;;; Notice that duplicates can occur.

(defun RANDOM-SAMPLE ( the-list n-sample )
  (let ((n-list (length the-list)))
    (cond ((< n-sample n-list)
           (loop for i below n-sample
                 collect (random-from-list the-list n-list)))
          (t the-list))))


;;;-----------------------------------------------------------------------------
;;; SUBCLASSP
;;;-----------------------------------------------------------------------------

;;; TODO: XXXXXX

(defun SUBCLASSP (class1 class2)
  "Is class1 a subclass of class2?"
  ;; Note, SUBTYPEP accepts classes or classnames in either position.
  (subtypep class1 class2))

;;;--------------------------------------------------------------------
;;;
;;; This is the problem string that cause an error to read-from-string:
;;;
;;; (READ-FROM-STRING "67550S2000")

;;;--------------------------------------------------------------------
;;; Request processing 
;;;--------------------------------------------------------------------

(define-condition naming-error (error)
  ((text :initarg :text :reader text)))

;;;-----------------------------------------------------------------------------
;;; HYPHENATE-TOKENS
;;;-----------------------------------------------------------------------------

(defmethod HYPHENATE-TOKENS ((tokens CONS))
  (cond ((> (length tokens) 1)
	 (let* ((new-tokens nil))
	   (dolist (token tokens)
	     (push token new-tokens)
	     (push "-" new-tokens))
	   (setf new-tokens (nreverse (cdr new-tokens)))
	    (seq-list-to-seq new-tokens)))
	((null tokens)
	 tokens)
	(t
	 (first tokens))))

;;;------------------------------------------------------------------------------'

(defmethod HYPHENATE-TOKENS ((tokens STRING))
  (cond ((find #\space tokens :test #'char=)
	 (setf tokens (split-sequence #\space tokens))
	 (cond ((> (length tokens) 1)
		(setf tokens (remove-if #'characterp tokens))
		(when tokens
		  (hyphenate-tokens tokens)))
	       (t
		(first tokens))))
	(t
	 tokens)))

;;;------------------------------------------------------------------------------'

(defmethod HYPHENATE-TOKENS ((token NUMBER))
  token)
 
;;;---------------------------------------------------------------------------
;;; STRING-LIST-TO-STRING
;;;---------------------------------------------------------------------------

(defun SEQ-LIST-TO-SEQ (seq-list &optional (seq-type 'STRING))
  "Concatenate list of sequences into a single sequence of type seq-type."
  (apply #'concatenate seq-type 
	     (mapcar #'coerce-to-string seq-list)))

;;;-----------------------------------------------------------------------------
;;; COERCE-TO-STRING
;;;-----------------------------------------------------------------------------

(defun COERCE-TO-STRING (obj)
  "Returns a string representation of obj; return should be treated as immutable."
  (typecase obj
    (string obj)
    (character (string obj))
    (symbol (symbol-name obj))
    (number (write-to-string obj))
    (otherwise (princ-to-string obj))))

;;;---------------------------------------------------------------------------
;;; HATAL
;;;---------------------------------------------------------------------------

(defun HATAL (hash-table)
  (loop for key being the hash-key of hash-table using (hash-value value)
     collect (cons key value)))

;;;---------------------------------------------------------------------------
;;; EMPTY-HASH-TABLE-P
;;;---------------------------------------------------------------------------

(defmethod EMPTY-HASH-TABLE-P ((table hash-table))
  (zerop (hash-table-count table)))

;;;---------------------------------------------------------------------------
;;; DTPR-EQ
;;;---------------------------------------------------------------------------

;;; Dotted pair equality.

(defun DTPR-EQ (dp1 dp2)
  (and (eq (car dp1)(car dp2))
       (eq (cdr dp1)(cdr dp2))))

;;;-----------------------------------------------------------------------------
;;; PRINT-TESTING-INTRO
;;;-----------------------------------------------------------------------------

(defun PRINT-TESTING-INTRO (fn-name &key (stream t))
  (format stream "~%~%--------------------------------------")
  (format stream "~%Testing: ~a" fn-name)
  (format stream "~%--------------------------------------~%"))

;;;-----------------------------------------------------------------------------
;;; TEST-FUNCTION
;;;-----------------------------------------------------------------------------

(defun TEST-FUNCTION (fn-name form result &optional (print-intro-p nil))
  (when print-intro-p
    (print-testing-intro fn-name))
  (format t "~%~%Trying: ~a" form)
  (format t "~%Result: ~a" result))


;;;----------------------------------------------------------------------------
;;; PRINSPACES
;;;----------------------------------------------------------------------------

(defun PRINSPACES (stream num colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (dotimes (i num)
    (write-char #\space stream)))

;; (format nil "~/pq::n-spaces/" 5)

;;;-------------------------------------------------------------------------
;;; MegaBytes & GigaBytes (from MRH)
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defconstant ONE-GIGABYTE (expt 2 30))

  (defconstant ONE-MEGABYTE (expt 2 20)))

;;;-------------------------------------------------------------------------

(defun TO-GB (num &rest more-nums)
  ;; give it 2 decimals precision
  (let ((total (apply #'+ num more-nums)))
    (/ (float (floor (* 100.0 (/ (float total) #.(float ONE-GIGABYTE)))))
	100.0)))

;;;-------------------------------------------------------------------------

(defun TO-MB (num &rest more-nums)
  ;; give it 2 decimals precision
  (let ((total (apply #'+ num more-nums)))
    (/ (float (floor (* 100.0 (/ (float total) #.(float ONE-MEGABYTE)))))
        100.0)))

;;;-------------------------------------------------------------------------

(defun MAXIMUM (list &key (key #'identity))
  (let* ((max-elmt (first list)))
    (dolist (elmt (rest list))
      (when (> (apply key elmt)
	       (apply key max-elmt))
	(setf max-elmt elmt)))
    max-elmt))

  
;;;************************************************************************
;;; Combinatorial Functions (RdL)
;;;************************************************************************

;;;------------------------------------------------------------------------
;;; CARTESIAN-PRODUCT
;;;------------------------------------------------------------------------

(defun CARTESIAN-PRODUCT (lists-of-lists &optional result)
  (let* ((new-result nil))
    (cond ((null lists-of-lists)
	   (mapcar #'reverse result))
	  (t
	   (dolist (item (first lists-of-lists))
	     (cond ((null result)
		    (push (list item) new-result))
		   (t
		    (dolist (list result)
		      (push (cons item list) new-result)))))
	   (cartesian-product (rest lists-of-lists) new-result)))))

;;;-----------------------------------------------------------------------------

;;; This returns all unique pair sets. I.e. (1 2) = (2 1)

(defun ALL-PAIRS (list)
  (let* ((pairs nil)
	 (new-pairs nil))
    (setf pairs (remove-if #'(lambda (x)(eq (first x)(second x)))
			   (cartesian-product (list list list))))
    (dolist (pair pairs)
      (unless (find (list (second pair)(first pair)) new-pairs :test #'equalp)
	(push pair new-pairs)))
    new-pairs))

;;;-----------------------------------------------------------------------------
;;; N-CHOOSE-I
;;;-----------------------------------------------------------------------------

;;; This implements the general combinatorics operator for choosing i things
;;; from a set of n things.

(defun N-CHOOSE-I (list i)
  (let ((result nil)
	(processed nil))
    (cond ((= i 1)
	   (mapcar #'list list))
	  ((= i 2)
	   (all-pairs list))
	  ((= i (length list))
	   (list list))
	  (t
	   ;; This is the general non-edge recursive case. Notice the use
	   ;; of the <processed> accumulator to avoid duplicates.
	   (dolist (elmt list)
	     (push elmt processed)
	     (setf result
	       (append 
		(mapcar #'(lambda (x)
			    (cons elmt x))
			(n-choose-i (set-difference list processed)(1- i)))
		result)))
	   result))))

;;;-----------------------------------------------------------------------------
;;; ALL-N-CHOOSE-I
;;;-----------------------------------------------------------------------------

;;; Like N-CHOOSE-I but iterates over every value of i. This implements the
;;; all possible subsets of a set except the empty set. This yields
;;; 2-to-the-N-minus-one results.

(defun ALL-N-CHOOSE-I (list &optional (i (length list)))
  (when (> i 0)
    (append (n-choose-i list i)(all-n-choose-i list (1- i)))))

  
;;;------------------------------------------------------------------------------  
;;; Placeholder for a real almost-equal.
;;; Threshold should be in percentage, since we
;;; want to see discounted costs as equal. Used in PQ-Discernment-PPA.lisp.

(defparameter *ALMOST-EQUAL-PROPORTION-THRESHOLD* 0.2)

(defun ALMOST-EQUAL ( x y &optional (threshold *almost-equal-proportion-threshold*) )
  (or (= x y)
      (<= (abs (/ (- x y)(/ (+ x y) 2.0))) threshold)))


;;;------------------------------------------------------------------------------

(defun POSITIVE-NUMBERP (x)
  (and (numberp x)
       (> x 0.0)))

;;;------------------------------------------------------------------------------

(defun NUMBER-TO-STRING (number)
  (format nil "~a" number))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------

