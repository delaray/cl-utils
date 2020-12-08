(in-package :UTILITIES)

;;;****************************************************************************
;;;
;;; FILE CONTENTS:
;;;
;;; Part I:    BASIC VALUE OPERTIONS
;;; Part II:   ADVANCED VALUE OPERTIONS
;;; Part III:  ADVANCED VALUE-SET OPERATIONS
;;; Part IV:   ITEMS
;;;
;;;****************************************************************************

(export '(OBJECT-NAME OBJECT-VALUE OBJECT-VALUES SAME-VALUE-P))

;;;****************************************************************************
;;;
;;; Part I:    VALUE Objects
;;;
;;;****************************************************************************

;;;---------------------------------------------------------------------------
;;; VALUE-MIXIN
;;;---------------------------------------------------------------------------

(defclass VALUE-MIXIN ()
  ((value :initarg :value
	  :initform nil
	  :accessor object-value)))

;;;---------------------------------------------------------------------------
;;; NAME-MIXIN
;;;---------------------------------------------------------------------------

(defclass NAME-MIXIN ()
  ((name :initarg :name
	  :initform nil
	  :accessor object-name)))

;;;---------------------------------------------------------------------------
;;; STATS-MIXIN
;;;---------------------------------------------------------------------------

(defclass STATS-MIXIN ()
  ((count :initarg :count
          :type fixnum
	  :initform 1
	  :accessor object-count)
   (frequency :initarg :frequency
	      :initform 0
	      :accessor object-frequency)
   (probability :initarg :probability
		:initform 0.0
		:accessor object-probability)))

;;;-----------------------------------------------------------------------------
;;; VALUE
;;;-----------------------------------------------------------------------------

(defclass VALUE (NAME-MIXIN VALUE-MIXIN)
  ())

;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj VALUE) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<Value: ~a>" (object-name obj)))))

;;;-----------------------------------------------------------------------------
;;; STATISTICAL-VALUE
;;;-----------------------------------------------------------------------------

(defclass STATISTICAL-VALUE (STATS-MIXIN VALUE)
  ())

;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj STATISTICAL-VALUE) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<Value: ~a(~a)>"
		 (object-name obj)
		 (object-count obj)))))

;;;-----------------------------------------------------------------------------
;;; VALUE-SET OBJECT
;;;-----------------------------------------------------------------------------

;;; A Value-set is really just a value object whose value is a collection
;;; of values.

(defclass VALUE-SET (VALUE)
  ((value :initarg :values
	  :accessor object-values)))
  
;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj VALUE-SET) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<Value-Set: ~a>"
		 (object-name obj)))))

;;;-----------------------------------------------------------------------------
;;; MAKE-VALUE
;;;-----------------------------------------------------------------------------

(defun MAKE-VALUE (value &key (count 1)(frequency 0)(name nil)(class 'VALUE))
  (make-instance class :name name 
		 :value value
		 :count count
		 :frequency frequency))

;;;-----------------------------------------------------------------------------
;;; OBJECT-VALUE
;;;-----------------------------------------------------------------------------

;;; Normally defined by the slot accessor of the VALUE object. This is
;;; for non value objects

;;; Not defined yet, need to eliminate object

;;;-----------------------------------------------------------------------------
;;; OBJECT-NAME
;;;-----------------------------------------------------------------------------

;;; This is not necessarily used yet but will part of the abstract object model
;;; in which every object has. This method shoulb be written for every type
;;; of object.

(defmethod OBJECT-NAME ((value t))
  value)

;;;-----------------------------------------------------------------------------

(defmethod OBJECT-NAME ((value VALUE-MIXIN))
  (object-value value))

;;;-----------------------------------------------------------------------------
;;; VALUE-OBJECT-P
;;;-----------------------------------------------------------------------------

;;; Normally defined by the slot accessor of the VALUE object. This is
;;; for non value objects

(defmethod VALUE-OBJECT-P ((value t))
  nil)

;;;-----------------------------------------------------------------------------

(defmethod VALUE-OBJECT-P ((value VALUE))
  t)

;;;-----------------------------------------------------------------------------
;;; SAME-VALUE-P
;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 t)(value-2 t))
  (eq value-1 value-2))

;;;-----------------------------------------------------------------------------
;;; SAME-VALUE-P: BASIC LISP DATA TYPES (more to be added)
;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 CHARACTER)(value-2 CHARACTER))
  (char= value-1 value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 (eql #\?))(value-2 CHARACTER))
  value-2)

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 CHARACTER)(value-2 (eql #\?)))
  value-1)

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 STRING)(value-2 STRING))
  (string-equal value-1 value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 NUMBER)(value-2 NUMBER))
  (= value-1 value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 SYMBOL)(value-2 SYMBOL))
  (string-equal (symbol-name value-1) (symbol-name value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 LIST)(value-2 LIST))
  (let ((same-value-p t))
    (when (= (length value-1)(length value-2))
      (loop
	(when (and (null value-1)(null value-2))
	  (return t))
	(unless (same-value-p (first value-1)(first value-2))
	  (setf same-value-p nil)
	  (return nil))
	(setf value-1 (rest value-1))
	(setf value-2 (rest value-2)))
      same-value-p)))

;;;-----------------------------------------------------------------------------
;;; SAME-VALUE-P: VALUE OBJECTS
;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE-MIXIN)(value-2 STRING))
  (same-value-p (object-value value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE-MIXIN)(value-2 NUMBER))
  (same-value-p (object-value value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE-MIXIN)(value-2 SYMBOL))
  (same-value-p (object-value value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 STRING)(value-2 VALUE-MIXIN))
  (same-value-p  value-1 (object-value value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 NUMBER)(value-2 VALUE-MIXIN))
  (same-value-p  value-1 (object-value value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 SYMBOL)(value-2 VALUE-MIXIN))
  (same-value-p  value-1 (object-value value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE-MIXIN)(value-2 VALUE-MIXIN))
  (same-value-p (object-value value-1)(object-value value-2)))

;;;-----------------------------------------------------------------------------
;;; VALUE-LESS-P
;;; Added by TP 6-04-08 for packaging-specification sorts.
;;; (see PQ-Nuvia-Description.lisp).
;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((value-1 CHARACTER)(value-2 CHARACTER))
  (char< value-1 value-2))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((value-1 STRING)(value-2 STRING))
  (string< value-1 value-2))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((value-1 NUMBER)(value-2 NUMBER))
  (< value-1 value-2))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((value-1 NUMBER)(value-2 STRING))
  (string< (prin1-to-string value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((value-1 STRING)(value-2 NUMBER))
  (string< value-1 (prin1-to-string value-2)))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((value-1 SYMBOL)(value-2 SYMBOL))
  (string<  (symbol-name value-1) (symbol-name value-2)))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((symbol-1 SYMBOL ) (string-2 STRING))
  (string< (symbol-name symbol-1) string-2))

;;;-----------------------------------------------------------------------------

(defmethod VALUE-LESS-P ((string-1 STRING) (symbol-2 SYMBOL))
  (string< string-1 (symbol-name symbol-2)))

;;;-----------------------------------------------------------------------------

;;; Nothing is less than nil.
(defmethod VALUE-LESS-P ((value-1 t)(value-2 NULL))
  nil)

;;;-----------------------------------------------------------------------------

;;; NIL is less than everything except NIL.

(defmethod VALUE-LESS-P ((value-1 NULL)(value-2 t))
  (if (null value-2) nil t))

;;;-----------------------------------------------------------------------------
;;; FIND-VALUE: WITHIN A SEQUENCE
;;;-----------------------------------------------------------------------------

(defmethod FIND-VALUE ((value STRING) (values SEQUENCE))
  (find value values :key #'object-value :test #'equal))

;;;-----------------------------------------------------------------------------

(defmethod FIND-VALUE ((value NUMBER) (values SEQUENCE))
  (find value values :key #'object-value :test #'equal))

;;;-----------------------------------------------------------------------------

(defmethod FIND-VALUE ((value VALUE) (values SEQUENCE))
  (find-value (object-value value) values))

;;;-----------------------------------------------------------------------------
;;; FIND-VALUES
;;;-----------------------------------------------------------------------------

(defmethod FIND-VALUES ((value t)(values CONS))
  (let ((values-found nil))
    (dolist (v values)
      (when (same-value-p v value)
	(push v values-found)))
    values-found))

;;;-----------------------------------------------------------------------------
;;; OBJECT-VALUE
;;;-----------------------------------------------------------------------------

;;; The 'value'of any thing defauts to itself.

(defmethod OBJECT-VALUE ((value t))
  value)

;;;-----------------------------------------------------------------------------
;;; NULL-VALUE-SET-P
;;;-----------------------------------------------------------------------------

;;; Normally this should check the count slot which is more efficient but we need
;;; to ensure that it is properly updated.

(defmethod NULL-VALUE-SET-P ((value-set VALUE-SET))
  (zerop (length (object-values value-set))))

;;;-----------------------------------------------------------------------------
;;; COMPARE-VALUE-SETS (an instance of compare-objects)
;;;-----------------------------------------------------------------------------

;;; This function takes a two value sets objects and returns three new value set
;;; containing the values they have in common and the differences according to
;;;  the specifi-ed <comparison-fn>. 

;;; Note: The <comparison-fn> is applied to the VALUE objects themselves and
;;; not directly to the value of each value object. The default comparison fn
;;; SAME-VALUE-P method on a VALUE object calls SAME-VALUE-P on the value
;;; of the VALUE object which is extracted using OBJECT-VALUE.

;;; This function returns 3 values:

;;; 1. A value set of common values
;;; 2. A value set of value objects in  <vs-1> but not in <vs-2>
;;; 3. A value set of value objects in <vs-2> but not in <vs-1>

;;;-----------------------------------------------------------------------------

;;; Note: We use PUSHNEW which can be slow, but values-1 and values-2
;;; are not expected to be large collections of values.

(defmethod COMPARE-VALUE-SETS ((values-1 LIST)(values-2 LIST)
			       
			       &key (comparison-fn #'same-value-p))
  (let ((common-values nil)
	(not-in-vs-2 nil)
	(not-in-vs-1 nil))
   ;; Process vs-1 values
    (dolist (value-1 values-1)
      (if (member value-1 values-2 :test comparison-fn)
	(pushnew value-1 common-values :test #'eq)
	(pushnew value-1 not-in-vs-2 :test #'eq)))
    ;; Process vs-2 values
    (dolist (value-2 values-2)
      (unless (member value-2 values-1 :test comparison-fn)
	(pushnew value-2 not-in-vs-1 :test #'eq)))
    ;; Return the three sorts of values
    (values common-values not-in-vs-2 not-in-vs-1)))

;;;-----------------------------------------------------------------------------

(defmethod COMPARE-VALUE-SETS ((vs-1 VALUE-SET)(vs-2 VALUE-SET)
			       &key (comparison-fn #'same-value-p))
  (let ((common-values (make-value-set nil 0 0 'common-values))
	(not-in-vs-2 (make-value-set nil 0 0 'not-in-value-set-2))
	(not-in-vs-1 (make-value-set nil 0 0 'not-in-value-set-1)))
    (multiple-value-bind (common-values-list not-in-vs-2-list not-in-vs-1-list)
	(compare-value-sets (object-values vs-1)(object-values vs-2)
			    :comparison-fn comparison-fn)
      (setf (object-values common-values) common-values-list)
      (setf (object-values not-in-vs-2) not-in-vs-2-list)
      (setf (object-values not-in-vs-1) not-in-vs-1-list)
      ;; Return the three value sets
      (values common-values not-in-vs-2 not-in-vs-1))))

;;;-----------------------------------------------------------------------------
;;; SAME-VALUE-P
;;;-----------------------------------------------------------------------------

;;; This is expensive but is necessary because values can occur in any order.
;;; Note: The duplication of values issue is not currently addressed.

(defmethod SAME-VALUE-P ((value-set-1 VALUE-SET)(value-set-2 VALUE-SET))
  (multiple-value-bind (common not-in-vs2 not-in-vs1)
      (compare-value-sets value-set-1 value-set-2)
    (declare (ignore common))
    (and (null-value-set-p not-in-vs1)
	 (null-value-set-p not-in-vs2))))

;;;-----------------------------------------------------------------------------
;;; SEARCH-VALUE-SET
;;;-----------------------------------------------------------------------------

(defmethod SEARCH-VALUE-SET ((value-set LIST)(value-sets LIST)
			     &optional (search-fn #'find))
  (funcall search-fn value-set value-sets :test #'same-value-p))

;;;-----------------------------------------------------------------------------

(defmethod SEARCH-VALUE-SET ((value VALUE)(value-sets LIST)
			     &optional (search-fn #'find))
  (funcall search-fn value value-sets :test #'same-value-p))

;;;-----------------------------------------------------------------------------

(defmethod SEARCH-VALUE-SET ((value-set VALUE-SET)(value-sets VALUE-SET)
			     &optional (search-fn #'find))
  (search-value-set value-set (object-values value-sets) search-fn))

;;;-----------------------------------------------------------------------------
;;; VALUE-SET-CONTAINS-VALUE-SET-P
;;;-----------------------------------------------------------------------------

(defmethod VALUE-SET-CONTAINS-VALUE-SET-P  ((value-set-1 VALUE-SET)
					    (value-set-2 VALUE-SET))
  (multiple-value-bind (common not-in-vs2 not-in-vs1)
      (compare-value-sets value-set-1 value-set-2)
    (declare (ignore common))
    (cond ((null-value-set-p not-in-vs1)
	   value-set-2)
	  ((null-value-set-p not-in-vs2)
	   value-set-1)
	  (t nil))))

;;;------------------------------------------------------------------------------
;;; MAKE-DESCRIPTION-STRING
;;;------------------------------------------------------------------------------

;;; Drill down of values & value-sets until string or char is reached.

(defmethod MAKE-DESCRIPTION-STRING ((object VALUE-MIXIN)
				    &key 
				    (delimiter #\space)
				    (padded nil))
  (let ((value (object-value object)))
    (cond ((stringp value)
	   value)
	  ((typep value 'VALUE-MIXIN)
	   (make-description-string object :delimiter delimiter :padded padded))
	  ((or (typep value 'VALUE-SET)
	       ;; This is gross, incoming object should  have been a value set.
	       (and (typep value 'LIST)
		    (or (typep (first value) 'VALUE-MIXIN)
			;;(typep (first value) 'PARSER::LANGUAGE-WORD-MIXIN)
			#+IGNORE
                        (typep (first value) 'INFERRED-NOUN) )))
	   (make-description-string
	    (mapcar #'(lambda (x)
			(make-description-string x
						 :delimiter delimiter
						 :padded padded))
		    (object-value value))))
	  (t
	   (format nil "~a" value)))))

;;;******************************************************************************
;;;
;;; FILE CONTENTS
;;; 
;;;
;;; Part I: VALUE OPERATIONS
;;; ------------------------
;;;
;;; VALUE-OBJECT-P
;;; SAME-VALUE-P
;;; VALUE-LESS-P
;;; FIND-VALUE
;;; FIND-VALUES
;;; SINGLE-VALUE-P
;;; ADD-VALUE
;;; DELETE-VALUE
;;; MERGE-SAME-VALUES
;;; MATCH-VALUES
;;; PHANTOM-VALUE-P
;;; OBJECT-DISPLAY-VALUE
;;;
;;;
;;; Part II: VALUE-SET OPERATIONS
;;; -----------------------------
;;;
;;; COMPARE-VALUE-SETS
;;;
;;;******************************************************************************


;;;******************************************************************************
;;; Part I: VALUE OPERATIONS
;;;******************************************************************************

;;;-----------------------------------------------------------------------------
;;; Methods for mixed datatypes. TP 9-11-08
;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 SYMBOL)(value-2 STRING))
  (string-equal (symbol-name value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 STRING)(value-2 SYMBOL))
  (string-equal value-1 (symbol-name value-2)))


;;;-----------------------------------------------------------------------------
;;; SAME-VALUE-P: VALUE OBJECTS
;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE)(value-2 STRING))
  (same-value-p (object-value value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE)(value-2 NUMBER))
  (same-value-p (object-value value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE)(value-2 SYMBOL))
  (same-value-p (object-value value-1) value-2))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 STRING)(value-2 VALUE))
  (same-value-p  value-1 (object-value value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 NUMBER)(value-2 VALUE))
  (same-value-p  value-1 (object-value value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 SYMBOL)(value-2 VALUE))
  (same-value-p  value-1 (object-value value-2)))

;;;-----------------------------------------------------------------------------

(defmethod SAME-VALUE-P ((value-1 VALUE)(value-2 VALUE))
  (same-value-p (object-value value-1)(object-value value-2)))


;;;-----------------------------------------------------------------------------
;;; DELETE-VALUE
;;;-----------------------------------------------------------------------------

(defmethod DELETE-VALUE ((object-value STRING) (values SEQUENCE))
  (%delete-value object-value values))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-VALUE ((object-value NUMBER) (values SEQUENCE))
  (%delete-value object-value values))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-VALUE ((object-value SYMBOL) (values SEQUENCE))
  (%delete-value object-value values))

;;;-----------------------------------------------------------------------------

(defmethod %DELETE-VALUE ((object-value t)(values SEQUENCE))
  (let ((value (find-value object-value values)))
    (when value
      (cond ((eq value object-value)
	     (remove value values :test #'equal))
	    (t
	     (delete-value value values))))))

;;;-----------------------------------------------------------------------------

(defmethod DELETE-VALUE ((value VALUE) (values SEQUENCE))
  (let ((value (find-value value values)))
    (cond ((= (object-count value) 1)
	   (remove (find-value value values) values
		   :test #'equal))
	  (t
	   (decf (object-count value))))))

;;;-----------------------------------------------------------------------------
;;; MERGE-SAME-VALUES
;;;-----------------------------------------------------------------------------

;;; Returns a new value object with the same name as value-1 and the combined
;;; counts and frequencies of all values.

(defmethod MERGE-SAME-VALUES ((value-1 VALUE) &rest rest)
  (let ((new-value (make-value (object-value value-1)
			       :count (object-count value-1)
			       :frequency (object-frequency value-1))))
    (dolist (value-i rest)
      (incf (object-count new-value)
             (object-count value-i))
      (incf (object-frequency new-value)
             (object-frequency value-i)))
    ;; Average the value frequency total
    (setf (object-frequency new-value)
      (floor (object-frequency new-value)
	     (1+ (length rest))))
    ;; Return the merged value
    new-value))

;;;-----------------------------------------------------------------------------

;;; If value-1 is not a value object, assume all values same string, or
;;; same number or symbol and just return value-1.

(defmethod MERGE-SAME-VALUES ((value-1 t) &rest rest)
  (declare (ignore rest))
  value-1)

;;;-----------------------------------------------------------------------------
;;; MATCH-VALUES
;;;-----------------------------------------------------------------------------

;;; These methods return as multiple values:

;;; 1. The probability of the match
;;; 2. The count of matching elements from values-1
;;; 3. The element-corresponce list (as pairs of subscripts)

#+IGNORE
(defmethod MATCH-VALUES ((values-1 SEQUENCE)(values-2 SEQUENCE))
  (match-sequences values-1 values-2 :basic))


;;;----------------------------------------------------------------------------
-;;; OBJECT-DISPLAY-VALUE
;;;-----------------------------------------------------------------------------

(defmethod OBJECT-DISPLAY-VALUE ((value t))
  value)

;;;-----------------------------------------------------------------------------

(defmethod OBJECT-DISPLAY-VALUE ((value VALUE))
  (object-value value))


;;;******************************************************************************
;;; Part II: VALUE-SET OPERATIONS
;;;******************************************************************************

;;;******************************************************************************
;;;
;;; FILE CONTENTS:
;;;
;;; Part I: ADVANCED VALUE OPERTIONS
;;;
;;; COMPUTE-SETS-OF-VALUES
;;; GROUP-ATTRIBUTE-VALUE-SETS-BY-ATTRIBUTE
;;; COMPUTE-ATTRIBUTE-VALUE-TOKENS
;;; FIND-NAMED-VALUE-SET
;;;
;;; Part II: VALUE-SET OPERATIONS
;;;
;;; MAKE-VALUE-SET
;;; FIND-NAMED-VALUE-SET
;;; MAKE-VENDOR-VALUE-SET
;;; COMPUTE-VALUE-SET-VALUES
;;; COMPUTE-VALUE-SET-ALIASES
;;;
;;; Part III: ADVANCED VALUE-SET OPERATIONS
;;;
;;; MAKE-VALUE-SET-SUBSTITUTIONS
;;; MAKE-DISTINCT-SETS
;;; MAKE-INITIAL-DISTINCT-PAIR-SETS
;;; FIND-BEST-VALUE-MATCHES
;;; FIND-UNASSIGNED-VALUES
;;; MAX-MUTUAL-OCCURENCES
;;;
;;;******************************************************************************

;;;******************************************************************************
;;; Part I: ADVANCED VALUE OPERATION
;;;******************************************************************************

;;;-----------------------------------------------------------------------------
;;; COMPUTE-SETS-OF-VALUES
;;;-----------------------------------------------------------------------------
  

;;;******************************************************************************
;;; Part II: VALUE SETS
;;;******************************************************************************

;;; The Variant components of templates group together values shared by a range
;;; of products. Because of the use of abbreviations, inconsistent word orderings,
;;; mispellings etc...it is hard to achieve consistency.
;;;   
;;; The idea between value-sets is to be able to compare sets of values in 
;;; order to achieve internal consistency of attribute values. 
;;;   
;;; By grouping together sets of values (we will refer to the grouping of
;;; sets of values as value-sets) based on shared common values we will obtain
;;; a self-revealing intristic context that will allow us to normalize
;;; not only the individual values, but collections of values as well.
;;;   
;;; For example we may have the following value-set (sets of sets of values):
;;;  
;;; (SM Large Med Medium LFT XLG)
;;; (SM Left Med Medium XLG)
;;; (Left RT Small Right)
;;; (LFT Left RT)
;;; (Left Right)
;;;  
;;; Using pattern matching (probablistic matching, Freq of Occurance,
;;; alias substitution, etc..) we would like to end up with the following 
;;; value set:
;;;  
;;; (Small Medium Large XLG)
;;; (Right Left)
;;;  
;;; This is probably not doable from the small set of examples provided above,
;;; which is simply intended to illustrate the idea. In practice we will be
;;; dealing with much larger sets of examples.
;;;  
;;; The resulting consistent value sets we will then universally applied
;;; first within iid-templates (which group together several idd-templates)
;;; then within vendors (which groups together iid-templates) and finally
;;; unvirsally to the entire database (which groups together vendors)
;;; of templates (idd-templates & tag-templates) in that order. We will
;;; proceed from most-specific contexts to most-general context.
;;;  
;;; This is the goal. To accomplish this goal we will first analyze 
;;; value-sets within vendors and then we will analyze them across vendors.
;;;  
;;; Note 1: Because of the "mess" of values in the numeric attributes we will
;;; initially limit the analysis of value-sets to sets of values consisting
;;; of tokens (words) made up of strictly alphabetic characters (presumably
;;; words or abbreviations in the english language).
;;;  
;;; Note 2: This approach parallels the context-sensitive alias substitution.
;;; The alias substitution is performed at the granularity level of "the value"
;;; as the "entity" (the grain). The value-set approach is the same thing
;;; at the granularity level of a "set of values" as the entity (the grain).
;;; The idea is that they will complement each other.

;;;-----------------------------------------------------------------------------
;;; MAKE-VALUE-SET
;;;-----------------------------------------------------------------------------

(defun MAKE-VALUE-SET (values &optional (count 1)(frequency 0)(name nil))
  (declare (ignore count frequency))
  (make-instance 'VALUE-SET
    :name name 
    :values values))

;;;-----------------------------------------------------------------------------

(defmethod FIND-NAMED-VALUE-SET (name (value-sets CONS))
  (find name value-sets 
	:key #'(lambda (x) (object-name x))
	:test #'equal))


;;;******************************************************************************
;;; PART II: ADVANCED VALUE SET OPERATIONS
;;;******************************************************************************
  
;;;-----------------------------------------------------------------------------
;;; FIND-SET-WITH-VALUE
;;;-----------------------------------------------------------------------------

(defun FIND-SET-WITH-VALUE (value sets &key (key #'identity))
  (let ((found-set nil)
	(n 0))
    (dolist (set sets)
      (when (find value set :test #'string-equal :key key)
	(setf found-set set)
	(return t))
      (incf n))
    (values found-set n)))
    
;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
