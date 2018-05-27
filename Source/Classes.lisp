(in-package :Utilities)


;;;-----------------------------------------------------------------------------
;;; LEXICON-MIXIN
;;;-----------------------------------------------------------------------------

(defclass LEXICON-MIXIN ()
  ())

;;;-----------------------------------------------------------------------------
;;; LEXICAL-REPOSITORY
;;;-----------------------------------------------------------------------------

;;; This is used to store the four hash-tables consisting of the lexical references
;;; for a particular source.

(defclass LEXICAL-REPOSITORY (LEXICON-MIXIN)
  ((dictionary :initform (make-hash-table :size 10 :test #'equalp)
	       :initarg :dictionary
	       :accessor object-dictionary)
   (proper-nouns :initform (make-hash-table :size 10 :test #'equalp)
		 :initarg :proper-nouns
		 :accessor object-proper-nouns)
   (abbreviations :initform (make-hash-table :size 10 :test #'equalp)
		  :initarg :abbreviations
		  :accessor object-abbreviations)
   (plurals :initform (make-hash-table :size 10 :test #'equalp)
		  :initarg :plurals
		  :accessor object-plurals)
   (adjectives :initform (make-hash-table :size 10 :test #'equalp)
	       :initarg :adjectivs
	       :accessor object-adjectives)
   (acronyms :initform (make-hash-table :size 10 :test #'equalp)
	     :initarg :acronyms
	     :accessor object-acronyms)
   (synonyms :initform (make-hash-table :size 10 :test #'equalp)
	     :initarg :synonyms
	     :accessor object-synonyms)
   (akas :initform (make-hash-table :size 10 :test #'equalp)
	     :initarg :akas
	     :accessor object-akas)
   (codes :initform (make-hash-table :size 10 :test #'equalp)
	     :initarg :codes
	     :accessor object-codes)
   (misspellings :initform (make-hash-table :size 10 :test #'equalp)
	     :initarg :misspellings
	     :accessor object-misspellings)
   ;; This slot is used to store the actual composites. I.e. "Stainless Steel".
   (composites :initform (make-hash-table :size 10 :test #'equalp)
	       :initarg :composites
	       :accessor object-composites)
   ;; While the slot above is used to store the actual composite
   ;; entries (i.e."Stainless Steel"), this slot stores the prefix
   ;; (i.e. stainless for analyzer identification)
   (composite-prefixes :initform (make-hash-table :size 10 :test #'equalp)
		       :initarg :composite-prefixes
		       :accessor object-composite-prefixes)
   ;; This slot is used to Proper Names
   (proper-names :initform (make-hash-table :size 10 :test #'equalp)
	       :initarg :proper-names
	       :accessor object-proper-names)
  ;; This slot is used to store MA defined noun-contextual attribution info
   (attribution :initform (make-hash-table :size 10 :test #'equalp)
		:initarg :attribution
	       :accessor object-attribution)
   ;; This is just a list of local source attributes
   (local-attributes :initarg :local-attributes
	       :initform nil
	       :accessor object-local-attributes)
   ;; These are currently mostly numeric-special patterns. Language token Classes
   ;; are automatically defined for these pattern types and token
   ;; instances of these classes are created by the analyzer
   (language-patterns :initform (make-hash-table :size 100 :test #'equalp)
		      :initarg :language-patterns
		      :accessor object-language-patterns)
   ;; These are loaded from a lisp file specified by the MA and applied by 
   ;; the analyzer prior to parsing.
   (token-expansions :initform nil
			  :initarg :token-expansions
			  :accessor object-token-expansions)
   ;; These are not implemented yet
   (phrase-transformations :initform nil
			   :initarg :phrase-transformations
			   :accessor object-phrase-transformations)))

;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj LEXICAL-REPOSITORY) stream)
   (cond (*print-readably*
	 (call-next-method))
	 (t
	  (print-unreadable-object (obj stream :type t :identity nil) 
	    (format stream "[~A,~A,~A,~A,~A]"
		    (hash-table-count (object-dictionary obj))
		    (hash-table-count (object-proper-nouns obj))
		    (hash-table-count (object-abbreviations obj))
		    (hash-table-count (object-acronyms obj))
		    (hash-table-count (object-composites obj))))
	  ;; (format stream "#<Lexical-Repository>")
	  )))

;;;-----------------------------------------------------------------------------
;;;  MAKE-LEXICAL-REPOSITORY
;;;-----------------------------------------------------------------------------

;;; Note: An initialize-instance :after method automatically adds an
;;; entry to *source-references*

(defun MAKE-LEXICAL-REPOSITORY ()
  (make-instance 'LEXICAL-REPOSITORY))

;;; Need to clea up usage of this variable across PQ, MBS & Parser packages.

(defvar *NULL-LEXICAL-REPOSITORY*  (make-lexical-repository))

;;;-----------------------------------------------------------------------------
;;; LEXICAL-REPOSITORY-SET
;;;-----------------------------------------------------------------------------

;;; This is used to store a collection of lexical repositories.

(defclass LEXICAL-REPOSITORY-SET (LEXICON-MIXIN VALUE-SET)
  ((value :accessor object-lexical-repositories)))

;;;-----------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj LEXICAL-REPOSITORY-SET) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<~a(~a)>"
		 (object-name obj)
		 (length (object-values obj))))))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
