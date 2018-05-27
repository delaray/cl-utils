(in-package :UTILITIES)


;;;*****************************************************************************
;;; Part 2: Source File Name Generation Protocol
;;;*****************************************************************************

;;;-----------------------------------------------------------------------------
;;; MAKE-SOURCE-FILENAME
;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name STRING))
  ;; 2011-01-27 mrh: Per explicit request (Mantis 239), preserving source case.
  (concatenate 'STRING source "-" file-name))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql  :scanned-and-analyzed-short-descriptions)))
  (make-source-filename source "Scanned-and-Analyzed-Short-Descriptions"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :token-expansions)))
  (make-source-filename source "Token-Expansions"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :local-attributes)))
  (make-source-filename source "Local-Attributes"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :descriptions-transformation)))
  (make-source-filename source "Descriptions-Transformation"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :phrase-transformations)))
  (make-source-filename source "Phrase-Transformations"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :pattern-categories)))
  (make-source-filename source "Numeric-Pattern-Categories"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :vocabulary-positions)))
  (make-source-filename source "Vocabulary-Positions"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :unknown-tokens)))
  (make-source-filename source "Unknown-Tokens"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql  :attribution-lexicon)))
  (make-source-filename source "Attribution-Lexicon"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :idiomatic-phrase)))
  (make-source-filename source "Idiomatic-Phrases"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :unrecognized-alphabetic-tokens)))
  (make-source-filename source "Unrecognized-Alphabetic-Tokens"))


;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :unrecognized-numeric-tokens)))
  (make-source-filename source "Unrecognized-Numeric-Tokens"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :unrecognized-special-tokens)))
  (make-source-filename source "Unrecognized-Special-Tokens"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :complete-long-descriptions)))
  (make-source-filename source "Long-Descriptions"))


;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :complete-short-descriptions)))
  (make-source-filename source "Short-Descriptions"))


;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :distinct-short-descriptions)))
  (make-source-filename source "Short-Descriptions"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :transformed-descriptions)))
  (make-source-filename source "Transformed-Descriptions"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :parsed-short-descriptions)))
  (make-source-filename source "Parsed-ShortDescriptions"))

;;;-----------------------------------------------------------------------------
;;; Grammatical Corrections
;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :uncorrected-sd-productions)))
  (make-source-filename source "Uncorrected-SD-Productions"))

;;;-----------------------------------------------------------------------------

;;; These are SD Grammatical Corrections

(defmethod MAKE-SOURCE-FILENAME ((source STRING) (file-name (eql :corrected-sd-productions)))
  (make-source-filename source "Corrected-SD-Productions"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :uncorrected-rd-productions)))
  (make-source-filename source "Uncorrected-RD-Productions"))

;;;-----------------------------------------------------------------------------

;;; These are SD Grammatical Corrections

(defmethod MAKE-SOURCE-FILENAME ((source STRING) (file-name (eql  :corrected-rd-productions)))
  (make-source-filename source "Corrected-RD-Productions"))

;;;-----------------------------------------------------------------------------

;;; These are SD Grammatical Corrections

(defmethod MAKE-SOURCE-FILENAME ((source STRING) (file-name (eql  :production-pattern-corrections)))
  (make-source-filename source "Production-Pattern-Corrections"))

;;;-----------------------------------------------------------------------------
;;; Noun Related Files
;;;-----------------------------------------------------------------------------

;;; This is for the grammatical production level Noun Inferrence.

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :uncorrected-production-nouns)))
  (make-source-filename source "Uncorrected-Production-Nouns"))

;;;-----------------------------------------------------------------------------

;;; This is for the grammatical production level Noun Corrections.

(defmethod MAKE-SOURCE-FILENAME ((source STRING) (file-name (eql :corrected-production-nouns)))
  (make-source-filename source "Corrected-Production-Nouns"))

;;;-----------------------------------------------------------------------------

;;; This is for the grammatical production level Noun Corrections.

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :final-production-nouns)))
  (make-source-filename source "Final-Production-Nouns"))


;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :analyzed-nouns)))
  (make-source-filename source "Analyzed-Nouns"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :uncorrected-sd-nouns)))
  (make-source-filename source "Uncorrected-Short-Description-Nouns"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :corrected-sd-nouns)))
  (make-source-filename source "Corrected-Short-Description-Nouns"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :SD-value-sets)))
  (make-source-filename source "Short-Description-Value-Sets"))


;;;-----------------------------------------------------------------------------

(Defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :RD-value-sets)))
  (make-source-filename source "Remaining-Description-Value-Sets"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :registered-nouns)))
  (make-source-filename source "Registered-Nouns"))

;;
;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :attribute-taxonomy)))
  (make-source-filename source "Attribute-Taxonomy"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :measurement-unit-taxonomy)))
  (make-source-filename source "Unit-Taxonomy"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :measurement-value-taxonomy)))
  (make-source-filename source "Value-Taxonomy"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :packaging-taxonomy)))
  (make-source-filename source "Packaging-Taxonomy"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :attribute-unit-bridges)))
  (make-source-filename source "Attribute-Unit-Bridges"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :value-attribute-bridges)))
  (make-source-filename source "Value-Attribute-Bridges"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :attribute-value-bridges)))
  (make-source-filename source "Attribute-Value-Bridges"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :attribute-names-SD)))
  (make-source-filename source "Attribute-Name-Assignments-SD"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :attribute-names-RD)))
  (make-source-filename source "Attribute-Name-Assignments-RD"))


;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :inferred-embedded-tags)))
  (make-source-filename source "Inferred-Embedded-Tags"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :tagged-reference-correct)))
  (make-source-filename source "Tagged-Reference-Data"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :tagged-reference-partial)))
  (make-source-filename source "Tagged-Reference-Data-Partial"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :tagged-reference-incorrect)))
  (make-source-filename source "Tagged-Reference-Data-Incorrect"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :non-attributed-values)))
  (make-source-filename source "Non-Attributed-Values"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :multiple-attributed-values)))
  (make-source-filename source "Multiple-Attributed-Values"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :vat)))
  (make-source-filename source "Attribution-Table"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :unprocessed)))
  (make-source-filename source "Attribution-Table"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :sd-productions)))
  (make-source-filename source "Short-Description-Nouns"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)(file-name (eql :short-production-nouns)))
  (make-source-filename source "Short-Production-Nouns"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :incorrect-tags)))
  (make-source-filename source "Tagged-Reference-Data-Incorrect"))

;;;-----------------------------------------------------------------------------

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :embedded-tags)))
  (make-source-filename source "Embedded-Type-Phrase-Tags"))

;;;-----------------------------------------------------------------------------

;;; It is somewhat redundant to have the keyword include 'meperia' but since
;;; this is a unique file rather than a pattern of files, so be it.

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :meperia-sources)))
  (make-source-filename source "Sources"))

;;;-----------------------------------------------------------------------------

;;; It is somewhat redundant to have the keyword include 'meperia' but since
;;; this is a unique file rather than a pattern of files, so be it.

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :meperia-nouns)))
  (make-source-filename source "Nouns"))

;;;-----------------------------------------------------------------------------

;;; It is somewhat redundant to have the keyword include 'meperia' but since
;;; this is a unique file rather than a pattern of files, so be it.

(defmethod MAKE-SOURCE-FILENAME ((source STRING)
				 (file-name (eql :meperia-attribution)))
  (make-source-filename source "Attribution"))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
