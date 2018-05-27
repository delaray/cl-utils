(in-package :module-manager)

;;; ===========================================================================
;;;
;;; Author: Raymond de Lacaze
;;;
;;; This file defines various Utilities using GBBopen's mini-module system.
;;;
;;; ===========================================================================

;;;----------------------------------------------------------------------------
;;; UTILITIES Package
;;;----------------------------------------------------------------------------

(eval-when (eval compile load)
  (unless (find-package "UTILITIES")
    (make-package "UTILITIES"
		  :use '(:common-lisp
			 :cl-user
			 #-SBCL :clos
			 #-SBCL :excl)
		  :nicknames '(:util))))


(define-root-directory '(:utilities-root) *load-truename*)

;;;----------------------------------------------------------------------------
;;; Support for external ASDF libraries
;;;----------------------------------------------------------------------------

(define-module :ASDF-MODULE
  (:directory :utilities-root "ASDF-Modules")
  (:files "asdf"
	  "asdf-module"))

;;;----------------------------------------------------------------------------

(define-module :ASERVE
  (:REQUIRES :asdf-module)
  (:directory :utilities-root "ASDF-Modules")
  (:files "aserve-module"))

;;;----------------------------------------------------------------------------

(define-module :POSTMODERN
  (:requires :asdf-module)
  (:directory :utilities-root "ASDF-Modules")
  (:files "postmodern-module"))

;;;----------------------------------------------------------------------------
;;; Utilities
;;;----------------------------------------------------------------------------

(define-module :Utilities
  (:directory :utilities-root)
  (:files "Variables"
	  "Classes"
	  "Macros"
	  "Split-Sequence"
	  "Utilities"
	  "Math"
	  "Math-3D"
	  "Values"
	  "Strings"
	  "Lists"
	  "Sets"
	  "File"
	  "Trees"
	  "Web"
	  "Source-Filename"))

;;;----------------------------------------------------------------------------
;;; Database
;;;----------------------------------------------------------------------------

;;; This uses POSTMODERN to connect to PostGres Databases.

(define-module :DATABASE
  (:directory :utilities-root)
  (:requires :utilities :postmodern)
  (:files "Database"))

;;;----------------------------------------------------------------------------
;;; TREE-GRAPHICS
;;;----------------------------------------------------------------------------

(define-module :TREE-GRAPHICS
  (:directory :utilities-root)
  (:requires :gom :utilities)
  (:files "Tree-Graphics"))

;;;----------------------------------------------------------------------------
;;; WEB
;;;----------------------------------------------------------------------------

(define-module :WEB
  (:directory :utilities-root)
  (:requires :utilities)
  (:files "Web"))

;;;----------------------------------------------------------------------------

(in-package :utilities)

;;;-------------------------------------------------------------------------
;;; End of File
;;;-------------------------------------------------------------------------
