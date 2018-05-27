;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(defpackage :utilities (:use #:asdf #:cl) (:nicknames "UTIL"))

(pushnew :no-internet *features*)

;;;----------------------------------------------------------------------------
;;; Utilities Module System Definition
;;;----------------------------------------------------------------------------

(in-package :utilities)

(defsystem utilities
  :author "Raymond de Lacaze <delaray@hotmail.com>"
  :version "1.0"
  :maintainer  "Raymond de Lacaze <delaray@hotmail.com>"
  :licence "MIT Style license for the packaging."
  :description "Miscellaneous CL Utilities"
  :long-description  "Miscellaneous CL Utilities"
  :components (#+IGNORE
	       (:static-file "COPYING")
               (:module "Source"
                        :components (#+IGNORE
				     (:static-file "README")
				     (:file "Variables")
				     (:file "Values")
				     (:file "Classes")
				     (:file "Split-Sequence")
				     (:file "Utilities")
				     (:file "Macros")
				     (:file "Math")
				     (:file "Lists")
				     (:file "Strings")
				     (:file "Sets")
				     (:file "Trees")
				     (:file "File")
				     (:file "Web")))))
					    
#+IGNORE
(asdf:oos 'asdf:load-op :utilities)

;;;-----------------------------------------------------------------------------
;;; MAKE-DATA-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-DATA-PATHNAME (filename &optional (type "txt"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Trunk" "Data")))

;;;-----------------------------------------------------------------------------
;;; MAKE-RESULTS-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-RESULTS-PATHNAME (filename &optional (type "txt"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Trunk" "Results")))

;;;-----------------------------------------------------------------------------
;;; MAKE-GRAPHS-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-GRAPHS-PATHNAME (filename &optional (type "lisp"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Trunk" "Data" "Graphs")))

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
