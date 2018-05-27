(in-package :CL-USER) 

;;;----------------------------------------------------------------------------

(unless (find-package "UTILITIES")
  (make-package "UTILITIES"
		:use '(:common-lisp
		       :cl-user
		       #+ALLEGRO :excl
		       #-SBCL :clos
		       #+SBCL :sb-mop)
		:nicknames '(:util)))

;;;----------------------------------------------------------------------------

(defparameter *UTILITIES-DEVICE*
    (pathname-device *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *UTILITIES-DIRECTORY*
   (pathname-directory *load-truename*))

;;;----------------------------------------------------------------------------

(defparameter *UTILITIES-ROOT*
    (make-pathname :name nil :type nil
                   :directory  (pathname-directory *load-truename*)
                   :defaults  *load-truename*))

;;;---------------------------------------------------------------------------
;;; PATHNAMES
;;;---------------------------------------------------------------------------

(defun MAKE-DATA-PATHNAME (filename &optional (type "txt"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Trunk" "Data")))

;;;---------------------------------------------------------------------------

(defun MAKE-RESULTS-PATHNAME (filename &optional (type "txt"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Trunk" "Results")))

;;;---------------------------------------------------------------------------

(defun MAKE-GRAPHS-PATHNAME (filename &optional (type "lisp"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Trunk" "Data" "Graphs")))

;;;----------------------------------------------------------------------------

(load "C:\\Projects\\trunk\\Utilities\\Source\\Variables.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Values.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Classes.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Split-Sequence.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Utilities.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Macros.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Math.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Lists.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Strings.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Sets.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\File.lisp")
(load "C:\\Projects\\trunk\\Utilities\\Source\\Trees.lisp")
;;(load "C:\\Projects\\trunk\\Utilities\\Source\\Web.lisp")
;;(load "C:\\Projects\\trunk\\Utilities\\Source\\Source-Filename.lisp")

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
