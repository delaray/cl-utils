(in-package :UTILITIES)

;;;----------------------------------------------------------------------------
;;; Global Utilities Pathname Related Variables
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

;;;----------------------------------------------------------------------------

(defvar *NULL-STRING* "")

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
