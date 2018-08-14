(cl:defpackage #:asdf-module
  (:use #:cl #:asdf #:module-manager))

(in-package #:asdf-module)

(defvar *source-directory*)
(defvar *target-directory*)

(multiple-value-setq (*source-directory* *target-directory*)
  (module-manager::module-directories :asdf-module))

;;; We declare all the asdf module directories explicitly.  This is
;;; for windows compatibility.  The trailing slash is necessary for
;;; asdf.  not sure whether this is still windows compatible.

(defparameter *asdf-modules*
    '(;; This open source aserver
      "aserve/"
      ;; These are all required for postmodern (pomo).
      "bordeaux-threads_0.5.1/"
      "closer-mop_0.55/"
      "ieee-floats/"
      "md5-1.8.5/"
      "postmodern-1.14/"
      "split-sequence/"
      "trivial-utf-8/"
      "usocket-0.4.1/"))

;;; "puri-1.5.1/"
;;; "cl-who-0.9.1/"
;;; "documentation-template-0.4.1-LH/"
;;; "closer-mop_0.55/"
;;; "cl-ppcre-1.3.3/"
;;; "fiveam-20080320/"
;;; "htmlgen/"
;;; "lw-compat_0.22/"

;; Add the directories to the *central-registry*. This means our
;; versions are chosen first.

(dolist (module *asdf-modules*)
  (pushnew (merge-pathnames module *source-directory*)
	   asdf:*central-registry* :test #'equal))

;; 11/10/09 mrh: And zap previously loaded component objects if they're one of our
;;               asdf modules.  Does some slightly lame things with trailing "/".
(maphash #'(lambda (module system-info)
	     (let* ((component (cdr system-info))
		    (raw-component-dir (car (last (pathname-directory (component-pathname component)))))
		    (component-dir (concatenate 'string raw-component-dir "/")))
	       (when (or (member component-dir *asdf-modules* :test #'equal)
			 (not (probe-file (component-pathname component))))
		 (warn "Zapping ASDF info about ~A module" module)
		 (remhash module asdf::*defined-systems*))))
	 asdf::*defined-systems*)


(defun translate (file source target)
  ;; this should work, but allegro munges symlinks.
   #+sbcl(merge-pathnames (enough-namestring file source) target)
   #+allegro(declare (ignore source))
   #+allegro(let* ((dir (pathname-directory file))
		   (position (search '("Utilities" "Source" "ASDF-Modules") dir :test #'equal)))
	      (if position
		  (merge-pathnames
		   (enough-namestring file (make-pathname :directory (subseq dir 0 (+ position 3))))
		   target)
		  file))) ;; if no match, we just let the file through.

;;;-------------------------------------------------------------------------

(defmethod asdf:output-files :around ((operation compile-op) (c source-file))
  "Translate directories for mini-module compilation."
  (loop for file in (call-next-method)
     for translated = (translate file *source-directory* *target-directory*)
      collect translated
     ;; BUG, should this check the mini-module default?
     do (ensure-directories-exist translated)))

;;;-------------------------------------------------------------------------
;;; End of File
;;;-------------------------------------------------------------------------

