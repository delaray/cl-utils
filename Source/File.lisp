(in-package :UTILITIES)

;;;************************************************************************
;;; FILE UTILITES
;;;************************************************************************
;;;
;;; MAKE-DATA-PATHNAME
;;; MAKE-RESULTS-PATHNAME
;;; MAKE-GRAPHS-PATHNAME
;;; MAKE-MW-PATHNAME
;;;
;;; WRITE-LIST-TO-FILE
;;; WRITE-HASH-TABLE-TO-FILE
;;; MAP-DELIMITED-FILE
;;; WRITE-DELIMITED-FILE
;;;
;;; WRITE-BINARY-FILE
;;;
;;;************************************************************************

;;;-----------------------------------------------------------------------------
;;; MAKE-DATA-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-DATA-PATHNAME (filename &optional (type "txt"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Data")))

;;;-----------------------------------------------------------------------------
;;; MAKE-RESULTS-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-RESULTS-PATHNAME (filename &optional (type "txt"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Results")))

;;;-----------------------------------------------------------------------------
;;; MAKE-GRAPHS-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-GRAPHS-PATHNAME (filename &optional (type "lisp2")(subdirectory nil))
  (let ((directory '(:absolute "Projects" "Data" "Graphs")))
    (when subdirectory
      (setf directory `(,@directory ,subdirectory)))
    (make-pathname :name filename
		   :type type
		   :device (pathname-device *utilities-root*)
		   :directory directory)))

;;;-----------------------------------------------------------------------------

(defun MAKE-GRAPHS-DIRECTORY (&optional (subdirectory nil))
  (let ((directory '(:absolute "Projects" "Data" "Graphs")))
    (when subdirectory
      (setf directory `(,@directory ,subdirectory)))
    (make-pathname :device (pathname-device *utilities-root*)
		 :directory subdirectory)))

;;;-----------------------------------------------------------------------------
;;; MAKE-MW-PATHNAME
;;;-----------------------------------------------------------------------------

(defun MAKE-MW-PATHNAME (filename &optional (type "html"))
  (make-pathname :name filename
		 :type type
		 :device (pathname-device *utilities-root*)
		 :directory '(:absolute "Projects" "Data" "Merriam-Webster")))

;;;---------------------------------------------------------------------------
;;; WRITE-LIST-TO-FILE
;;;---------------------------------------------------------------------------

(defun WRITE-LIST-TO-FILE (list &optional (filename "File"))
  (with-results-pathname (filename)
    (mapc #'(lambda (x)
	      (cond ((atom x)
		     (format t "~a~%" x))
		    (t
		     (dolist (elmt x)
		       (write-tabbed-entry elmt t))
		     (format t "~%"))))
	  list)))

;;;---------------------------------------------------------------------------
;;; WRITE-HASH-TABLE-TO-FILE
;;;---------------------------------------------------------------------------

;;; Note: Uses one field for the key and multiple fields if value is a list
;;; Note: Uses MAKE-DESCRIPTION-STRING to convert keys & values to strings.

(defun WRITE-HASH-TABLE-TO-FILE (hash-table filename 
				 &key 
				 (inner-delimiter #\,)
				 #+IGNORE
				 (key-fields 1)
				 (headers nil)
				 #+IGNORE
				 (key-1-formatter #'identity))
  ;; Ensure a pathname
  (unless (pathnamep filename)
    (setf filename (make-data-pathname filename "txt")))
  (print filename)
  (format t "~&Writing table to file:~%~a ..."  filename)
  (with-results-pathname (filename)
    (when headers
      (dolist (header headers)
	(write-tabbed-entry header t)))
    (maphash #'(lambda (key values)
		 (format t "~%")
		 (cond ((atom key)
			(write-tabbed-entry key t))
		       (t
			(dolist (sub-key key)
			  (write-tabbed-entry sub-key t))))
		 (if (atom values)
		     (write-tabbed-entry values t)
		   (dolist (value values)
		     (cond ((atom value)
			    (write-tabbed-entry value t))
			   (t
			    (write-tabbed-entry
			     (make-description-string value :delimiter inner-delimiter)
			     t))))))
	     hash-table))
  (remhash nil hash-table)
  hash-table)


;;;---------------------------------------------------------------------------
;;; LOAD-HASH-TABLE-FROM-FILE
;;;---------------------------------------------------------------------------

;;; Note: Uses one field for the key and multiple fields if value is a list
;;; Note: Uses MAKE-DESCRIPTION-STRING to convert keys & values to strings.

(defun LOAD-HASH-TABLE-FROM-FILE (filename 
				  &key 
				  (inner-delimiter #\,)
				  (table (make-hash-table :size 20 :test #'equalp)))
  ;; Ensure a pathname
  (unless (pathnamep filename)
    (setf filename (make-data-pathname filename "txt")))
  (format t "~&Loading table from file ~a ..." (pathname-name filename))
  (when (probe-file filename)
    (map-delimited-file
     filename
     #'(lambda (tokens)
	 (setf (gethash (first tokens) table)
	   (mapcar #'(lambda (x)
		       (split-sequence inner-delimiter x))
		   (mapcar #'cleanup-token (rest tokens))))))o
    (remhash nil table))
  table)
  

;;;---------------------------------------------------------------------------
;;; LOAD-HASH-TABLE-FROM-LISP-FILE
;;;---------------------------------------------------------------------------

;;; Note: Uses one field for the key and multiple fields if value is a list
;;; Note: Uses MAKE-DESCRIPTION-STRING to convert keys & values to strings.

(defun LOAD-HASH-TABLE-FROM-LISP-FILE (filename 
				       &key 
				       (inner-delimiter #\,)
				       (table (make-hash-table :size 20 :test #'equalp)))
  ;; Ensure a pathname
  (unless (pathnamep filename)
    (setf filename (make-data-pathname filename "lisp")))
  (format t "~&Loading table from file ~a ..." (pathname-name filename))
  (with-open-file (file filename :direction :input)
    (let* ((forms (read file)))
      (dolist (form forms)
	(setf (gethash (cleanup-token (first form)) table)
	  (mapcar #'(lambda (x)
		      (split-sequence inner-delimiter x))
		  (mapcar #'cleanup-token (rest form)))))))
  table)
  

;;;-----------------------------------------------------------------------------
;;; MAP-DELIMITED-FILE
;;;-----------------------------------------------------------------------------

(defun MAP-DELIMITED-FILE (pathname function &key (delimiter #\tab))
  (with-open-file (file pathname :direction :input)
    (let ((eof -1)
	  (next-line nil))
      (loop 
	(setf next-line (read-line file nil eof))
	(when (equal next-line eof)
	  (return t))
	(funcall function (butlast (split-sequence delimiter next-line))))
      t)))


;;;-----------------------------------------------------------------------------
;;; WRITE-DELIMITED-FILE
;;;-----------------------------------------------------------------------------

(defun WRITE-DELIMITED-FILE (pathname headers hash-table &key (delimiter #\tab))
 (with-open-file (file pathname :direction :output :if-exists :supersede)
   (dolist (header headers)
     (format file "~s~a" header delimiter))
   (maphash #'(lambda (key values)
		(format file "~%~s~a" key #\tab)
		(dolist (value values)
		  (format file "~s~a" value #\tab)))
	    hash-table)
   t))


;;;---------------------------------------------------------------------------
;;; WRITE-TABBED-ENTRY
;;;---------------------------------------------------------------------------

(defun WRITE-TABBED-ENTRY (elmt stream)
  (format stream "~a~a" elmt #\tab))

;;;-----------------------------------------------------------------------------
;;; WRITE-BINARY-FILE
;;;-----------------------------------------------------------------------------

(defmethod WRITE-BINARY-FILE ((data ARRAY)(pathname PATHNAME))
  (with-open-file (file pathname :direction :output :if-exists :supersede)
    (dotimes (i (length data))
      (write-byte (aref data i) file)))
    t)

;;;---------------------------------------------------------------------------
;;; FILES-IN-DIRECTORY
;;;---------------------------------------------------------------------------

#+ALLEGRO
(defun FILES-IN-DIRECTORY (&optional (directory (current-directory)))
  (let ((files nil))
    (#+ALLEGRO excl::map-over-directory 
     #+SBCL sbcl-impl::map-directory
     #'(lambda (x)
	 (when (pathname-name x)
	   (push x files)))
     directory
     :include-directories nil
     :recurse nil)
    files))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------

	  