(in-package :utilities)

;;;**************************************************************************
;;; LIST UTILITIES
;;;**************************************************************************
;;;
;;; FLATTEN-LIST
;;; NTHCAR
;;; NTH-HASH
;;;
;;;**************************************************************************

(defun FLATTEN-LIST (x)
  (cond ((null x)
	 nil)
	((atom x)
	 x)
	((atom (first x))
	 (cons (first x) (flatten-list (rest x))))
	(t
	 (append (flatten-list (first x))
		 (flatten-list (rest x))))))
    
;;;----------------------------------------------------------------------------
;;; NTHCAR
;;;----------------------------------------------------------------------------

(defun NTHCAR (n list)
  (let ((result nil)
	(count 0))
    (dolist (elmt list)
      (when (>= count n)
	(return t))
      (push elmt result)
      (incf count))
    (nreverse result)))
      
;;;----------------------------------------------------------------------------
;;; NTH-HASH
;;;----------------------------------------------------------------------------

(defmethod NTH-FIRST ((n INTEGER)(ht HASH-TABLE))
  (let ((results nil)
	(count 1))
    (maphash #'(lambda (key value)
		 (push (list key value) results)
		 (incf count)
		 (when (> count n)
		   (return-from nth-first results)))
	     ht)
    results))
		      
;;;----------------------------------------------------------------------------
;;; SINGLETONP
;;;----------------------------------------------------------------------------

(defmethod SINGLETONP ((list LIST))
  (null (cdr list)))

;;;----------------------------------------------------------------------------

(defmethod SINGLETONP ((list NULL))
  nil)
		      
;;;----------------------------------------------------------------------------
;;; PRETTY-PRINT-LIST
;;;----------------------------------------------------------------------------

(defmethod PRETTY-PRINT-LIST ((list LIST))
  (mapc #'(lambda (element)
	    (cond ((atom element)
		   (print element))
		  (t
		   (mapc #'print element)))
	    (format t "~%---------------------------------------------------"))
	list)
  t)

;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
