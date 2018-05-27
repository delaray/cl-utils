(in-package :utilities)

;;;-----------------------------------------------------------------
;;; ALL-ORDERINGS
;;;-----------------------------------------------------------------

;;; Returns all possible orderings of the elements in set

 (defun ALL-ORDERINGS (set)
  (cond ((null set)
	 nil)
	((= (length set) 1)
	 (list set))
	(t
	 (apply #'append 
		(mapcar #'(lambda (elmt)
			    (let ((set (remove elmt set)))
			      (mapcar #'(lambda (x)
					  (cons elmt x))
				      (all-orderings set))))
			set)))))

;;;-----------------------------------------------------------------
;;; Power Set
;;;-----------------------------------------------------------------

;;; Returns the proper Power-set of the elements in set

(defun POWER-SET (set)
  (cond ((null set)
	 nil)
	((= (length set) 1)
	 (list set))
	(t
	 (append (apply #'append
			(mapcar #'(lambda (x)
				    (power-set (remove x set)))
				set))
		 (mapcar #'(lambda (x)
			     (cons (first set) x))
			 (power-set (rest set)))))))


;;;-----------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------
