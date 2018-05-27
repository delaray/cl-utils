
;;;------------------------------------------------------------------------------
;;; DAG CLASS
;;;------------------------------------------------------------------------------

(defclass DAG ()
  ((nodes :initform (make-array '(100) :initial-element nil)
	   :initarg :nodes
	   :accessor dag-nodes)))


;;;------------------------------------------------------------------------------
;;; DAGS
;;;------------------------------------------------------------------------------

(defclass DAG-NODE (NODE)
  ((parents :initform nil
	    :initarg :parents
	    :accessor inbound-nodes
	    :accessor node-parents)
   (children :initform nil
	     :initarg :children
	     :accessor outbound-nodes
	    :accessor node-children)
   (depth :initform 0
	  :initarg :depth
	  :accessor node-depth)
   (count :initform 1
	  :initarg :count
	  :accessor node-count)))

;;;---------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj DAG-NODE) stream)
  (format stream "#<~a: Value=~a, Depth=~a, Count=~a>"
	  (or (slot-value obj 'name) "Node")
	  (node-value obj)
	  (node-depth obj)
	  (node-count obj)))

;;;******************************************************************************
;;; DAGS
;;;******************************************************************************


;;;------------------------------------------------------------------------------
;;; MISCELLANEOUS NODE RETRIEVAL OPERATIONS
;;;------------------------------------------------------------------------------

(defmethod NODES-OF-DEPTH ((dag DAG) i)
  (when (dag-nodes dag)
    (svref (dag-nodes dag) i)))

;;;------------------------------------------------------------------------------

(defmethod NODE-SIBBLINGS ((node DAG-NODE) &optional (include-node-p nil))
  (when (node-parent node)
    (if include-node-p
	(node-children (node-parent node))
      (remove node (node-children (node-parent node))))))

;;;------------------------------------------------------------------------------
;;; DAG OPERATIONS
;;;------------------------------------------------------------------------------

(defmethod ADD-NODE-TO-DAG ((node DAG-NODE)(dag DAG))
  (pushnew node (svref (dag-nodes dag) (node-depth node))))

;;;------------------------------------------------------------------------------

(defmethod MAKE-DAG-NODE (value (dag DAG) &optional (depth 0))
  (let ((new-node (%make-dag-node value depth)))
    (add-node-to-dag new-node dag)
    new-node))

;;;----------------------------------------------------------------------------
;;; MAKE-DAG-NODE
;;;----------------------------------------------------------------------------

;;; Uses *dag-node-resources* to resource old nodes if possible.

(defun %MAKE-DAG-NODE (value depth)
  (let* ((new-node))
    (cond (*dag-node-resources*
	   (setf new-node (first *dag-node-resources*))
	   (setf *dag-node-resources* (cdr *dag-node-resources*))
	   (setf (node-value new-node) value)
	   (setf (node-depth new-node) depth)
	   (setf (node-parents new-node) nil)
	   (setf (node-children new-node) nil)
	   (setf (node-count new-node) 1)
	   new-node)
	  (t
	   (make-instance 'dag-node :value value :depth depth)))))

;;;----------------------------------------------------------------------------

(defmethod ADD-CHILD ((child DAG-NODE)(parent DAG-NODE))
  (pushnew child (node-children parent))
  (pushnew parent (node-parents child))
  child)

;;;----------------------------------------------------------------------------

(defmethod DELETE-CHILD ((child DAG-NODE))
  (dolist (parent (node-parents child))
    (setf (node-children parent)
      (remove child (node-children parent) :test #'eq)))
  (setf (node-parents child) nil))

;;;----------------------------------------------------------------------------

(defmethod DELETE-PARENT ((child DAG-NODE)(parent DAG-NODE))
  (setf (node-children parent)
    (remove child (node-children parent) :test #'eq))
  (setf (node-parents child) 
    (remove parent (node-parents child) :test #'eq)))


;;;------------------------------------------------------------------------------
;;; FIND NODE
;;;------------------------------------------------------------------------------

(defmethod FIND-NODE (value nodes &optional depth)
  (declare (ignore depth))
  (find value nodes :test #'equal :key #'node-value))

;;;------------------------------------------------------------------------------

(defmethod FIND-NODE (value (dag DAG) &optional (depth 0))
  (find-node value (svref (dag-nodes dag) depth)))

;;;------------------------------------------------------------------------------

(defmethod FIND-NODE ((node DAG-NODE) (dag DAG) &optional (depth 0))
  (declare (ignore depth))
  (find node (svref (dag-nodes dag) (node-depth node)) :test #'eq))

;;;------------------------------------------------------------------------------
;;; FIND-NODE*
;;;------------------------------------------------------------------------------

(defmethod FIND-NODE* (value (dag DAG) &optional (depth 0)(parent nil))
  (cond (parent
	 (find-node value (node-children parent)))
	(t
	 (find-node value (svref (dag-nodes dag) depth)))))


;;;-----------------------------------------------------------------------------
;;; DAG NODE ACCESSORS
;;;-----------------------------------------------------------------------------

(defmethod NODE-NAME ((node DAG-NODE))
  (format nil "~a" (node-value node)))

;;;-----------------------------------------------------------------------------

(defmethod NODE-CHILD ((node DAG-NODE))
  (first (node-children node)))

;;;-----------------------------------------------------------------------------

(defmethod NODE-PARENT ((node DAG-NODE))
  (first (node-parents node)))

;;;-----------------------------------------------------------------------------

(defmethod LEAF-NODE-P ((node DAG-NODE))
  (null (node-children node)))

;;;-----------------------------------------------------------------------------

(defmethod NODE-HAS-SINGLE-CHILD-P ((node DAG-NODE))
  (= (length (node-children node)) 1))

;;;---------------------------------------------------------------------------

(defmethod NODE-HAS-SINGLE-PARENT-P ((node DAG-NODE))
  (= (length (node-parents node)) 1))

;;;---------------------------------------------------------------------------

(defmethod EVERY-CHILD-HAS-COUNT-1 ((node DAG-NODE))
  (every #'(lambda (x)(= (node-count x) 1)) (node-children node)))

;;;---------------------------------------------------------------------------

(defmethod EVERY-NODE-HAS-SAME-CHILD ((nodes CONS))
  (let ((same-child-p t)
	(first-child (node-child (first nodes))))
    (dolist (node (rest nodes))
      (unless (eq first-child (node-child node))
	(setf same-child-p nil)))
    same-child-p))

;;;---------------------------------------------------------------------------
;;; End of File
;;;---------------------------------------------------------------------------

