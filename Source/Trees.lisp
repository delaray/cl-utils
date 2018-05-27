(in-package :UTILITIES)

;;;***************************************************************************
;;; TREE-MIXIN
;;;***************************************************************************
;;;
;;; Classes
;;; -------
;;;
;;; NODE
;;; TREE-MIXIN
;;; TREE-NODE
;;; DAG
;;; DAG-NODE
;;;
;;;
;;; Functions
;;; ---------
;;;
;;; MAKE-TREE-NODE
;;; ADD-CHILD
;;; DELETE-CHILD
;;;
;;; DELETE-INTERNAL-NODE
;;; DELETE-LEAF-NODE
;;; DELETE-SUBTREE
;;;
;;; FIND-LEAF-NODES

;;; LEAF-NODE-P
;;; INTERNAL-NODE-P
;;;
;;; MAP-TREE-NODES
;;; MAP-LEAF-NODES
;;;
;;; NODES-OF-DEPTH
;;;
;;; PRINT-TREE
;;; PRETTY-PRINT-TREE
;;;
;;;
;;;
;;;*****************************************************************************


;;;*****************************************************************************
;;; NODE & TREE CLASSES
;;;*****************************************************************************

;;;---------------------------------------------------------------------------
;;; TREE-MIXIN
;;;---------------------------------------------------------------------------

(defclass TREE-MIXIN ()
  ((parent :initform nil
	   :initarg :parent
	   :accessor object-parent
	   :accessor node-parent)
   (children :initform nil
	     :initarg :children
	     :accessor object-children
	     :accessor node-children)
   (depth :initform 0
	  :initarg :depth
	  :accessor object-depth
	  :accessor node-depth)
   (collapsed-p :initform nil
		:initarg :collapsed-p
		:accessor node-collapsed-p)))

;;;---------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj TREE-MIXIN) stream)
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (print-unreadable-object (obj stream :type t :identity nil)
	   (format stream "Name: ~a, Value: ~a, Depth: ~a"
		   (object-name obj)
		   (node-value obj)
		   (node-depth obj))))))

;;;---------------------------------------------------------------------------
;;; NODE CLASS
;;;---------------------------------------------------------------------------

;;; Because more often than not we will both a value and a name
;;; associated with a node in a tree or a graph as well as statiscal
;;; information about that node we have chosen to have node inherit
;;; from value. In the event that this proves too cumbersome at this
;;; level in the class hierarchy, the VALUE class can be inherited
;;; from some of the classes that inherit from NODE such as the
;;; CONCEPT class.

(defclass NODE (VALUE)
  ((name :accessor node-name)
   (value :accessor node-value)))

;;;---------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj NODE) stream)
  (format stream "#<Node: ~a>" (node-name obj)))

;;;---------------------------------------------------------------------------
;;; TREE-NODE
;;;---------------------------------------------------------------------------

(defclass TREE-NODE (TREE-MIXIN NODE)
  ())


;;;****************************************************************************
;;; TREE API
;;;****************************************************************************

;;;----------------------------------------------------------------------------
;;; MAKE-TREE-NODE
;;;----------------------------------------------------------------------------

(defun MAKE-TREE-NODE (name &key (depth 0)(class 'TREE-NODE)(value nil))
  (make-instance class :name name :value value :depth depth))

;;;---------------------------------------------------------------------------
;;; ADD-CHILD
;;;---------------------------------------------------------------------------

(defmethod ADD-CHILD ((child TREE-MIXIN)(parent TREE-MIXIN))
  (setf (node-children parent)
    (append (node-children parent)(list child)))
  (setf (node-parent child) parent)
  child)

;;;----------------------------------------------------------------------------
;;; DELETE-CHILD
;;;----------------------------------------------------------------------------

(defmethod DELETE-CHILD ((child TREE-MIXIN))
  (when (node-parent child)
    (setf (node-children (node-parent child))
      (remove child (node-children (node-parent child)) :test #'eq)))
  (setf (node-parent child) nil))

;;;-----------------------------------------------------------------------------
;;; DELETE-INERNAL-NODE
;;;-----------------------------------------------------------------------------

;;; This removes <node> from tree and makes any children of node,
;;; children of the parent of <node>.

(defmethod DELETE-INTERNAL-NODE ((node TREE-MIXIN))
  (let* ((children (object-children node))
	 (parent (object-parent node))
	 (parent-children  (object-children parent))
	 (position (position node parent-children)))
    (when (object-children parent)
      (setf (object-children node) nil)
      (setf (object-parent node) nil)
      (setf (object-children parent)
	(append (subseq parent-children 0 position)
		children
		(subseq parent-children (1+ position))))
      (mapc #'(lambda (child)
		(setf (object-parent child) parent))
	    children))))

;;;-----------------------------------------------------------------------------
;;; DELETE-LEAF-NODE
;;;-----------------------------------------------------------------------------

(defmethod DELETE-LEAF-NODE ((child TREE-MIXIN))
  (cond ((object-children child)
	 (error "~a is not a leaf node, it has ~a children." 
		child (length (object-children child))))
	(t
	 (when (node-parent child)
	   (setf (node-children (node-parent child))
	     (remove child (node-children (node-parent child)) :test #'eq))))))

;;;-----------------------------------------------------------------------------
;;; DELETE-SUBTREE
;;;-----------------------------------------------------------------------------

(defmethod DELETE-SUBTREE ((child TREE-MIXIN))
  (delete-child child)
  child)

;;;----------------------------------------------------------------------------
;;; FIND-LEAF-NODES
;;;----------------------------------------------------------------------------

(defmethod FIND-LEAF-NODES ((node TREE-MIXIN)
			    &key
			    (leaf-nodes nil)
			    (result-accessor #'identity))
  (cond ((leaf-node-p node)
	 (push (funcall result-accessor node) leaf-nodes)
	 leaf-nodes)
	(t
	 (dolist (child (node-children node))
	   (setf leaf-nodes
	     (find-leaf-nodes child
			      :leaf-nodes leaf-nodes
			      :result-accessor result-accessor)))
	 leaf-nodes)))

;;;----------------------------------------------------------------------------
;;; LEAF-NODE-P
;;;----------------------------------------------------------------------------

(defmethod LEAF-NODE-P ((node TREE-MIXIN))
  (null (node-children node)))

;;;----------------------------------------------------------------------------
;;; INTERNAL-NODE-P
;;;----------------------------------------------------------------------------

(defmethod INTERNAL-NODE-P ((node TREE-MIXIN))
  (not (leaf-node-p node)))

;;;----------------------------------------------------------------------------
;;; MAP-TREE-NODES
;;;----------------------------------------------------------------------------

(defmethod MAP-TREE-NODES ((node TREE-MIXIN) map-fn
			   &key
			   (child-accessor #'node-children))
  (funcall map-fn node)
  (mapc #'(lambda (x)(map-tree-nodes x map-fn))
	(funcall child-accessor node)))

;;;----------------------------------------------------------------------------
;;; MAX-TREE-DEPTH
;;;----------------------------------------------------------------------------

(defmethod MAX-TREE-DEPTH ((node TREE-MIXIN))
  (let* ((max-depth 0))
    (map-tree-nodes
     node
     #'(lambda (x)
	 (setf max-depth (max max-depth (node-depth x)))))
    max-depth))

;;;----------------------------------------------------------------------------
;;; MAP-LEAF-NODES
;;;----------------------------------------------------------------------------

(defmethod MAP-LEAF-NODES ((node TREE-MIXIN) map-fn)
  (cond ((leaf-node-p node)
	 (funcall map-fn node))
	(t
	 (mapc #'(lambda (x)(map-leaf-nodes x map-fn))
	       (node-children node)))))

;;;----------------------------------------------------------------------------
;;; NODES-OF-DEPTH
;;;----------------------------------------------------------------------------

;;; Returns all nodes of a particular depth.

(defmethod NODES-OF-DEPTH ((node TREE-MIXIN) i)
  (cond ((= i 0)
	 (list node))
	 ((= i 1)
	  (node-children node))
	 (t
	  (apply #'append 
		 (mapcar #'(lambda (child)(nodes-of-depth child (1- i)))
			 (node-children node))))))

;;;-----------------------------------------------------------------------------
;;; PRINT-TREE
;;;-----------------------------------------------------------------------------

(defmethod PRINT-TREE ((node TREE-MIXIN)
		       &key 
		       (depth 0)
		       (children-accessor #'node-children))
  (format t "~%")
  (dotimes (i (* depth 3))
    (princ " "))
  (format t "~a" (node-name node))
  (mapc #'(lambda (x)
	    (print-tree x :depth (1+ depth) :children-accessor children-accessor))
	(funcall children-accessor node)))


;;;---------------------------------------------------------------------------
;;; PRETTY-PRINT-TREE
;;;---------------------------------------------------------------------------

(defmethod PRETTY-PRINT-TREE ((node TREE-MIXIN)  
			      &key 
			      (depth 0)
			      (children-accessor #'node-children))
  (format t "~%")
  (dotimes (i  depth)
    (format t "   "))
  (format t " -> ~a" (node-name node))
  (mapc #'(lambda (x)
	    (pretty-print-tree x :depth (1+ depth) :children-accessor children-accessor))
	(funcall children-accessor node))
  t)


;;;*****************************************************************************
;;; Building Directory Trees From PQ Trees (TP)
;;;*****************************************************************************

;;; -----------------------------------------------------------------
;;; Traverses a tree of nodes in depth first manner,
;;; applying nd-function to each node.
;;; -----------------------------------------------------------------

(defmethod MAP-NODES-WITH-NODE-LIST (nd-function 
                                      (parent TREE-NODE) 
                                      &optional (node-list nil))
  (if (null node-list) (setf node-list (list parent)))
  (funcall nd-function parent node-list)
  (dolist (each-child (node-children parent))
    (map-nodes-with-node-list nd-function 
                               each-child 
                               (append node-list (list each-child)))))

;;; -----------------------------------------------------------------

;;; Turn a list of atoms into a list of strings without

(defun STRINGIFY-LIST (value-list)
  (let ((results nil))
    (dolist (each-item value-list)
      (push (stringify-value each-item) results))
    (nreverse results)))

;;; -----------------------------------------------------------------

(defun STRINGIFY-VALUE (each-item)
  (if (stringp each-item)
      each-item
    (format nil "~s" each-item)))

;;; -----------------------------------------------------------------

;;; From a list of node strings, do the equivalent of a mkdir.

(defun MAKE-DIRECTORY-FROM-LIST (dir-list)
  (ensure-directories-exist (make-dir-pathname dir-list) :verbose nil))


;;; Create a pathname from a list of node strings.

(defun MAKE-DIR-PATHNAME (dir-string &optional (dir-offset (user-homedir-pathname)))
  (cond ((listp dir-string)
         (make-pathname :device (pathname-device dir-offset)
                        :directory `(,@(pathname-directory dir-offset) 
                                       ,@dir-string)))
        (t
         (make-pathname :device (pathname-device dir-offset)
                        :directory `(,@(pathname-directory dir-offset) 
                                       ,dir-string)))))


;;;------------------------------------------------------------------------------
;;; CREATE-DIRECTORY-TREE
;;;------------------------------------------------------------------------------

;;; This is an example of a top-level function that creates
;;; a directory hierarchy from the node-values of the nodes
;;; in the tree.

(defmethod CREATE-DIRECTORY-TREE ((tree-root TREE-NODE))
  (map-nodes-with-node-list 
   #'(lambda (tree-node node-list)
       (let* ((dir-value-list (mapcar #'pretty-node-value node-list))
	      (file-value-list (pretty-node-contents tree-node))
	      (directory (make-directory-from-list (stringify-list dir-value-list))))
	 (print directory)
	 (mapcar #'(lambda (filename)
		     (setf filename
		       (make-pathname :name filename
				      :directory (pathname-directory  directory)
				      :device (pathname-device directory)))
		     (with-open-file (file filename
				      :direction :output
				      :if-does-not-exist :create
				      :if-exists :supersede)
		       (format file " ")))
		 file-value-list)))
   tree-root))

;;;------------------------------------------------------------------------------

;;; Default method just calls stringy-value. Specialized methods can be
;;; written on individual subclasses.

(defmethod PRETTY-NODE-VALUE ((node TREE-NODE))
  (stringify-value (node-value node)))

;;;------------------------------------------------------------------------------

;;; If nodes have 'content' associated with them we can display these
;;; as files in the directory hierarchy.

;;; The default method assumes no node content.  Specialized methods can be
;;; written on individual subclasses to provide content.

(defmethod PRETTY-NODE-CONTENTS ((node TREE-NODE))
  nil)

;;;************************************************************************
;;; SAVE & RESTORE
;;;************************************************************************

;;;-----------------------------------------------------------------------------
;;; SAVE-TREE
;;;-----------------------------------------------------------------------------

(defgeneric SAVE-TREE (tree format destination))

;;;-----------------------------------------------------------------------------

(defmethod SAVE-TREE ((tree-node TREE-NODE)(format (eql :lisp))(file-name STRING))
  (setf file-name (make-data-pathname file-name "lisp"))
  (format t "~%Saving graph to ~a..." file-name)
  (with-open-file (file file-name :direction :output :if-exists :supersede)
    ;; First Write the vertices
    (format file ";;; List of nodes.")
    (format file "~%(")
    (map-tree-nodes 
     tree-node
     #'(lambda (node)
	 (format file "~%(~s ~d)"
		 (node-name node)
		 (node-depth node))))
    (format file "~%)")
     ;; Now write the edges
    (format file "~%~%;;; List of edges")
    (format file "~%(")
    (map-tree-nodes 
     tree-node
     #'(lambda (node)
	 (dolist (child (object-children node))
	 (format file "~%(~s ~s)"
		 (object-name node)
		 (object-name child)))))
  (format file "~%)"))
   t)

;;;-----------------------------------------------------------------------------
;;; RESTORE-TREE
;;;-----------------------------------------------------------------------------

(defgeneric RESTORE-TREE (format source))

;;;-----------------------------------------------------------------------------

(defmethod RESTORE-TREE ((format (eql :lisp))(file-name STRING))
  (setf file-name (make-data-pathname file-name "lisp"))
  (format t "~%Restoring graph fromo ~a..." file-name)
  (with-open-file (file file-name :direction :input)
    (let ((nodes (read file))
	  (edges (read file))
	  (tree-nodes nil))
      ;; Create the vertices
      (dolist (node nodes)
	(push (make-tree-node (first node) :depth (second node))
	      tree-nodes))
      (setf tree-nodes (nreverse tree-nodes))
      ;; Create the edges
      (dolist (edge edges)
	(let* ((parent (find (first edge) tree-nodes
			     :test #'equal :key #'node-name))
	       (child (find (second edge) tree-nodes
			    :test #'equal :key #'node-name)))
	  (when (and parent child)
	    (add-child child parent))))
      ;; Return the graph
      (values  (first tree-nodes) tree-nodes))))


;;;----------------------------------------------------------------------------
;;; MAKE-SAMPLE-TREE
;;;----------------------------------------------------------------------------

(defun MAKE-SAMPLE-TREE ()
  (let* ((one (make-tree-node "1" :depth 0))
	 (two (make-tree-node "2" :depth 1))
	 (three (make-tree-node "3" :depth 1))
	 (four (make-tree-node "4" :depth 2))
	 (five (make-tree-node "5" :depth 2))
	 (six (make-tree-node "6" :depth 2))
	 (seven (make-tree-node "7" :depth 2))
	 (eight (make-tree-node "8" :depth 3))
	 (nine (make-tree-node "9" :depth 3))
	 (ten (make-tree-node "10" :depth 3))
	 (eleven (make-tree-node "11" :depth 3)))
    (add-child two one)
    (add-child three one)
    (add-child four two)
    (add-child five two)
    (add-child six three)
    (add-child seven three)
    (add-child eight four)
    (add-child nine four)
    (add-child ten five)
    (add-child eleven five)
    one))
	 
;;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------

