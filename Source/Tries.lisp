(in-package :UTILITIES)

;;;***************************************************************************
;;; TRIES
;;;***************************************************************************
;;;
;;; TRIE-NODE
;;; MAKE-TRIE-NODE
;;; ADD-SEQUENCE-TO-TRIE
;;; FIND-TOKEN-IN-TRIE
;;; FIND-TOKENS-IN-TRIE
;;; TOKENS-IN-TRIE
;;; FIND-TOKEN-IN-TRIES
;;; FIND-ONLY-ONE-IN-TRIE
;;; FIND-ONLY-ONE-IN-TRIE
;;; PRUNE-TRIE
;;; COLLAPSE-SINGLE-CHILDREN
;;; COLLAPSE-COLLAPSED-NODES
;;;
;;;***************************************************************************

;;;---------------------------------------------------------------------------
;;; TRIE-NODE
;;;---------------------------------------------------------------------------

(defclass TRIE-NODE (TREE-NODE)
  ((count :initform 1
	  :initarg :count
	  :accessor node-count)
   (terminal-p :initform nil
	       :initarg :terminal-p
	       :accessor terminal-node-p)))

;;;---------------------------------------------------------------------------
;;; MAKE-TRIE-NODE
;;;---------------------------------------------------------------------------

(defun MAKE-TRIE-NODE (value depth &optional parent (class 'TRIE-NODE))
  (make-instance class :value value :depth depth :parent parent))

;;;----------------------------------------------------------------------------
;;; ADD-SEQUENCE-TO-TRIE
;;;----------------------------------------------------------------------------

;;; This adds seq to TRIE starting at node.

(defmethod ADD-SEQUENCE-TO-TRIE ((node TRIE-NODE) seq depth
				 &key (class 'TRIE-NODE))
  (%add-sequence-to-trie node (string-upcase seq) depth :class class))

;;;-----------------------------------------------------------------------------

(defmethod %ADD-SEQUENCE-TO-TRIE ((node TRIE-NODE) seq depth
				  &key (class 'TRIE-NODE))
  (cond ((= depth (length seq))
	 (setf (terminal-node-p node) t)
	 node)
	(t
	 (let ((elmt (elt seq depth))
	       (children (node-children node))
	       (child nil))
	   (setf child (find elmt children :test #'char= :key #'node-value))
	   (cond (child
		  (incf (node-count child))
		  (%add-sequence-to-trie child seq (1+ depth) :class class))
		 (t
		  (setf child (make-trie-node elmt depth node class))
		  (add-child child node)
		  (%add-sequence-to-trie child seq (1+ depth) :class class)))))))


;;;-----------------------------------------------------------------------------
;;; FIND-TOKEN-IN-TRIE
;;;-----------------------------------------------------------------------------

(defmethod FIND-TOKEN-IN-TRIE ((token STRING)(node TRIE-NODE)
			       &key (depth 0) (exact-p t))
  (setf token (string-upcase token))
  (%find-token-in-trie token node 
		       :depth depth
		       :exact-p exact-p))

;;;-----------------------------------------------------------------------------

(defun %FIND-TOKEN-IN-TRIE (token node &key (depth 0)(exact-p t))
  (cond ((>= depth (length token))
	 (if exact-p
	     ;; only succeed if terminal-node-p
	     (if (terminal-node-p node) node nil)
	   ;; Succeed if regardless of terminal-node-p
	   node))
	((null (node-children node))
	 nil)
	(t
	 (let* ((next-node (find (elt token depth)(node-children node)
				 :key #'node-value
				 :test #'char=)))
	   (when next-node
	     (%find-token-in-trie token next-node
				  :depth (1+ depth)
				  :exact-p exact-p))))))

;;;-----------------------------------------------------------------------------
;;; FIND-TOKENS-IN-TRIE
;;;-----------------------------------------------------------------------------

(defmethod FIND-TOKENS-IN-TRIE ((token STRING)(node TRIE-NODE))
  (let* ((subtrie (find-token-in-trie token node :exact-p nil))
	 (tokens nil))
    (when (and node subtrie)
      (setf tokens (tokens-in-trie subtrie))
      (setf tokens
	(mapcar #'(lambda (x)
		    (string-upcase
		     (apply #'concatenate 'STRING (cons token (cdr x)))))
		tokens))
      (when (terminal-node-p subtrie)
	(push (string-upcase token) tokens))
      tokens)))

;;;----------------------------------------------------------------------------
;;; TOKENS-IN-TRIE
;;;----------------------------------------------------------------------------

(defun TOKENS-IN-TRIE (node &optional (values))
  (cond ((null (node-children node))
	 `(,(mapcar #'(lambda (x)(format nil "~a" x))
		    (reverse (cons (node-value node) values)))))
	((terminal-node-p node)
	 (append  `(,(mapcar #'(lambda (x)(format nil "~a" x))
			     (reverse (cons (node-value node) values))))
		  (apply #'append
			 (mapcar #'(lambda (child)
				     (tokens-in-trie child (cons (node-value node)
								 values)))
				 (node-children node)))))
	(t
	 (apply #'append
		(mapcar #'(lambda (child)
			    (tokens-in-trie child (cons (node-value node) values)))
			(node-children node))))))

;;;-----------------------------------------------------------------------------

(defun TOKEN-SETS-IN-TRIE (node &optional (depth 0))
  (let* ((nodes (nodes-of-depth node depth)))
    (mapcar #'tokens-in-trie nodes)))
  
;;;---------------------------------------------------------------------------
;;; FIND-TOKEN-IN-TRIES
;;;---------------------------------------------------------------------------

(defmethod FIND-TOKEN-IN-TRIES ((token STRING) &optional (nodes nil))
  (dolist (node nodes)
    (when (find-token-in-trie token node)
      (return node))))

;;;-----------------------------------------------------------------------------
;;; FIND-ONLY-ONE-IN-TRIE
;;;-----------------------------------------------------------------------------

(defmethod FIND-ONLY-ONE-IN-TRIES ((token1 STRING)(token2 STRING)
				   &optional nodes)
  (dolist (node nodes)
    (when (and (find-token-in-trie token1 node)
		(not (find-token-in-trie token2 node)))
      (return t))))

;;;---------------------------------------------------------------------------
;;; FIND-ONLY-ONE-IN-TRIE
;;;---------------------------------------------------------------------------

(defmethod FIND-ONLY-ONE-IN-TRIE ((token1 STRING)(token2 STRING)(node TRIE-NODE))
  (and (find-token-in-trie token1 node)
       (not (find-token-in-trie token2 node))))

;;;---------------------------------------------------------------------------
;;; PRUNE-TRIE
;;;---------------------------------------------------------------------------

#+IGNORE
(defun PRUNE-TRIE (node)
  (cond ((null node)
	 nil)
	((atom node)
	 (cond ((member (node-value node) *known-delimiters*
			:test #'eq)
		(setf (node-children (node-parent node)) nil))
	       (t
		(prune-trie (node-children node)))))
	(t
	 (mapc #'prune-trie node))))

  
;;;---------------------------------------------------------------------------
;;; COLLAPSE-SINGLE-CHILDREN
;;;---------------------------------------------------------------------------

(defvar *collapsed-p* nil)

(defun COLLAPSE-SINGLE-CHILDREN (node)
  (setf *collapsed-p* nil)
  (loop
    (collapse-single-children-1 node)
    (when (null *collapsed-p*)
      (return t))
    (setf *collapsed-p* nil))
  node)
      
;;;----------------------------------------------------------------------------

(defun COLLAPSE-SINGLE-CHILDREN-1 (node)
  (cond ((null (node-children node))
	 t)
	(t
	 (when (= 1 (length (node-children node)))
	   ;; Collapse the child
	   (let ((child (first (node-children node))))
	     (setf (node-value node)
	       (format nil "~a~a" (node-value node)(node-value child)))
	     (dolist (c (node-children node))
	       (setf (node-parent c) node))
	     (setf (node-children node)
	       (node-children child))
	       (setf (node-collapsed-p node) t)
	       (setf *collapsed-p* t)))
	 ;; Process descendants
	 (dolist (child (node-children node))
	   (collapse-single-children-1 child)))))

;;;---------------------------------------------------------------------------
;;; COLLAPSE-COLLAPSED-NODES
;;;---------------------------------------------------------------------------

(defun COLLAPSE-COLLAPSED-NODES (root)
  (dolist (node (node-children root))
    (collapse-collapsed-nodes-1 node)))

;;;---------------------------------------------------------------------------

(defun COLLAPSE-COLLAPSED-NODES-1 (node)
  (cond ((null (node-children node))
	 t)
	(t
	 (when (node-has-collapsed-child node)
	   (dolist (child (node-children node))
	     (let ((new-node nil))
	       (setf new-node
		 (make-instance 'tree-node
		   :value (format nil "~a~a" (node-value node) (node-value child))
		   :parent (node-parent node)
		   :depth (node-depth node) 
		   :collapsed-p t
		   :children (node-children child)))
	       (when (node-parent node)
		 (add-child new-node (node-parent node)))
	       (mapc #'(lambda (x)
			 (setf (node-parent x) new-node))
		     (node-children node))
	       (delete-child child)))
	   (delete-child node))
	 (mapc #'collapse-collapsed-nodes-1 (node-children node)))))

;;;---------------------------------------------------------------------------

(defun NODE-HAS-COLLAPSED-CHILD (node)
  (let ((collapsed-p nil))
    (dolist (child (node-children node))
      (when (node-collapsed-p child)
	(setf collapsed-p t)))
    collapsed-p))

;;;--------------------------------------------------------------------------
;;;
;;;---------------------------------------------------------------------------
