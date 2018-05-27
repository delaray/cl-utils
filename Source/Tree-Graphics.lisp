(in-package :GOM)

(use-package :utilities)


;;;**************************************************************************
;;; TREE GRAPHICS
;;;**************************************************************************
;;;
;;; Classes
;;; -------
;;;
;;; GRAPHICAL-NODE
;;; TREE-WINDOW
;;;
;;;
;;; Functions
;;; ---------
;;;
;;; MAKE-TREE-WINDOW
;;; INITIALIZE-TREE-WINDOW
;;;
;;; HANDLE-RESHAPE-WIDGE
;;; HANDLE-REFRESH-WIDGET
;;;
;;; DRAW-TREE
;;; DRAW-TREE-NODES
;;; DRAW-TREE-EDGES
;;;
;;;**************************************************************************

;;;--------------------------------------------------------------------------
;;; GRAPHICAL-NODE
;;;--------------------------------------------------------------------------

(defclass GRAPHICAL-NODE ()
  ((node :initarg :node
	  :initform nil
	  :accessor object-node)
   (position :initarg :position
	     :initform nil
	     :accessor object-position)))

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj GRAPHICAL-NODE) (stream t))
  (cond (*print-readably*
	 (call-next-method))
	(t
	 (format stream "#<GNODE: ~a, Depth: ~d>"
		 (object-name (object-node obj))
		 (util::node-depth (object-node obj))))))

;;;--------------------------------------------------------------------------
;;; TREE-WINDOW
;;;--------------------------------------------------------------------------

(defclass TREE-WINDOW (GOM-WINDOW)
  ((tree :initarg :graph
	  :initform nil
	  :accessor object-tree)
   (orientation :initarg :orientation
		:initform :horizontal
		:accessor object-orientation)
   (nodes :initarg nodes
	  :initform nil
	  :accessor object-nodes)))

;;;--------------------------------------------------------------------------
;;; MAKE-TREE-WINDOW
;;;--------------------------------------------------------------------------

(defmethod MAKE-TREE-WINDOW ((root util::TREE-NODE))
  (let* ((window (make-window 'Tree :class 'tree-window)))
    (setf (object-tree window) root)
    (initialize-tree-window window)
    window))
      
;;;--------------------------------------------------------------------------
;;; INITIALIZE-TREE-WINDOW
;;;--------------------------------------------------------------------------

;;; TODO: Need to modify initialize-graph-window to update graphical
;;; vertex positioms rather than regenerate the graphical vertices for
;;; efficiency.

(defmethod INITIALIZE-TREE-WINDOW ((window TREE-WINDOW))
  (when (object-tree window)
    (let* ((orientation (object-orientation window))
	   (root (object-tree window))
	   (width (widget-inner-width window))
	   (height (widget-inner-height window))
	   (depth (util::max-tree-depth root))
	   (h-inc (floor width (1+ depth)))
	   (v-inc (floor height (1+ depth)))
	   (h-margin (floor h-inc 4))
	   (v-margin (floor v-inc 4))
	   (mid-x (floor width 2))
	   (mid-y (floor height 2))
	   (g-root (make-instance 'graphical-node 
		     :node root
		     :position (if (eq orientation :horizontal)
				   (list h-margin mid-y)
				 (list mid-x v-margin)))))
      (setf (object-nodes window) nil)
      (setf (object-nodes window)
	(nreverse
	 (cons g-root
	       (if (eq orientation :horizontal)
		   (%initialize-tree-window-h g-root 1
					      (+ h-margin h-inc) h-inc
					      v-margin (- height v-margin)
					      nil)
		 (%initialize-tree-window-v g-root 1
					    h-margin (- width h-margin)
					    (+ v-margin v-inc) v-inc
					    nil)))))
      (refresh-widget window))))

;;;--------------------------------------------------------------------------

(defun %INITIALIZE-TREE-WINDOW-H (pg-node depth x x-inc min-y max-y result)
  (let* ((children (util::object-children (object-node pg-node)))
	 (v-inc (floor (- max-y min-y) (length children)))
	 (g-node nil)
	 (v-count 1)
	 (result* result))
    (dolist (node children)
      (let* ((y (- (+ min-y (* v-count v-inc))(floor v-inc 2)))
	     (y1  (- y (floor v-inc 2)))
	     (y2  (+ y (floor v-inc 2))))
	(incf v-count)
	(setf g-node
	  (make-instance 'graphical-node :node node :position (list x y)))
	(push g-node result*)
	(when (util::object-children node)
	  (setf result*
	    (%initialize-tree-window-h g-node (1+ depth) (+ x x-inc) x-inc
				       y1 y2
				       result*)))))
    result*))

;;;--------------------------------------------------------------------------

(defun %INITIALIZE-TREE-WINDOW-V (pg-node depth min-x max-x y y-inc result)
  (let* ((children (util::object-children (object-node pg-node)))
	 (h-inc (floor (- max-x min-x) (length children)))
	 (g-node nil)
	 (h-count 1)
	 (result* result))
    (dolist (node children)
      (let* ((x (- (+ min-x (* h-count h-inc))(floor h-inc 2)))
	     (x1  (- x (floor h-inc 2)))
	     (x2  (+ x (floor h-inc 2))))
	(incf h-count)
	(setf g-node
	  (make-instance 'graphical-node :node node :position (list x y)))
	(push g-node result*)
	(when (util::object-children node)
	  (setf result*
	    (%initialize-tree-window-v g-node (1+ depth)
				       x1 x2
				       (+ y y-inc) y-inc
				       result*)))))
    result*))

;;;--------------------------------------------------------------------------
;;; HANDLE-RESHAPE-WIDGET
;;;--------------------------------------------------------------------------

;;; TODO: Need to modify initialize-graph-window to update graphical
;;; vertex positioms rather than regenerate the graphical vertices for
;;; efficiency.

(defmethod HANDLE-RESHAPE-WIDGET :after ((window TREE-WINDOW))
  (initialize-tree-window window))

;;;--------------------------------------------------------------------------
;;; HANDLE-REFRESH-WIDGET
;;;--------------------------------------------------------------------------

(defmethod HANDLE-REFRESH-WIDGET :after ((window TREE-WINDOW))
   (clear-widget window)
   (draw-tree window))

;;;--------------------------------------------------------------------------
;;; HANDLE-WIDGET-EVENT
;;;--------------------------------------------------------------------------

 (defmethod HANDLE-WIDGET-EVENT :after ((window TREE-WINDOW) event-type event-data)
  (clear-widget window)
  (let ((mouse-button (event-data-value event-data)))
    (when (and (eq event-type  :mouse-up)
	       (eq mouse-button :mouse-left))
      (if (eq (object-orientation window) :horizontal)
	  (setf (object-orientation window) :vertical)
	(setf (object-orientation window) :horizontal))
      (initialize-tree-window window))))
 
;;;--------------------------------------------------------------------------
;;; DRAW-GRAPH
;;;--------------------------------------------------------------------------

(defmethod DRAW-TREE ((window TREE-WINDOW))
   ;; Draw the edges
  (dolist (gnode (object-nodes window))
    (let* ((gnode1 (find (object-node gnode) (object-nodes window)
			 :test #'eq :key #'object-node))
	   (p1 (and gnode1 (object-position gnode1))))
      ;;(format t "~%Gnode1 = ~a" gnode1)
      (when gnode1
	(dolist (child (util::object-children (object-node gnode)))
	  (let* ((gnode2 (find child (object-nodes window)
			       :test #'eq :key #'object-node))
		 (p2 (and gnode2 (object-position gnode2))))
	    ;;(format t "~%Gnode2 = ~a" gnode2)
	    (when gnode2
	      (draw-gom-line window
			     (first p1)(second p1)
			     (first p2)(second p2))))))))
  ;; Draw the nodes
  (draw-tree-nodes window))

;;;--------------------------------------------------------------------------

(defmethod DRAW-TREE-NODES ((window TREE-WINDOW))
  (dolist (node (object-nodes window))
    (let* ((point (object-position node))
	   (name (object-name (object-node node)))
	   (name-width (string-width window name))
	   (name-height (string-height window name))
	   (name-x (round (- (first point)(/ name-width 2))))
	   (name-y (round (- (second point)(/ name-height 2)))))
      (cond ((<= (length name) 2)
	     ;; Clear the space occupied by the edge tips
	     (draw-gom-circle window (first point) (second point) 15
			      :color (widget-background-color window)
			      :fill t)
	     (draw-gom-circle window (first point)(second point) 15
			      :color cg::blue))
	    (t
	     (draw-gom-rectangle window (- name-x 2) (- name-y 2)
				 (+ name-width 4)(+ name-height 4)
				 :color (widget-background-color window)
				 :fill (widget-background-color window))
	     (draw-gom-rectangle window (- name-x 2) (- name-y 2)
				 (+ name-width 4)(+ name-height 4)
				 :color cg::blue :fill nil)))	   
      ;; Draw the vertex name inside the circle. Note: We my wish to
      ;; draw a rectangle for long names
      (draw-gom-string  window name name-x name-y))))

;;;--------------------------------------------------------------------------
;;; End of File
;;;--------------------------------------------------------------------------