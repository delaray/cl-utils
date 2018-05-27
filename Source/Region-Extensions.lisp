;;;; -*- Mode:Common-Lisp; Package:CHALKBOX; Syntax:common-lisp; Base:10 -*-
;;;; *-* File: lapis: /u7/gbb/v-400/internal/source/chalkbox/chalkbox-extensions/region-extensions.lisp *-*
;;;; *-* Edited-By: Cork *-*
;;;; *-* Last-Edit: Saturday, January 3, 1998  19:57:03 *-*
;;;; *-* Machine: GRANITE (Explorer II, Microcode 489) *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                       CHALKBOX EXTENSIONS
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Raymond de Lacaze
;;;
;;; Copyright (c) 1998-1999
;;; Knowledge Technologies International
;;; All rights reserved
;;;
;;; Copyright (c) 1995-1998
;;; Blackboard Technology Group, Inc., Amherst, MA 01002
;;;
;;; The software, data, and information contained herein are proprietary
;;; to, and contain valuable trade secrets of Knowledge Technologies
;;; International (KTI).  They are given in confidence by KTI pursuant
;;; to a written agreement, and may be used, copied, transmitted, and
;;; stored only in accordance with the terms of such license.
;;;
;;; Restricted Rights Legend
;;; ------------------------
;;; Use, duplication, and disclosure of the software, data and information
;;; contained herein by any agency, department or entity of the U.S.
;;; Government are subject to restrictions of Restricted Rights for
;;; Commercial Software developed at private expense as specified in FAR
;;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;;; applicable.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  09-01-95 File Created (de Lacaze)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(in-package "CHALKBOX")

(proclaim `(optimize (speed  ,*chalkbox-optimize-speed*)
                     (safety ,*chalkbox-optimize-safety*)))

(insert-chalkbox-legal-notice)

;;;************************************************************************************
;;;
;;; SYNOPSIS OF FILE CONTENTS:
;;; -------------------------
;;;
;;; Part 1:  INTERACTIVE DRAW FUNCTIONS
;;; Part 2:  DECOMPOSE-INTERSECTING REGIONS 
;;;
;;;************************************************************************************

;;;************************************************************************************
;;; PART 1: Region Predicates & Drawing functions
;;;************************************************************************************

;;; ----------------------------------------------------------------------;;;
;;; REGION PREDICATES
;;; ----------------------------------------------------------------------;;;

(defun REGION-STRICTLY-CONTAINS-POINT-P (x y width height point-x point-y)
  (and (<& x point-x (+& x width -1))
       (<& y point-y (+& y height -1))))

;;;----------------------------------------------------------------------------

(defun REGION-STRICTLY-CONTAINS-REGION-P (x1 y1 w1 h1 x2 y2 w2 h2)
  (and (<& x1 x2)
       (<& y1 y2)
       (<& (+& x2 w2) (+& x1 w1))
       (<& (+& y2 h2) (+& y1 h1))))

;;;----------------------------------------------------------------------------

(defun REGIONS-STRICTLY-INTERSECT-P (x1 y1 w1 h1 x2 y2 w2 h2)
  (let* ((x (+& x1 w1))
	 (y (+& y1 h1)))
    (region-strictly-contains-point-p x2 y2 (+& w1 w2) (+& h1 h2) x y)))

;;; -------------------------------------------------------------------------

;;; This returns a subset of regions that intersect with region

(defun REGIONS-INTERSECTING-REGION (region regions)
  (let* ((result nil))
    (dolist (r regions)
      (when (regions-intersect-p
	     (first r)(second r)(third r)(fourth r)
	     (first region)(second region)(third region)(fourth region))
	(push r result)))
    ;; preserve the ordering
    (nreverse result)))

;;; -------------------------------------------------------------------------

;;; This returns a subset of regions that intersect with region

(defun REGIONS-STRICTLY-INTERSECTING-REGION (region regions)
  (let* ((result nil))
    (dolist (r regions)
      (when (regions-strictly-intersect-p
	     (first r)(second r)(third r)(fourth r)
	     (first region)(second region)(third region)(fourth region))
	(push r result)))
    ;; preserve the ordering
    (nreverse result)))

;;;--------------------------------------------------------------------;;;
;;; DRAW-REGION
;;;--------------------------------------------------------------------;;;

;;; These are defined for convenience, might want to add accessor keyword
;;; arguments to access the components of a region.

;;; Regions is just a list of regions i.e. ((x1 y1 w1 h1) (x2 y2 w2 h2) ....)

(defmethod DRAW-REGIONS ((widget t) regions &rest rest)
  (with-widget-inner-region (widget)
     (mapc #'(lambda (region)(apply #'draw-region widget region rest)) regions)))

;;;--------------------------------------------------------------------;;;

(defmethod DRAW-REGION ((widget t) region &key
			(foreground-color "Black")
			(fill-color nil)
			(thickness 1))
  (draw-rectangle widget (first region)(second region)(third region)(fourth region)
		  :foreground-color foreground-color
		  :fill-color fill-color
		  :thickness thickness)) 

;;;************************************************************************************
;;; PART 2: Decompose Intersecting Regions
;;;************************************************************************************

;;;--------------------------------------------------------------------;;;
;;; DECOMCOMPOSE INTERSECTING REGIONS
;;;--------------------------------------------------------------------;;;

;;;  We assume boundaries is an ordered list representing the occlusion order.
;;;  The algorithm we use is described below in %decompose-intersecting-regions

(defun DECOMPOSE-ALL-INTERSECTING-REGIONS (regions &optional processed)
  (cond ((null regions)
	 processed)
	(t
	 ;; One or more regions left to process
	 (let* ((r1 (first regions))
		(r2 (first (regions-strictly-intersecting-region r1 processed))))
	   (cond ((null r2)
		  ;; R1 doesn't intersect anything above it
		  (decompose-all-intersecting-regions (cdr regions) `(,@processed ,r1)))
		 (t
		  ;; There is an above overlapping region, so partition region
		  ;; with respect to this overlap.
		  (let* ((rs (decompose-intersecting-regions r2 r1)))
		    (mapcar #'(lambda (r)(setf (cdr (last r))(last r1))) rs)
		    (decompose-all-intersecting-regions
		     (append rs (cdr regions)) processed))))))))
	   
;;;--------------------------------------------------------------------;;;

;;; This function breaks up r2 into non-overlapping sub-regions,
;;; excluding pieces the piece intersects with r1.

(defun DECOMPOSE-INTERSECTING-REGIONS (r1 r2)
  (let* ((x1 (first r1))
	 (y1 (second r1))
	 (w1 (third r1))
	 (h1 (fourth r1))
	 (x2 (first r2))
	 (y2 (second r2))
	 (w2 (third r2))
	 (h2 (fourth r2)))
    (when (and x1 y1 w1 h1 x2 y2 w2 h2)
      (cond ((region-contains-region-p x1 y1 w1 h1 x2 y2 w2 h2)
	     nil)
	    (t
	     (%decompose-intersecting-regions x1 y1 w1 h1 x2 y2 w2 h2))))))

;;;--------------------------------------------------------------------;;;

;;; Region2 will be decomposed into 0 or at most 4 sub-regions.
;;; The idea behind the algorithm below is quite simple. First imagine
;;; that r2 is completely occluded by r1, then nil will be returned, since
;;; there is no viable piece of r2. Now if r2 extends northward past r1
;;; then there will be a top piece, if r2 extends westward past r1 then
;;; there will be a left piece, etc.. These cases can be viewed as mutually
;;; exclusive. If r2 completely contains r1 then 4 pieces will be returned
;;; the diagram below shows the agreed upon decomposition so that the pieces
;;; do no not overlap.

;;; Worst case scenario: (4 pieces)

;;;                                                 r2
;;;       +--------+------------------------------+
;;;       |        |                              |
;;;       |        |                              |
;;;       |        |                              |
;;;       |        |                              |
;;;       |        +---------------------+--------+
;;;       |        |                     |        |
;;;       |        |                     |        |
;;;       |        |                     |        |
;;;       |        |                     |        |
;;;       |        |          r1         |        |
;;;       |        |                     |        |
;;;       |        |                     |        |
;;;       |        |                     |        |
;;;       |        |                     |        |
;;;       |        +---------------------+--------+
;;;       |        |                              |
;;;       |        |                              |
;;;       |        |                              |
;;;       |        |                              |
;;;       +--------+------------------------------+


(defun %decompose-intersecting-regions (x1 y1 w1 h1 x2 y2 w2 h2)
  (multiple-value-bind (x3 y3 w3 h3)
	   (intersect-regions x1 y1 w1 h1 x2 y2 w2 h2)
    (cond ((null-region-p x3 y3 w3 h3)
	   `((,x2 ,y2 ,w2 ,h2)))
	  (t
	   ;; The regions intersect
	   (let* ((sub-regions nil))
	     ;; Left piece
	     (when (< x2 x1)
	       (push `(,x2 ,y2 ,(- x1 x2) ,h2) sub-regions))
	     ;; Top Piece
	     (when (< y2 y1)
	       (push `(,x3 ,y2 ,(- w2 (- x3 x2)) ,(- y3 y2)) sub-regions))
	     ;; Bottom piece
	     (when (> (+ y2 h2)(+ y1 h1))
	       (push `(,x3 ,(+ y1 h1) ,(- w2 (- x3 x2)) ,(- (+ y2 h2)(+ y1 h1))) sub-regions))
	     ;; Right piece
	     (when (> (+ x2 w2) (+ x1 w1))
	       (push `(,(+ x1 w1) ,y3 ,(- (+ x2 w2)(+ x1 w1)) ,h3) sub-regions))
	     ;; Return the regions
	     sub-regions)))))

;;; -------------------------------------------------------------------------
;;; Almost End of File
;;; -------------------------------------------------------------------------
