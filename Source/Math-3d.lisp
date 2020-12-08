(in-package :UTIL)

;;;***********************************************************************
;;; 3D Math Functions
;;;***********************************************************************
;;;
;;; File Contents
;;; -------------
;;;
;;; SPHERICAL-TO-CARTESIAN 
;;;
;;; CUBE-POINTS
;;; PRISM-POINTS
;;;
;;; ROTATE-POINTS
;;;
;;; ROTATE-ABOUT-X-AXIS
;;; ROTATE-ABOUT-Y-AXIS
;;; ROTATE-ABOUT-Z-AXIS
;;;
;;; SPHERE-POINT
;;; ROUND-POINT
;;;
;;;***********************************************************************

;;;-----------------------------------------------------------------------
;;;  SPHERICAL-TO-CARTESIAN
;;;-----------------------------------------------------------------------

(defun SPHERICAL-TO-CARTESIAN (ro theta phi)
  (let* (;; Use this complex number instead of sin & cos
	 (theta-euler (cis theta))
	 (phi-euler (cis phi))
	 (sin-theta (imagpart theta-euler))
	 (cos-theta (realpart theta-euler))
	 (sin-phi  (imagpart phi-euler))
	 (cos-phi (realpart phi-euler)))
    (list `(,(* ro sin-phi cos-theta)
	    ,(* ro sin-phi sin-theta)
	    ,(* ro cos-phi)))))

;;;-----------------------------------------------------------------------
;;; CUBE-POINTS
;;;-----------------------------------------------------------------------

(defun CUBE-POINTS (x y z s			
		    &key
		    (x-rotation (- (/ util::pi 4)))
		    (y-rotation (- (/ util::pi 4)))
		    (z-rotation (- (/ util::pi 2)))
		    (pivot nil))
    (let* ((points nil)
	   (half-size (floor s 2))
	   (pivot (or pivot `(,(+ x half-size) ,(+ y half-size) ,(+ z half-size)))))
      (setf points  `((,x ,y ,z)
		      (,(+ x s) ,y ,z)
		      (,(+ x s) ,(+ y s) ,z)
		      (,x ,(+ y s) ,z)
		      (,x ,y ,(+ z s))
		      (,(+ x s) ,y ,(+ z s))
		      (,(+ x s) ,(+ y s) ,(+ z s))
		      (,x ,(+ y s) ,(+ z s))))
      (setf points (rotate-points points `(,x-rotation ,y-rotation ,z-rotation) pivot))
      (setf points (mapcar #'(lambda (point)(mapcar #'round point)) points))
      ;; (mapc #'print points)
      points))

;;;-----------------------------------------------------------------------
;;; PRISM-POINTS
;;;-----------------------------------------------------------------------

(defun PRISM-POINTS (x y z sx sy sz			
		    &key
		    (x-rotation (- (/ util::pi 4)))
		    (y-rotation (- (/ util::pi 4)))
		    (z-rotation (- (/ util::pi 2)))
		    (pivot nil))
    (let* ((points nil)
	   (half-x-size (/ sx 2))
	   (half-y-size (/ sy 2))
	   (half-z-size (/ sz 2))
	   (pivot (or pivot `(,(+ x half-x-size) ,(+ y half-y-size) ,(+ z half-z-size)))))
      (setf points  `((,x ,y ,z)
		      (,(+ x sx) ,y ,z)
		      (,(+ x sx) ,(+ y sy) ,z)
		      (,x ,(+ y sy) ,z)
		      (,x ,y ,(+ z sz))
		      (,(+ x sx) ,y ,(+ z sz))
		      (,(+ x sx) ,(+ y sy) ,(+ z sz))
		      (,x ,(+ y sy) ,(+ z sz))))
      (setf points (rotate-points points `(,x-rotation ,y-rotation ,z-rotation) pivot))
      (setf points (mapcar #'(lambda (point)(mapcar #'round point)) points))
      points))

;;;-----------------------------------------------------------------------
;;; ROTATE-POINTS
;;;-----------------------------------------------------------------------

(defun ROTATE-POINTS (points rotations pivot)
  (destructuring-bind (x-rotation y-rotation z-rotation) rotations
    (setf points (util::rotate-about-x-axis points x-rotation pivot))
    (setf points (util::rotate-about-y-axis points y-rotation pivot))
    (setf points (util::rotate-about-z-axis points z-rotation pivot))
    points))
   
;;;-----------------------------------------------------------------------
;;; ROTATE-ABOUT-X-AXIS
;;;-----------------------------------------------------------------------

(defun ROTATE-ABOUT-X-AXIS (point-list angle pivot &optional sin cos)
  (setf point-list (mapcar #'copy-list point-list))
  (unless (and sin cos)
    (let (;; Use this complex number instead of sin & cos
	  (euler-number (cis angle)))
      ;; The imaginary part is the sin
      (setf sin (imagpart euler-number))
      ;; The real part is the cos
      (setf cos (realpart euler-number))))
   (let* ((y0 (second pivot))
	  (z0 (third pivot)))
     (dolist (point point-list)
      (let* ((y (second point))
	     (z (third point)))
	(setf (second point)
	  (y-rotate-about-x-axis y z y0 z0 sin cos))
	(setf (third point) 
	  (z-rotate-about-x-axis y z y0 z0 sin cos))))
     point-list))
  
;;;-----------------------------------------------------------------------

(defun y-rotate-about-x-axis  (y z y0 z0 sin cos)
   (+ y0 (+ (* cos  (- y y0)) (* sin (- z z0)))))

;;;-----------------------------------------------------------------------

(defun z-rotate-about-x-axis  (y z y0 z0 sin cos)
  (+ z0 (- (* cos (- z z0)) (* sin (- y y0)))))
	     
;;;-----------------------------------------------------------------------
;;; ROTATE-ABOUT-Y-AXIS
;;;-----------------------------------------------------------------------

;;; Assume angle in radians

(defun ROTATE-ABOUT-Y-AXIS (point-list angle pivot &optional sin cos)
  (setf point-list (mapcar #'copy-list point-list))
  (unless (and sin cos)
     (let (;; Use this complex number instead of sin & cos
	   (euler-number (cis angle)))
       ;; The imaginary part is the sin
       (setf sin (imagpart euler-number))
       ;; The real part is the cos
       (setf cos (realpart euler-number))))
  (let* ((x0 (first  pivot))
	 (z0 (third pivot)))
    (dolist (point point-list)
      (let* ((x (first point))
	     (z (third point)))
	(setf (first point) 
	  (x-rotate-about-y-axis x z x0 z0 sin cos))
	(setf (third point)
	  (z-rotate-about-y-axis x z x0 z0 sin cos))))
    point-list))
 
;;;-----------------------------------------------------------------------

(defun x-rotate-about-y-axis  (x z x0 z0 sin cos)
  (+ x0 (- (* cos (- x x0)) (* sin (- z z0)))))

;;;-----------------------------------------------------------------------

(defun z-rotate-about-y-axis  (x z x0 z0 sin cos)
  (+ z0 (+ (* cos (- z z0)) (* sin (- x x0)))))
 
;;;-----------------------------------------------------------------------
;;; ROTATE-ABOUT-Z-AXIS
;;;-----------------------------------------------------------------------

(defun ROTATE-ABOUT-Z-AXIS (point-list angle pivot &optional sin cos)
  (setf point-list (mapcar #'copy-list point-list))
  (unless (and sin cos)
     (let (;; Use this complex number instead of sin & cos
	   (euler-number (cis angle)))
       ;; The imaginary part is the sin
       (setf sin (imagpart euler-number))
       ;; The real part is the cos
       (setf cos (realpart euler-number))))
  (let* ((x0 (first  pivot))
	 (y0 (second pivot)))
    (dolist (point point-list)
      (let* ((x (first point))
	     (y (second point)))
	(setf (first point)
	  (x-rotate-about-z-axis x y x0 y0 sin cos))
	(setf (second point)
	  (y-rotate-about-z-axis x y x0 y0 sin cos))))
    point-list))
	
;;;-----------------------------------------------------------------------

(defun x-rotate-about-z-axis  (x y x0 y0 sin cos)
   (+ x0 (+ (* cos (- x x0)) (* sin (- y y0)))))

;;;-----------------------------------------------------------------------

(defun y-rotate-about-z-axis  (x y x0 y0 sin cos)
  (+ y0 (- (* cos (- y y0)) (* sin (- x x0)))))


;;;------------------------------------------------------------------------
;;; SPHERE-POINT
;;;------------------------------------------------------------------------

(defun SPHERE-POINT (radius center phi theta)
  (destructuring-bind (x0 y0 z0) center
    `(,(+ x0
	  (* radius 
	     (cos (util::degrees-to-radians theta))
	     (cos (util::degrees-to-radians phi))))
      ,(+ y0
	  (* radius 
	     (sin (util::degrees-to-radians theta))
	     (cos (util::degrees-to-radians phi))))
      ,(+ z0
	  (* radius (sin (util::degrees-to-radians phi)))))))

;;;------------------------------------------------------------------------
;;; ROUND-POINT
;;;------------------------------------------------------------------------

(defun ROUND-POINT (point)
  (mapcar #'round point))

(defun round-points (points)
  (mapcar #'round-point points))

;;;-----------------------------------------------------------------------
;;; POINTS-ON-SPHERE
;;;------------------------------------------------------------------------;;;

;;; NOTE: This function probaby does not do what it claims :-)

;;; This function returns a list of n (x y) pairs that are uniformely distributed
;;; around a circle with center 'center' and diameter 'diameter' starting at
;;; angle start-angle.

;; (defmethod POINTS-ON-SPHERE ((center LIST)(ro NUMBER)(slice (eql :X)))
;;   (destructuring-bind (x-center y-center z-center) center 
;;   (let* ((angle-inc (/ 2pi n))
;; 	 (angle1 0 #+IGNORE start-angle)
;; 	 (points nil))

;;     ;; Move around the circle in increments of 'anc-inc'
;;     (dotimes (i n)
;;       (let* (;; Use this complex number instead of sin & cos
;; 	     (euler-number (cis angle1))
;; 	     ;; The imaginary part is the sin
;; 	     (sin-mid-angle (imagpart euler-number))
;; 	     ;; The real part is the cos
;; 	     (cos-mid-angle (realpart euler-number))
;; 	     ;; Tentative x-position of the label (one-pixel away)
;; 	     (point-x (+ x-center (round (* (+ 1 radius) cos-mid-angle))))
;; 	     ;; Tentative y-position of the label (one-pixel away)
;; 	     (point-y (+ y-center (round (* (+ 1 radius) sin-mid-angle)))))
;; 	(push `(,point-x ,point-y) points)
;; 	(incf angle1 angle-inc)))

;;     ;; Return the points
;;    (nreverse points))))


;;; -------------------------------------------------------------------------
;;; End of File
;;; -------------------------------------------------------------------------
