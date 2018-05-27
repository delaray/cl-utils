(in-package :UTILITIES)

(export '(SAFE-WORD-TO-SYMBOL WORD-TO-SYMBOL))

;;;-----------------------------------------------------------------------------
;;; SAFE-READ-FROM-STRING
;;;-----------------------------------------------------------------------------

;;; NOTE: <word> is potentially evaluated twice.

(defmacro SAFE-READ-FROM-STRING (word)
  (let ((tag (gensym)))
    `(block ,tag
       (handler-bind ((error #'(lambda (x)
					(declare (ignore x))
					(return-from ,tag ,word))))
	 (read-from-string ,word)))))

;;;----------------------------------------------------------------------------
;;; WORD-TO-SYMBOL
;;;----------------------------------------------------------------------------

;;; This version returns 2 values. The first value can be a symbol, number or
;;; the an explicitly interned symbol, if read-from-string to signaled an error.
;;; The second value is t or nil indicating whether or not a symbol was returned.

(defun WORD-TO-SYMBOL (word &optional (package :UTIL))
  "Return a symbol or a number. Replace spaces with dashes."
  (cond ((symbolp word)
	 word)
	(t 
	 (let* ((new-word (hyphenate-tokens word))
		(result (safe-read-from-string new-word)))
	   (cond ((symbolp result)
		  (values result t))
		 ((numberp result)
		  (values result nil))
		 (t
		  (ignore-errors (values (intern result package) t))))))))

;;;-----------------------------------------------------------------------------
;;; SAFE-WORD-TO-SYMBOL
;;;-----------------------------------------------------------------------------

(defmacro SAFE-WORD-TO-SYMBOL (string-var-name)
  `(if (same-value-p ,string-var-name *null-string*)
     (cerror "Continue anyway." "Null string is not a valid ~a."
             ',string-var-name)
     (word-to-symbol ,string-var-name)))

;;;-----------------------------------------------------------------------------
;;; BETWEEN
;;;-----------------------------------------------------------------------------

(defmacro BETWEEN (a b c)
  (let ((value (gensym)))
    `(let ((,value ,b))
       (and (<= ,a ,value)(<= ,value ,c)))))
	    
;;;-----------------------------------------------------------------------------
;;; WITH-SAFE-OPEN-FILE
;;;-----------------------------------------------------------------------------

(defmacro WITH-SAFE-OPEN-FILE ((file pathname &rest open-file-args) &body body)
  `(cond ((probe-file ,pathname)
	  (with-open-file (,file ,pathname ,@open-file-args)
	    ,@body))
	 (t
	  (format t "~%")
	  (warn "File ~a does not exist." (pathname-name ,pathname)))))

;;;-----------------------------------------------------------------------------
;;; WITH-RESULTS-PATHNAME
;;;-----------------------------------------------------------------------------

(defmacro WITH-RESULTS-PATHNAME ((filename &optional (source nil)) &body body)
  `(let* ((%%filename%% (if (pathnamep ,filename) ,filename (make-data-pathname ,filename)))
	  (directory (pathname-directory %%filename%%)))
     (if ,source
	 (setf directory (make-pathname :directory `(,@directory ,source)))
       (setf directory (make-pathname :directory directory)))
     (unless (probe-file directory)
       (make-directory directory))
     (setf %%filename%%
       (make-pathname :directory (pathname-directory directory)
		      :type (pathname-type %%filename%%)
		      :name (pathname-name %%filename%%)))
     (with-open-file (file %%filename%%
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :supersede
		      :external-format :latin1)
       (let* ((*standard-output* file))
	 ,@body))))

#+IGNORE
(defmacro WITH-RESULTS-PATHNAME (filename &body body)
  `(let* ((%%filename%% (make-results-pathname ,filename)))
     (with-open-file (file %%filename%%
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :supersede)
       (let* ((*standard-output* file))
	 ,@body))))


;;;-----------------------------------------------------------------------------
;;; POP-N-PUSH
;;;-----------------------------------------------------------------------------

;;; Does (push (pop from) to) without generating new conses.
(defmacro POP-N-PUSH (from to)
  (let ((top (gensym)))
   `(let ((,top ,from))
      (setf ,from (rest ,from))
      (setf (rest ,top) ,to)
      (setf ,to ,top) )))

;;;; --------------------------------------------------------------------------
;;;;   Functions used in the following macros
;;;; --------------------------------------------------------------------------

;;; Sometimes it's nice to have your gensyms mean something when
;;; you're reading the macroexpansion of some form.  The problem
;;; is that if you give a prefix to GENSYM it remains the prefix
;;; until you change it.  


(defvar *newsym-counter* 0)

(defun NEWSYM (&optional (prefix "X"))
  (unless (stringp prefix)
    (setf prefix (string prefix)))
  (make-symbol (format nil "~:@(~a~)-~3,'0d" prefix (incf *newsym-counter*))))

;;;-----------------------------------------------------------------------------
;;; DOLIST-BY-TWOS
;;;-----------------------------------------------------------------------------

(defmacro DOLIST-BY-TWOS ((v1 v2 list &optional result) &body body)
  (let ((remaining-list (gensym)))
    `(do* ((,remaining-list ,list (cddr ,remaining-list))
	   (,v1 (first ,remaining-list) (first ,remaining-list))
	   (,v2 (second ,remaining-list) (second ,remaining-list)))
	  ((null ,remaining-list)
           ;; Reference these two variables here to suppress
           ;; possible compiler warnings if one is not used.
           ,v1 ,v2
           ,result)
       ,@body)))


;;;-----------------------------------------------------------------------------
;;; DOLIST-OR-ATOM:
;;;-----------------------------------------------------------------------------

;;; DOLIST-OR-ATOM:
;;;
;;;   (dolist-or-atom (var maybe-list)
;;;      <body>)

(defmacro DOLIST-OR-ATOM ((var possible-list &optional test-form) &body body)
  ;; This could be coded as follows but if body is large the
  ;; space penalty is significant.
  ;; 
  ;; (if ,test-form
  ;;     (dolist (,var ,possilble-list) ,@body)
  ;;     (let ((,var ,possible-list)) ,@body))

  (let ((test-result (newsym "TEST"))
	(remaining (newsym "REST")))
    (when (null test-form)
      (setf test-form `(listp ,possible-list)))
    `(do* ((,test-result ,test-form)
	   (,remaining ,possible-list (cdr ,remaining))
	   (,var (if ,test-result (first ,remaining) ,remaining)
	         (first ,remaining)))
	  ((when ,test-result
	     (endp ,remaining)))
       ,@body
       (unless ,test-result (return nil)))))

;;;-------------------------------------------------------------------
;;; DIVF
;;;-------------------------------------------------------------------


(defmacro DIVF (numerator &optional denominator)
  "divf is like incf.  If denominator, it sets the place designated by
   numerator to numerator/denominator. If no denominator, sets
   numerator to its reciprocal."
  `(setf ,numerator (/ ,numerator ,@(when denominator (list denominator)))))

;;;-----------------------------------------------------------------------------
;;; End of File
;;;-----------------------------------------------------------------------------
