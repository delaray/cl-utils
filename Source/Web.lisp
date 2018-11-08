;;; Quick code to do a Google search on a query & return the results.
;;;
;;; Relies on aserve (or portable-allegroserve).

(in-package :util)

#-SBCL
(eval-when (compile load eval)
  (require :aserve))

;;;*****************************************************************************
;;; Web Utilities
;;;*****************************************************************************
;;;
;;; *HTML-PREFIX*
;;;
;;; DOCUMENT
;;; HTML-DOCUMENT
;;;
;;; GET-URL-PAGE
;;; GOOGLE-SEARCH
;;;
;;; FIND-URLS-IN-PARAGRAPHS
;;;
;;; FIND-HREFS-IN-DOC
;;; FIND-HREFS-IN-HTML
;;; EXTRACT-URL-FROM-HREF

;;;-----------------------------------------------------------------------------
;;; *HTML-PREFIX*
;;;-----------------------------------------------------------------------------

(defvar *HTML-PREFIX* "href=")

;;;-----------------------------------------------------------------------------
;;; *MERRIAM-WEBSTER-PREFIX*
;;;-----------------------------------------------------------------------------

(defvar *MERRIAM-WEBSTER-PREFIX* "http://www.merriam-webster.com/dictionary/")

;;;-----------------------------------------------------------------------------
;;; *WORD-WEB-PREFIX*
;;;-----------------------------------------------------------------------------

(defvar *WORD-WEB-PREFIX* "http://www.wordwebonline.com/search.pl?w=")


(defparameter *IMAGE-SEARCH-STRING*
    "http://www.google.com/search?q=nature&as_filetype=jpg&num=100")

;;;-----------------------------------------------------------------------------
;;; DOCUMENT
;;;-----------------------------------------------------------------------------

(defclass DOCUMENT (VALUE)
  ((name :accessor document-name)
   (value :initarg :content
	  :accessor document-content
	  :accessor object-content)
   ;; This is a hash table with counts of the words in <text>.
   (vocabulary :initform (make-hash-table :size 20 :test #'equalp)
	       :accessor document-vocabulary)))

;;;--------------------------------------------------------------------;;;

(defmethod PRINT-OBJECT ((obj DOCUMENT) stream)
 (cond (*print-readably*
	 (call-next-method))
       (t
	(format stream "#<DOCUMENT: ~a>" (document-name obj)))))

;;;-----------------------------------------------------------------------------
;;; SEQUENTIAL-DOCUMENT
;;;-----------------------------------------------------------------------------

(defclass SEQUENTIAL-DOCUMENT (DOCUMENT)
  ((paragraphs :initarg :paragraphs
	       :initform (make-hash-table :size 20 :test #'eq)
	       :accessor document-paragraphs)))

;;;---------------------------------------------------------------------------

(defmethod FIND-DOCUMENT-PARAGRAPH ((document SEQUENTIAL-DOCUMENT) (n NUMBER))
  (gethash n (document-paragraphs document)))

;;;---------------------------------------------------------------------------

(defmethod MAP-DOCUMENT-PARAGRAPHS ((document SEQUENTIAL-DOCUMENT) function)
  (maphash #'(lambda (count paragraph)
	       (declare (ignore count))
	       (funcall function paragraph))
	   (document-paragraphs document)))

;;;---------------------------------------------------------------------------

(defmethod COUNT-DOCUMENT-PARAGRAPHS ((document SEQUENTIAL-DOCUMENT))
  (hash-table-count (document-paragraphs document)))

;;;---------------------------------------------------------------------------
;;; HTML-LINK-MIXIN
;;;---------------------------------------------------------------------------

(defclass HTML-LINK-MIXIN ()
  (;; These are internal links within the same wesite.
   (internal :initarg :internal
	     :initform nil
	     :accessor internal-links)
   ;; These are external to other websites
   (external :initarg :external
	     :initform nil
	     :accessor external-links)
   ;; These are links within the same page.
   (intranal :initarg :intranal
	     :initform nil
	     :accessor intranal-links)))

;;;-----------------------------------------------------------------------------
;;; HTML-DOCUMENT
;;;-----------------------------------------------------------------------------

(defclass HTML-DOCUMENT (HTML-LINK-MIXIN SEQUENTIAL-DOCUMENT)
  ((address :initarg :address
	    :initform nil
	    :accessor document-address)
   (links   :initform (make-hash-table :test #'equal :size 20)
	    :initarg :links
	    :accessor document-links)))
  
;;;--------------------------------------------------------------------;;;

(defmethod PRINT-OBJECT ((obj HTML-DOCUMENT) stream)
 (cond (*print-readably*
	 (call-next-method))
       (t
	(format stream "#<HTML-DOC: ~a>" (document-name obj)))))

;;;-----------------------------------------------------------------------------

(defmethod DOCUMENT-CONTENT ((obj HTML-DOCUMENT))
  (let ((html-string-object (call-next-method)))
    (when html-string-object
      (object-value html-string-object))))

;;;-----------------------------------------------------------------------------

(defmethod INITIALIZE-INSTANCE :after ((obj HTML-DOCUMENT) &rest rest)
  (make-html-document-paragraphs obj)
  (setf (document-links obj) (compute-document-links obj)))

;;;-----------------------------------------------------------------------------
;;; HTML-STRING
;;;-----------------------------------------------------------------------------

(defclass HTML-STRING (VALUE)
  ((name :accessor html-string-name)
   (value :accessor html-string-value)))
  
;;;--------------------------------------------------------------------;;;

(defmethod PRINT-OBJECT ((obj HTML-STRING) stream)
 (cond (*print-readably*
	 (call-next-method))
       (t
	(format stream "#<HTML-STRING: ~a>" (html-string-name obj)))))

;;;-----------------------------------------------------------------------------
;;; PARAGRAPH
;;;-----------------------------------------------------------------------------

(defclass PARAGRAPH (VALUE)
  ((name :accessor paragraph-name)
   (value :initarg :content
	  :accessor paragraph-content
	  :accessor paragraph-text)
   (number :initarg :number
	   :initform 1
	   :accessor paragraph-number)
   ;; This is a hsh table with counts of the words in <text>.
   (vocabulary :initform (make-hash-table :size 20 :test #'equalp)
	       :accessor paragraph-vocabulary)
     ;; This is the BP to the document object.
   (document :initarg :document
	     :initform nil
	     :accessor paragraph-document)))

;;;--------------------------------------------------------------------------

(defmethod PRINT-OBJECT ((obj PARAGRAPH) stream)
 (cond (*print-readably*
	 (call-next-method))
       (t
	(format stream "#<PARAGRAPH: ~a>" (paragraph-name obj)))))

;;;---------------------------------------------------------------------------

(defmethod INITIALIZE-INSTANCE :after ((obj PARAGRAPH) &rest initargs)
  (unless (paragraph-name obj)
    (when (paragraph-document obj)
      (setf (paragraph-name obj) 
	(format nil "~a-~a"
		(document-name (paragraph-document obj))
		(paragraph-number obj))))))

;;;---------------------------------------------------------------------------
;;; HTML-PARAGRAPH
;;;---------------------------------------------------------------------------

(defclass HTML-PARAGRAPH (HTML-LINK-MIXIN PARAGRAPH)
  (;; This is the content with the htlp tags stripped out. Note the
   ;; accessor overides the accessor from the inhereited class we
   ;; distinguish <content> and <text> in html documents, the former
   ;; having emedded html tags.
   (ptext :initarg :text
	  :initform nil
	  :accessor paragraph-text)
   ;; These are urls
   (links :initarg :links
	  :initform nil
	  :accessor paragraph-links)))


;;;-----------------------------------------------------------------------------
;;; GET-URL-PAGE
;;;-----------------------------------------------------------------------------

#+ALLEGRO
(defun GET-URL-PAGE (url)
  "Simply do an http-request"
  (ignore-errors 
   (multiple-value-bind (body code headers uril)
       (net.aserve.client:do-http-request url)
     (cond ((= code 404)
	    ;;(format t "~%Page not found: ~a" url)
	    nil)
	   (t
	    body)))))

#-ALLEGRO
(defun GET-URL-PAGE (url)
  "Simply do an http-request"
  (ignore-errors 
   (multiple-value-bind (x1 x2 x3 x4 body)
       (drakma:http-request url)
     (cond ((= code 404)
	    ;;(format t "~%Page not found: ~a" url)
	    nil)
	   (t
	    body)))))

;;;-----------------------------------------------------------------------------
;;; GET-URL-IMAGE
;;;-----------------------------------------------------------------------------

#+ALLEGRO
(defun GET-URL-IMAGE (url)
  "Simply do an http-request"
  (ignore-errors (net.aserve.client:do-http-request url :format :binary)))

;;;------------------------------------------------------------------------------
;;; GOOGLE-SEARCH
;;;------------------------------------------------------------------------------

#+ALLEGRO
(defun GOOGLE-SEARCH (query-string)
  "Send the specified query request to Google, and return the resulting HTML.
   Don't do any post-processing on it.  Query string may contain embedded
   quotes."
  (let ((result-string
	 (net.aserve.client:do-http-request "https://www.google.com/search"
	   :query `(("q" . ,query-string)))))
    result-string))


;;;-----------------------------------------------------------------------------
;;; WIKI-SEARCH
;;;-----------------------------------------------------------------------------

;;; site:en.wikipedia.org  topic-foo

#+ALLEGRO
(defun GOOGLE-WIKI-SEARCH (query-string)
  "Send the specified query request to Google, and return the resulting HTML."
  (let ((result-string
	 (net.aserve.client:do-http-request "http://www.google.com/search"
	   :query `(("q" . ,(format nil "site:en.wikipedia.org ~a" query-string))))))
    result-string))

;;;-----------------------------------------------------------------------------

#+IGNORE
(defun WIKI-SEARCH (query-string)
  "Send the specified query request to Wiki, and return the resulting HTML."
  (let ((result-string
	 (net.aserve.client:do-http-request "http://en.wikipedia.org/wiki/search"
	   :query `(("q" . ,query-string)))))
    result-string))

;;;-----------------------------------------------------------------------------
;;; COMPUTE-DOCUMENT-LINKS
;;;-----------------------------------------------------------------------------

(defmethod COMPUTE-DOCUMENT-LINKS ((obj HTML-DOCUMENT))
  (let ((links-list nil)
	(links-hash (document-links obj)))
    (map-document-paragraphs
     obj
     #'(lambda (p)
	 (setf links-list (append (paragraph-links p) links-list))))
    (setf links-list (remove-duplicates links-list))
    (dolist (link links-list)
      (setf (gethash link links-hash) link))
    links-hash))

;;;-----------------------------------------------------------------------------
;;; FIND-URLS-IN-DOCUMENT
;;;-----------------------------------------------------------------------------

(defmethod FIND-URLS-IN-DOCUMENT ((html-doc HTML-DOCUMENT))
  (find-urls-in-document (document-content html-doc)))

;;;-----------------------------------------------------------------------------

(defmethod FIND-URLS-IN-DOCUMENT ((html-string STRING))
  (let* ((raw-hrefs (find-hrefs-in-html html-string)))
    (mapcar #'(lambda (href-text)
		(extract-url-from-href href-text))
	    raw-hrefs)))

;;;-----------------------------------------------------------------------------
;;; FIND-URLS-IN-PARAGRAPHS
;;;-----------------------------------------------------------------------------

(defmethod FIND-URLS-IN-PARAGRAPHS ((html-doc HTML-DOCUMENT))
  (find-urls-in-paragraphs (document-content html-doc)))

;;;-----------------------------------------------------------------------------

(defmethod FIND-URLS-IN-PARAGRAPHS ((paragraphs LIST))
  (let* ((raw-hrefs (mapcar #'find-hrefs-in-html paragraphs)))
    (mapcar #'(lambda (href-list)
		(mapcar #'(lambda (href-text)
			    (extract-url-from-href href-text))
			href-list))
	    raw-hrefs)))


;;;-----------------------------------------------------------------------------
;;; FIND-URLS-IN-PARAGRAPH
;;;-----------------------------------------------------------------------------

(defmethod FIND-URLS-IN-PARAGRAPH ((html-string STRING))
  (let* ((raw-hrefs (find-hrefs-in-html html-string)))
    (mapcar #'(lambda (href-text)
		(extract-url-from-href href-text))
	    raw-hrefs)))

;;;-----------------------------------------------------------------------------
;;; FIND-HREFS-IN-DOC
;;;-----------------------------------------------------------------------------

(defmethod FIND-HREFS-IN-DOC ((html-doc HTML-DOCUMENT)(doc-type (eql :HTML)))
  (find-hrefs-in-doc (document-content html-doc) doc-type))

;;;-----------------------------------------------------------------------------

(defmethod FIND-HREFS-IN-DOC ((html-string STRING)(doc-type (eql :HTML)))
  (find-hrefs-in-html html-string))

;;; Finds all HREF references in an html string and returns a list of
;;; URLS.

;;; Note: URLS may be absolute or relative addresses. Maybe need to
;;; add missing prefix to relative addresses.

;;;-----------------------------------------------------------------------------
;;; FIND-URLS-IN-HTML
;;;-----------------------------------------------------------------------------

(defmethod FIND-HREFS-IN-HTML ((html-doc STRING))
  (let* ((start 0)
	 (end 0)
	 (results nil)
	 (result nil))
    (loop 
      (setf start
	(search *html-prefix* html-doc :start2 end :test #'string-equal))
      (when (null start)
	(return results))
      (setf end (position #\> html-doc :start start :test #'char=))
      (when end
	(setf result (subseq html-doc start end))
	(push result results))
      (setf start end))))

;;;-----------------------------------------------------------------------------

(defun EXTRACT-URL-FROM-HREF (href-text)
  (let* ((url (strip-href-prefix href-text))
	 (space-pos (position #\space url)))
    (when (and space-pos (< space-pos (length url)))
      (setf url (subseq url 0 space-pos)))
    (subseq url 0 (1- (length url)))))

;;;-----------------------------------------------------------------------------
;;; LOAD-HTML-FILE
;;;-----------------------------------------------------------------------------

(defmethod LOAD-HTML-FILE ((filename STRING) &key (document-class 'html-document))
  (load-html-file (make-data-pathname filename "html") :document-class document-class))

;;;-----------------------------------------------------------------------------

(defmethod LOAD-HTML-FILE ((pathname PATHNAME) &key (document-class 'html-document))
  (let ((html-string-object (read-html-file pathname)))
    (when html-string-object
      (make-instance document-class
	:name  (pathname-name pathname)
	:content html-string-object
	:address pathname))))

;;;-----------------------------------------------------------------------------
;;; READ-HTML-FILE
;;;-----------------------------------------------------------------------------

(defmethod READ-HTML-FILE ((filename STRING))
  (read-html-file (make-data-pathname filename "html")))

;;;-----------------------------------------------------------------------------

(defmethod READ-HTML-FILE ((pathname PATHNAME))
  (cond ((probe-file pathname)
	 (with-open-file (file pathname :direction :input)
	   (let* ((eof -1)
		  (line nil)
		  (result-str line))
	     (loop
	       (setf line (read-line file nil eof))
	       (when (eql line -1)
		 (return t))
	       (setf result-str 
		 (concatenate 'string result-str (format nil "~%") line)))
	     (make-html-string-object (pathname-name pathname) result-str))))
	(t
	 (warn "File ~a does not exist." pathname))))

;;;-----------------------------------------------------------------------------
;;; READ-HTML-STRING
;;;-----------------------------------------------------------------------------

(defmethod READ-HTML-STRING (filename)
  (let ((pathname nil)
	(result-str ""))
    (if (pathnamep filename)
	(setf pathname filename)
      (setf pathname (make-data-pathname filename "html")))
    (when (probe-file pathname)
      (with-open-file (file pathname :direction :input)
	(let* ((eof -1)
	       (line nil))
	  (loop
	    (setf line (read-line file nil eof))
	    (when (eql line -1)
	      (return t))
	    (setf result-str 
	      (concatenate 'string result-str (format nil "~%") line))))))
    result-str))

;;;-----------------------------------------------------------------------------

(defun MAKE-HTML-STRING-OBJECT (name html-string)
  (make-instance 'HTML-STRING :name name :value  html-string))

;;;-----------------------------------------------------------------------------
;;; SAVE-HTML-FILE
;;;-----------------------------------------------------------------------------

(defmethod SAVE-HTML-FILE ((filename STRING)(html-string STRING))
  (ignore-errors (save-html-file (make-data-pathname filename "html") html-string)))

;;;-----------------------------------------------------------------------------

(defmethod SAVE-HTML-FILE ((pathname PATHNAME)(html-string STRING))
  (ignore-errors
   (with-open-file (file pathname :direction :output :if-exists :supersede)
     (write-line html-string file))
   t))

;;;-----------------------------------------------------------------------------

(defmethod SAVE-HTML-FILE ((pathname t)(html-string NULL))
  nil)

;;;-----------------------------------------------------------------------------
;;; MAKE-HTML-DOCUMENT-PARAGRAPHS
;;;-----------------------------------------------------------------------------

(defmethod MAKE-HTML-DOCUMENT-PARAGRAPHS ((html-doc UTIL::HTML-DOCUMENT)
					  &key
					  (start 0)
					  (end (length (util::document-content html-doc))))
  (when (> end 10)
    (let* ((content (util::document-content html-doc))
	   (raw-paragraphs (util::find-html-paragraphs content start end))
	   (paragraphs-hash (document-paragraphs html-doc))
	   (count 0))
      (dolist (raw-paragraph raw-paragraphs)
	(incf count)
	(setf (gethash count paragraphs-hash)
	  (make-html-document-paragraph html-doc raw-paragraph :count count)))))
  (document-paragraphs html-doc))

;;;-----------------------------------------------------------------------------
;;; MAKE-HTML-DOCUMENT-PARAGRAPH
;;;-----------------------------------------------------------------------------

;;; <paragraph-spec> = (<paragraph-text> <paragraph-start> <paragraph-end>)

(defmethod MAKE-HTML-DOCUMENT-PARAGRAPH ((doc HTML-DOCUMENT)
					 (paragraph-spec LIST)
					 &key (count 0))
  (let ((paragraph-class (default-document-paragraph-class doc))
	(paragraph-content (first paragraph-spec))
	(start (second paragraph-spec))
	(end (third paragraph-spec))
	(paragraph nil))
    (setf paragraph
      (make-html-paragraph paragraph-content
			   :class paragraph-class
			   :count count
			   :start start
			   :end end))
    (setf (paragraph-document paragraph) doc)
    paragraph))

;;;-----------------------------------------------------------------------------

(defmethod DEFAULT-DOCUMENT-PARAGRAPH-CLASS ((doc HTML-DOCUMENT)) 
  'html-paragraph)

;;;-----------------------------------------------------------------------------

(defun MAKE-HTML-PARAGRAPH (paragraph-content
			    &key
			     (count 0)
			     (start 0)
			     (end (length paragraph-content))
			     (class 'html-paragraph))
    (make-instance class
	:name (format nil "P~a" count)
	:number count
	:content paragraph-content
	:text (util::strip-html-tags paragraph-content)
	:links (util::find-urls-in-paragraph paragraph-content)
	:start start
	:end end))

;;;-----------------------------------------------------------------------------
;;; FIND-HTML-PARAGRAPHS
;;;-----------------------------------------------------------------------------

(defmethod FIND-HTML-PARAGRAPHS ((html-doc HTML-DOCUMENT)
				 &optional
				 (start 0)
				 (end (length (document-content html-doc))))
  (find-html-paragraphs (document-content html-doc) start end))

;;;-----------------------------------------------------------------------------

(defmethod FIND-HTML-PARAGRAPHS ((html-doc STRING)
				 &optional
				 (start 0)
				 (end (length html-doc)))
  (let ((paragraphs nil)
	(next-start start))
    (loop
      (multiple-value-bind (paragraph start1 end1)
	  (find-html-paragraph html-doc next-start end)
	(cond (paragraph
	       (push (list paragraph start1 end1) paragraphs)
	       (setf next-start (1+ end1)))
	      (t
	       (return t)))))
    (nreverse paragraphs)))

;;;-----------------------------------------------------------------------------
;;; LOAD-FIRST-HTML-PARAGRAPH
;;;-----------------------------------------------------------------------------

(defmethod LOAD-FIRST-HTML-PARAGRAPH ((filename PATHNAME))
  (when (probe-file filename)
    (let* ((html-string (ignore-errors (read-html-string filename)))
	   (paragraph-string (and html-string (find-first-html-paragraph html-string))))
      (when paragraph-string
	(make-html-paragraph paragraph-string :count 1)))))
    
;;;-----------------------------------------------------------------------------
;;; FIND-FIRST-HTML-PARAGRAPH
;;;-----------------------------------------------------------------------------

(defmethod FIND-FIRST-HTML-PARAGRAPH ((html-doc STRING)
				      &optional
				      (start 0)
				      (end (length html-doc)))
   (let* ((result nil)
	  (p-start1 (search "<h2>Contents<\/h2>" html-doc
			    :test #'string-equal
			    :start2 start
			    :end2 end))
	  (p-start2 (search  "<\/table>" html-doc :end2 p-start1 :from-end t))
	  (p-start (search "<p>" html-doc :start2 p-start2))
	  (p-end nil))
     (when (and p-start p-start1 (< p-start p-start1))
       (setf p-end (search "<\/p>" html-doc
			   :test #'string-equal
			   :start2 p-start
			   :end2 p-start1)))
     (when (and p-start p-end)
       (unless (>= p-start p-end)
	 (setf result (subseq html-doc p-start p-end)))
       (values result p-start p-end))))

;;;-----------------------------------------------------------------------------
;;; FIND-HTML-PARAGRAPH
;;;-----------------------------------------------------------------------------

(defmethod FIND-HTML-PARAGRAPH ((html-doc STRING)
			       &optional
			       (start 0)
			       (end (length html-doc)))
  (let ((result nil)
	(p-start (search "<p>" html-doc
			 :test #'string-equal
			 :start2 start
			 :end2 end))
	(p-end (search "<\/p>" html-doc
		       :test #'string-equal
		       :start2 start
		       :end2 end)))
    (when (and p-start p-end)
      (unless (>= p-start p-end)
	(setf result (subseq html-doc p-start p-end)))
      (values result p-start p-end))))

;;;-----------------------------------------------------------------------------
;;; FIND-HTML-HEADERS
;;;-----------------------------------------------------------------------------

(defmethod FIND-HTML-HEADERS ((html-doc HTML-DOCUMENT)(header-number INTEGER)
			      &key
			      (start 0)
			      (end (length (document-content html-doc)))
			      (recursive t))
  (find-html-headers (document-content html-doc) header-number
		     :start start :end  end :recursive recursive))

;;;-----------------------------------------------------------------------------


(defmethod FIND-HTML-HEADERS ((html-doc STRING)(header-number INTEGER)
			      &key
			      (start 0)
			      (end (length html-doc))
			      (recursive t))
  (let ((header-name (format nil "h~a" header-number))
	(headers nil)
	(name nil)
	(next-start start))
    (loop
      (multiple-value-bind (header start1 end1 start2)
	  (find-html-header html-doc header-name next-start end)
	(cond (header
	       (setf name (extract-name-from-html-header header))
	       (when name
		 (push (list name header-number start1 start2)
		       headers))
	       (setf next-start (1+ end1))
	       (when (and recursive start2)
		 (let ((child-headers (find-html-headers html-doc (1+ header-number)
							 :start start1 
							 :end start2
							 :recursive recursive)))
		   (when child-headers
		     (setf headers (cons (cons (first headers)
					       child-headers)
					 (cdr headers)))))))
	      (t
	       (return t)))))
    (nreverse headers)))

;;;-----------------------------------------------------------------------------
;;; FIND-HTML-HEADER
;;;-----------------------------------------------------------------------------

(defmethod FIND-HTML-HEADER ((html-doc STRING)(header-name STRING)
			     &optional
			     (start 0)
			     (end (length html-doc)))
  (when (and start end)
    (let* ((result nil)
	   (h-start (search (format nil "<~a>" header-name) html-doc
			    :test #'string-equal
			    :start2 start
			    :end2 end))
	   (h-end (and h-start (search (format nil "</~a>" header-name) html-doc
				       :test #'string-equal
				       :start2 start
				       :end2 end)))
	   (h-start2 (and h-end (search (format nil "<~a>" header-name) html-doc
					:test #'string-equal
					:start2 h-end
					:end2 (max h-end end)))))
      (when (and h-start h-end)
	(unless (>= h-start h-end)
	  (setf result (subseq html-doc h-start h-end)))
	(values result h-start h-end (and h-start2 (1- h-start2)))))))

;;;-----------------------------------------------------------------------------
;;; EXTRACT-NAME-FROM-HTML-HEADER
;;;-----------------------------------------------------------------------------

(defmethod EXTRACT-NAME-FROM-HTML-HEADER ((html-header STRING))
  (let* ((id-start (search "id=" html-header :test #'string-equal))
	 (id-end (and id-start (search ">" html-header :start2 id-start :test #'string-equal)))
	 (name-end (and id-end (search "<" html-header :start2 id-end :test #'string-equal))))
    (when (and id-end name-end)
      (subseq html-header (1+ id-end) name-end))))

;;;-----------------------------------------------------------------------------
;;; STRIP-HTML-TAGS
;;;-----------------------------------------------------------------------------

(defmethod STRIP-HTML-TAGS ((str STRING))
  (let* ((start nil)
	 (end nil))
    (loop
      (setf start (position #\< str :test #'char=))
      (setf end (position #\> str  :test #'char=))
      (when (null start)
	(return str))
      (when (and start end)
	(if (= start 0)
	    (setf str (subseq str (1+ end)))
	  (setf str (concatenate 'string
		      (subseq str 0 start)
		      (subseq str (1+ end)))))))
    str))


;;;-----------------------------------------------------------------------------
;;; STRIP-HREF-PREFIX
;;;-----------------------------------------------------------------------------

(defun STRIP-HREF-PREFIX (html-text)
  (subseq html-text 6))

;;----------------------------------------------------------------------------
;;; End of File
;;;----------------------------------------------------------------------------
