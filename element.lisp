(cl:in-package :xdoc)

(defclass tag ()
  ((name :type cons
	 :initarg :name
	 :reader name)))

(defclass elt-tag (tag)
  ((attrs :type list
	  :initarg :attrs
	  :reader attributes)))

(defclass prologue (elt-tag) ())
(defclass start-tag (elt-tag) ())
(defclass empty-tag (elt-tag) ())
(defclass end-tag (tag) ())

(defclass element (elt-tag)
  ((children :type list
	    :initform nil
	    :initarg :children
	    :accessor children)))

(defun parse-name (name)
  (let ((sep (position #\: name)))
    (cond ((null sep)
	   (cons nil name))
	  ((zerop sep)
	   (cons nil (subseq name 1)))
	  (t
	   (cons (subseq name 0 sep)
		 (subseq name (1+ sep)))))))

(defun attr (name elt)
  "Get the value of the first attribute of ELT with name NAME."
  (cdr (assoc name (attributes elt)
	      :test #'string=)))

(defun find-child (name elt)
  "Get the first child of ELT with name NAME, or the first text content if NAME = :TEXT."
  (do ((list (children elt) (cdr list)))
      ((cond ((endp list) t)
	     ((eq :text name) (stringp (car list)))
	     (t (and (typep (car list) 'tag)
		     (string= name (name (car list))))))
       (car list))))

(defun find-element (name elt)
  "A depth-first search of the entire ELT for a tag with name NAME."
  (dolist (child (children elt))
    (unless (stringp child)
      (let ((e? (find-element name child)))
	(when e? (return e?))))))

(defun text (name elt)
  "<foo><bar>hello</bar></foo> => \"hello\""
  (find-child :text (find-child name elt)))

