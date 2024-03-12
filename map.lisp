(cl:in-package :xdoc)

(define-condition malformed-document (error)
  ((msg :type string
	:initarg :msg
	:reader error-message))
  (:report (lambda (con stream)
	     (write-string (error-message con) stream))))

(defun edmsg (msg &rest args)
  (error 'malformed-document :msg (apply #'format nil msg args)))

(defun validate-document (tree)
  (let ((name (getf tree :name))
	(children (getf tree :children))
	(wholefun (getf tree :whole))
	(entryfun (getf tree :entry))
	(exitfun (getf tree :exit))
	(textfun (getf tree :text)))
    (cond ((null name)
	   (edmsg ":NAME must be supplied"))
	  ((and wholefun (or children entryfun exitfun textfun))
	   (edmsg ":WHOLE cannot coexist with other tags"))
	  (children
	   (map nil #'validate-document children)))))

(defmacro define-document (name tree)
  "Define and validate a tree of functions to call with MAPDOC.
The tree is structured in this way:

  (:name string
   :entry function
   :text function
   :whole function
   :children (<list of trees>))

:Children contains the trees of every subelement you want to parse. You don't have
to have a tree for every child; if a tag with no tree is found, it will be
ignored.
:Entry will be called when the tag is first encountered (before its children).
:Exit will be called when the parser finds the end tag.
:Whole will have the whole element parsed and passed to the function. :Whole
cannot coexist with the other options.

The functions are called with two arguments: the first is the tag itself, and the
second is a list of its parent tags."
  (validate-document tree)
  `(defparameter ,name ',tree))

(defun mapdoc* (tree history start input)
  (let ((children (getf tree :children))
	(wholefun (getf tree :whole))
	(entryfun (getf tree :entry))
	(exitfun (getf tree :exit))
	(textfun (getf tree :text)))
    (if wholefun
	(funcall wholefun (finish-element start input) history)
	(progn
	  (when entryfun (funcall entryfun start history))
	  (when (eq (class-of start) (find-class 'start-tag))
	    (do ((tag (get-tag input) (get-tag input)))
		((typep tag 'end-tag))
	      (typecase tag
		(string (when textfun (funcall textfun tag (cons start history))))
		(tag
		 (let ((entry (find-if (lambda (elt)
					 (string= (getf elt :name)
						  (name tag)))
				       children)))
		   (if entry
		       (progn (mapdoc* entry (cons start history) tag input))
		       (finish-element tag input)))
		 (when exitfun (funcall exitfun start history))))))))))

(defun mapdoc (tree source)
  "Parse the document a tag at a time. SOURCE can be a pathname or a stream."
  (validate-document tree)
  (etypecase source
    (stream
     (let* ((input (make-instance 'xml-input :source source))
	    (begin (do ((tag (get-tag input) (get-tag input)))
		       ((typep tag '(or empty-tag start-tag)) tag))))
       (unless (string= (name begin) (getf tree :name))
	 (edmsg "The root tag of the tree and the document do not match. (~S vs. ~S)"
	       (getf tree :name)
	       (name begin)))
       (mapdoc* tree nil begin input)))
    (pathname
     (with-open-file (file source :element-type 'character)
       (mapdoc tree file)))))
