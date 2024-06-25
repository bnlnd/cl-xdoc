(cl:in-package :xdoc)

(defclass tag ()
  ((name :type string
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

(defmethod name ((object string)) :text)
(defmethod children ((object string)) nil)

(defun attr (name elt)
  "Get the value of the first attribute of ELT with name NAME."
  (cdr (assoc name (attributes elt)
	      :test #'string=)))

(defun find-child (name elt)
  "Find the first child with name NAME, or text when NAME = :TEXT."
  (find name (children elt) :key #'name :test 'equal))

(defun find-element (name elt)
  "A depth-first search of the entire ELT for a tag with name NAME."
  (if (equal name (name elt))
      elt
      (dolist (child (children elt))
       	(let ((elt? (find-element name child)))
	  (when elt? (return elt?))))))

(defun find-all-children (name elt)
  "Make a list of all children with name NAME, or text when NAME = :TEXT."
  (remove name (children elt) :key #'name :test-not 'equal))

(defun text (name elt)
  "<foo><bar>hello</bar></foo> => \"hello\""
  (find-child :text (find-child name elt)))
