(cl:in-package :cl-user)

(defpackage :xdoc
  (:use :cl)
  (:export #:element
	   #:name
	   #:attributes
	   #:children
	   #:read-document-stream
	   #:read-document-file
	   #:malformed
	   #:malformed-document
	   #:define-document
	   #:mapdoc
	   #:attr
	   #:find-child
	   #:find-element
	   #:text))
