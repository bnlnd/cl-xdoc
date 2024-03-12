(cl:in-package :cl-user)

(asdf:defsystem "xdoc"
  :version "0.1.0"
  :description "Map over XML documents"
  :license "zlib"
  :author "Branan Landau"
  :mailto "blandau@posteo.net"
  :depends-on ()
  :serial t
  :components ((:file "package")
	       (:file "input")
	       (:file "element")
	       (:file "parse")
	       (:file "map")))
