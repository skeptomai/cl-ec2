;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; The xml schema objects ASDF system definition
;;;;

(defsystem XML-SCHEMA-OBJECTS
  :name "XML-SCHEMA-OBJECTS"
  :author "Christopher Brown <skeptomai@mac.com>"
  :version "1"
  :maintainer "Christopher Brown <skeptomai@mac.com>"
  :licence "Apache License"
  :description "Serialize XML via CLOS objects whose classes are created via XSD"
  :long-description "Serialize XML via CLOS objects whose classes are created via XSD"
  :depends-on (:cl-ppcre :cl-utilities :flexi-streams :s-xml)
  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "xml-schema-objects" :depends-on ("package"))))))

;;;; eof
