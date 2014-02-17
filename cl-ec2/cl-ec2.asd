;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; The cl-ec2 ASDF system definition
;;;;

(in-package :asdf)

(defsystem CL-EC2
  :name "CL-EC2"
  :author "Christopher Brown <skeptomai@mac.com>"
  :version "1"
  :maintainer "Christopher Brown <skeptomai@mac.com>"
  :licence "Apache License"
  :description "Client for Amazon's Elastic Compute Cloud"
  :long-description "Common Lisp client to run, terminate and query Amazon Elastic Compute Cloud (EC2) instances."
  :depends-on (:cl-base64 :s-xml :drakma :ironclad :cl-ppcre
                          :cl-utilities :url-rewrite :iso8601 :xml-schema-objects)
  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "cl-ec2" :depends-on ("package"))))))

;;;; eof
