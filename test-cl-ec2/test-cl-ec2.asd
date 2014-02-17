;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; The test-compile ASDF system definition
;;;;

(defsystem TEST-CL-EC2
  :name "TEST-CL-EC2"
  :author "Christopher Brown <skeptomai@mac.com>"
  :version "1"
  :maintainer "Christopher Brown <skeptomai@mac.com>"
  :licence "Apache License"
  :description "test calls against ec2"
  :depends-on (:cl-ec2 :xml-schema-objects :cl-ppcre :cl-utilities :flexi-streams :s-xml)
  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "test-cl-ec2" :depends-on ("package"))))))

;;;; eof
