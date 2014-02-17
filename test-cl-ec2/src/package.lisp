;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; This is a Common Lisp implementation of XML serialization via CLOS classes created from XSD
;;;;

(defpackage :test-cl-ec2
  (:documentation "test calls against ec2")
  (:export 
   "WRITE-TEST" )
  (:use :cl :cl-ec2 :xml-schema-objects :cl-ppcre :cl-utilities :flexi-streams :s-xml))

(in-package :test-cl-ec2)

;;;; eof
