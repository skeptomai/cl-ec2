;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; This is a Common Lisp implementation of XML serialization via CLOS classes created from XSD
;;;;

(defpackage :xml-schema-objects  
  (:export "BUILD-SCHEMA-CLASSES" "READ-RESPONSE" "READ-TYPE" "WRITE-TYPE" "FIND-ELEMENT-NAME" 
           "AS-KEYWORD" "XS-STRING" "XS-STRING-VAL" "XS-BOOLEAN" "XS-BOOLEAN-VAL" "GET-OBJ-SLOT-NAMES" 
           "GET-OBJ-SLOT-VALUES")
  (:documentation "creates CLOS classes from XSD and instances from XML files read in")
  (:use :cl ))

;;;; eof
