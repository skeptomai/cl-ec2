;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-

(in-package :test-cl-ec2)

(defun write-test ()
  "test writing an XML serializer class"
  (s-xml:print-xml-string 
   (write-type 
    (let* ((diri (make-instance 'xml-schema-objects:tns-describe-images-response-item-type)))
      (setf (slot-value diri 'xml-schema-objects::is-public ) 
            (list (make-instance 'xs-string :string-val "yes")))
      (setf (slot-value diri 'xml-schema-objects::image-owner-id ) 
            (list (make-instance 'xs-string :string-val "Bobby Dirt")))
      (setf (slot-value diri 'xml-schema-objects::image-state )
            (list (make-instance 'xs-string :string-val "New York State of Mind")))
      (setf (slot-value diri 'xml-schema-objects::image-location ) 
            (list (make-instance 'xs-string :string-val "Jamaica, Queens")))
      (setf (slot-value diri 'xml-schema-objects::image-id ) 
            (list (make-instance 'xs-string :string-val "Just me baby")))
      diri) 
    (xml-schema-objects:as-keyword 
     (find-element-name 'xml-schema-objects::describe-images-response))) :input-type :lxml))

