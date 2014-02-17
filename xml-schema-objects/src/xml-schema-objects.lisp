;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;; This is a Common Lisp implementation of XML serialization 
;;;; via CLOS classes created from XSD
;;;;

;; Copyright 2009 Christopher Brown (irc: skeptomai) (email: cb@skeptomai.com)
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package :xml-schema-objects)

;; We ensure the XML schema types are in our secret
;; namespace 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors
    (make-package "schema-set"))

  #+sbcl
  (use-package 'sb-mop)
  #+lispworks
  (use-package 'clos)
  
  (defvar *internal-schema-package-sym* '|schema-set|)

  (defvar *debug* nil)

  (defmacro dprint (fspec &rest items) 
    (and *debug* `(format t ,fspec ,@items)))
  
  (let ((schema-sym-names
         (list "schema" "complexType" 
               "group" "annotation" 
               "element" "group" "sequence" 
               "choice" "attribute"))
        (internal-schema-package (find-package *internal-schema-package-sym*)))
    (dolist (np schema-sym-names) 
      (export (intern np internal-schema-package) *internal-schema-package-sym*))))

(defvar *types* (make-hash-table :test #'equal)) 

(defgeneric read-type (sxml typedesc))
(defgeneric write-type (obj name))

(defun register-namespace-and-type (ns type)
  "puts the new type in hash, by its namespace"
  (dprint "registering ns ~a, type ~a~%" ns type)
  (symbol-macrolet ((type-hash (gethash ns *types*)))
    (or type-hash (setf type-hash (make-hash-table :test #'equal)))
    (setf (gethash type type-hash) type)
    *types*))

(defmacro dive (expr)
  (let ((diver (gensym)))
    `(let ((,diver ,expr))
       (if (consp ,diver)
           (first ,diver)
           ,diver))))

(declaim (inline mod-up))
(defun mod-up (expr)
  "turns atom into list if necessary to make all inputs lists"
  (if (consp expr)
      expr
      (list expr)))

(declaim (inline zip))
(defun zip (l1 l2)
  "zips two lists into one"
  (let ((res (list)))
    (mapcar #'(lambda (l r) (push l res) (push r res)) l1 l2)  (nreverse res)))

(defclass context ()
  ((level :initarg :level :accessor context-level)
   (name :initarg :name :accessor context-name)
   (schema-type :initarg :schema-type :initform nil :accessor context-schema-type)
   (target-namespace :initarg :target-namespace :accessor context-target-namespace))
  (:documentation "carries context while reading XML response, including depth"))

(defclass element ()
  ((name :initarg :name :initform nil :accessor element-name)
   (sym-name :initarg :sym-name :initform nil :accessor element-sym-name)
   (type :initarg :type :initform nil :accessor element-type)
   (arity :initarg :arity :initform 1 :accessor element-arity))
  (:documentation "describes an XML element"))

(defclass xs-date-time ()
  ((date-time-val :initarg :date-time-val :accessor xs-date-time-val))
  (:documentation "represents XS dateTime as a string value"))

(defclass xs-string ()
  ((string-val :initarg :string-val :accessor xs-string-val))
  (:documentation "represents XSD/XML string type"))

(defclass xs-boolean ()
  ((boolean-val :initarg :boolean-val :accessor xs-boolean-val))
    (:documentation "represents XSD/XML boolean type"))

(defclass xs-int ()
  ((int-val :initarg :integer-val :accessor xs-int-val))
    (:documentation "represents XSD/XML integer type"))

(defun find-element-name (element-type &optional (containing-sym 'doc-root))
  "finds the element name in the doc-root collection"
  (dprint "~a, element-type ~a, in sym ~a~%" 'find-element-name element-type containing-sym)
  (with-hash-table-iterator (elements (get containing-sym 'slot-elements))
    (loop
       (multiple-value-bind (more? key value) (elements)
         (unless more? (return))
         (dprint "type: ~a, key ~a~%" element-type key)
         (when (eql element-type key )
           (return (element-name value)))))))

(defun find-target-namespace (schema-decl)
  "finds target namespace in XML processing instructions"
  (let* ((decls (mapcar #'(lambda (x) 
                            (cons (cl-utilities:split-sequence #\: (string (car x))) (rest x)))
                        (typedesc->alist (rest (first schema-decl)))))
         (target-ns 
          (find-if #'(lambda (x) (string-equal (first (first x)) "targetNamespace")) decls))
         (prefix-and-ns 
          (find-if #'(lambda (x) 
                       (and (not (string-equal (first (first x)) "targetNamespace")) 
                            (string-equal (cdr x) (cdr target-ns)))) decls)))
    (values (second (first prefix-and-ns)) (cdr prefix-and-ns))))

(defun get-sym (sym-name)
  "makes symbol from sym-name according to our rules"
  (dprint "~a making symbol from ~a  ~%" 'get-sym sym-name)
  (let ((sym (intern (trans-ident sym-name) :xml-schema-objects)))
    (dprint "~a made symbol called ~a ~%" 'get-sym sym)
    sym))
  
(defun swap-type-sym (type-sym)
  "replaces the arbitrary namespace prefix used for schema with our internal one"
  (find-symbol (string type-sym) *internal-schema-package-sym*))

(defun as-keyword (sym)
  "returns a keyword symbol"
  (intern (string sym) :keyword))

(declaim (inline alist->prop))
(defun alist->prop (prop type-alist)
  `(,prop (cdr (assoc (as-keyword (string ',prop)) ,type-alist))))

(declaim (inline alist->props))
(defun alist->props (props type-alist)
  (mapcar #'(lambda (x) (alist->prop x type-alist)) props))

(defun typedesc->alist (typedesc)
  "turns the remainder of an element's description into an alist"
  (let ((type-alist nil)
        (elem (copy-list typedesc)))
    (loop
       do
       (setf type-alist (acons (pop elem) (pop elem) type-alist))
       (if (null elem)
           (return type-alist)))))

(defmacro with-props ((&rest props) typedesc &body body)
  (let ((type-alist (gensym)))
    `(let* ((,type-alist (typedesc->alist ,typedesc))
            ,@(alist->props props type-alist))
       (progn
         ,@body))))

(defmethod read-type :before (sxml typedesc)
  (dprint "read-type ~a , ~a~%" sxml typedesc))

(defmethod write-type :before (obj name)
  (dprint "write-type ~a, object ~a, name ~a~%" (class-of obj) obj name))

(defmethod read-type (sxml (typedesc (eql 'xs-date-time)))
  "read an xsd datetime"
  (dprint "read-type ~a , ~a~%" sxml typedesc)
  (let ((instance (make-instance 'xs-date-time)))
    (setf (xs-date-time-val instance) (if (consp sxml)
                                       (second sxml)
                                       nil))
    instance))

(defmethod write-type ((obj xs-date-time) name)
  "write an xsd string to sxml dom"
  `(,name ,(xs-date-time-val obj)))

(defmethod read-type (sxml (typedesc (eql 'xs-string)))
  "read an xsd string"
  (dprint "read-type ~a , ~a~%" sxml typedesc)
  (let ((instance (make-instance 'xs-string)))
    (setf (xs-string-val instance) (if (consp sxml)
                                       (second sxml)
                                       nil))
    instance))

(defmethod write-type ((obj xs-string) name)
  "write an xsd string to sxml dom"
  `(,name ,(xs-string-val obj)))

(defmethod read-type (sxml (typedesc (eql 'xs-boolean)))
  "read an xsd boolean"
  (let ((instance (make-instance 'xs-boolean)))
    (setf (xs-boolean-val instance) (second sxml))
    instance))

(defmethod write-type ((obj xs-boolean) name)
  "write an xsd boolean to sxml dom"
  `(,name ,(xs-boolean-val obj)))

(defmethod read-type (sxml (typedesc (eql 'xs-int)))
  "read an xsd int"
  (let ((instance (make-instance 'xs-int)))
    (setf (xs-int-val instance) (second sxml))
    instance))

(defmethod write-type ((obj xs-int) name)
  "write an xsd int to sxml dom"
  `(,name ,(xs-int-val obj)))

(defun flatten (l)
  "turns nested list into single depth list"
  (cond ((null l) '())
        ((consp l)
         (append (flatten (car l)) (flatten (cdr l))))
        (t (list l))))

(defun trans-ident (str)
  "turns XSD identifiers in camel case to lisp identifiers with hyphens"
  (if (symbolp str)
      (string str)
      (let ((ident (copy-seq str)))
        (substitute #\- #\: 
                    (string-upcase
                     (concatenate 
                      'string
                      (nreverse   
                       (let ((trans nil))
                         (loop for char across ident
                            and pos = 0 then (1+ pos)
                            do (cond ((and 
                                       (< (char-code char) 97) 
                                       (> (char-code char) 64))
                                      (when 
                                          (and 
                                           (> pos 0) 
                                           (not (char-equal (char ident (1- pos)) #\:)))
                                        (push #\- trans))
                                      (push (code-char (+ 32 (char-code char))) trans))
                                     (t (push char trans))))
                         trans))))))))

(defun element->slot (class-sym element)
  "takes an element description and builds a slot definition in class-sym"
  (let* ((slot-name-sym (element-sym-name element))
         (accessor-sym (intern 
                        (string (make-symbol (concatenate 'string 
                                                          (string class-sym) "-" 
                                                          (string slot-name-sym)))) :xml-schema-objects)))
    `(,slot-name-sym :initarg ,(as-keyword slot-name-sym) :initform nil :accessor ,accessor-sym )))

(defun elements->slots (class-sym elements)
  "builds slot symbol names from the class name and each element"
  (dprint "~a element-slots: ~{~a~^,~} ~%" 'slot-defs-from-elements elements)
  (mapcar #'(lambda (e) (element->slot class-sym e)) elements))

(defun hang-elements-on-class-sym (class-sym elements)
  "create hash table of element names to their type for children of this element"
  (symbol-macrolet ((slot-element-hash (get class-sym 'slot-elements)))
    (setf slot-element-hash (make-hash-table))
    
    (dolist (np elements)
      (dprint "now hanging ~a with value ~a on ~a ~%" (element-sym-name np) (element-type np) class-sym)
      (setf (gethash (element-sym-name np) slot-element-hash) np))))

(defun class-from-class-sym (class-sym slots)
  "creates a CLOS class from class-sym and its slot descriptions"
  (dprint "class exist yet? ~a : ~a~%" class-sym (find-class class-sym nil))
  (eval `(defclass ,class-sym () (,@slots))))

(defun printer-from-class-sym (class-sym)
  "object printer method for each class"
  (eval 
   `(progn
      (dprint "currently in package ~a ~%" *package*)
      (defmethod print-object ((obj ,class-sym) stream)
        (let* ((slot-names (get-obj-slot-names obj))
               (slot-vals (get-obj-slot-values obj)))
          (print-unreadable-object (obj stream :type t :identity t)
            (format stream "~a ~{~a:~a~^,~}" ',class-sym (zip slot-names slot-vals))))))))

(defun reader-from-class-sym (class-sym)
  "object reader method for each class"
  (eval `(progn
           (defmethod read-type (sxml (typedesc (eql ',class-sym)))
             (dprint "read-type for ~a, expression ~a ~%" ',class-sym sxml )
             ;; create instance of our type
             (let ((instance (make-instance ',class-sym)))
               ;; make nice with getting its hash of element-sym-name to type mappings
               (symbol-macrolet ((slot-element-hash (get ',class-sym 'slot-elements)))
                 ;; for each of the children in this element, read 
                 ;; BUGBUG ... I mod-up sxml because empty elements just appear in lxml
                 ;; as bare values, rather than a list with one member [cb]
                 (dolist (np (rest (mod-up sxml)))
                   (dprint "what? ~a ~%" np)
                   (let* ((element-name (string (dive (dive np))))
                          (element-sym-name (get-sym element-name )))
                     (dprint "now trying out ~a with name ~a, ~a ~%" np element-name element-sym-name)
                     (when *debug*
                       (maphash #'(lambda (key value) 
                                  (declare (ignorable key value))
                                  (dprint "the hash contains ~a ~a~%" key value)) slot-element-hash))
                     (dprint "calling with ~a type ~%" 
                             (element-type (gethash element-sym-name slot-element-hash)))
                     (setf (slot-value instance element-sym-name) 
                           (cons 
                            (read-type np (element-type (gethash element-sym-name slot-element-hash))) 
                            (slot-value instance element-sym-name))))))
               instance )))))

(defun get-obj-slot-names (obj)
  "gets the class's direct slots... we have no inheritance worries here"
  (mapcar #'(lambda (x) (slot-definition-name x)) (class-direct-slots (class-of obj))))

(defun get-obj-slot-values (obj)
  "gets the class's direct slot values... we have no inheritance worries here"
  (mapcar #'(lambda (x) (slot-value obj (slot-definition-name x))) (class-direct-slots (class-of obj))))

(defun export-class (class-sym)
  "exports the class into xml-schema-objects package"
  (export class-sym :xml-schema-objects))

(defun export-class-accessors (class-sym)
  "export the class accessor methods into xml-schema-objects package"
  (let ((class (find-class class-sym)))
    (dolist (np 
             (class-direct-slots class))
      (dolist (reader 
               (slot-definition-readers np))
        (export reader :xml-schema-objects))
      (dolist (writer 
               (slot-definition-writers np))
        (export writer :xml-schema-objects)))))

(defun writer-from-class-sym (class-sym)
  "object writer method from class"
  (eval 
   `(progn
      (defmethod write-type ((obj ,class-sym) name)
        (dprint "currently in package ~a ~%write-type for ~a~%" *package* ',class-sym)
        ;; first, create node for this type
        (let ((node (list name))
              (slot-names (get-obj-slot-names obj))
              (slot-values (get-obj-slot-values obj))
              (sym-find ',class-sym))
          (dprint "slot names : ~{~a~^,~}~%" slot-names)
          (dprint "slot values : ~{~a~^,~}~%" slot-values)
          (loop for name in slot-names
             for value in slot-values
             do (mapcar 
                 #'(lambda (x) 
                     (push (write-type x (as-keyword (find-element-name name sym-find))) node)) 
                 (mod-up value)))
          (nreverse node))))))
    
(defun make-sxml-class (class-sym elements)
  "makes the new class based on name and elements in slots"
  (let ((slots (elements->slots class-sym elements)))
    (hang-elements-on-class-sym class-sym elements)
    ;; now create class and reader method for this class
    (class-from-class-sym class-sym slots)
    ;; and create reader method for this type
    (reader-from-class-sym class-sym)
    ;; create the writer
    (writer-from-class-sym class-sym)
    ;; create a printer
    (printer-from-class-sym class-sym)
    ;; now cheat to export the class and its accessors
    (export-class class-sym)
    (export-class-accessors class-sym)))

(defgeneric sxml->type (type sxml &key context )
  (:documentation "Creates a lisp type from a particular SXML element"))

(declaim (inline sxml-to-type))
(defun sxml-to-type (sxml &key context)
  "massage the sxml snippet to get type-name, then dispatch on that"
  (let ((type-name (dive (first sxml))))
    (sxml->type (swap-type-sym type-name) sxml :context context)))

(declaim (inline recur-elements-from-expr))
(defun recur-elements-from-expr (expr &key context)
  (flatten (nreverse (mapcar #'(lambda (np) (sxml-to-type np :context context)) expr))))

(defmethod sxml->type ((xmltype (eql '|schema-set|:|element|)) sxml &key (context 0))
  "creates type from element"
  (declare (ignorable context))
  (with-props (|type| |name| |maxOccurs|) (rest (first sxml))
    ;; BUGBUG fix minOccurs & maxOccurs
    (let* ((sym-name (get-sym |name|))
           (sym-type (get-sym |type|))
           (element 
            (make-instance 'element :name |name| :sym-name sym-name :type sym-type :arity |maxOccurs|)))
      
      ;; register that this element appears in the namespace
      ;; and what its type is for verification at XML read time
      (dprint "~a: context: ~a, expect an element ~a  ~%"
              'building-element-type
              context
              element)
      
      (destructuring-bind (type-namespace type-name) 
          (cl-utilities:split-sequence #\: |type|)
        (declare (ignore type-name))
        (register-namespace-and-type type-namespace sym-type))
      element)))

(defmethod sxml->type ((type (eql '|schema-set|:|complexType|)) sxml &key (context 0))
  "creates lisp type (de)serializers from xsd complex-type declaration"
  
  ;; BUGBUG: make sure this conforms.  second element should
  ;; be a cons with sequence, or group or something
  ;; empty-element-type is not! [cb]
  
  (destructuring-bind ((type-name &rest props) &rest child-elements) sxml
    (declare (ignore type-name))
    (with-props (|name|) props
                (let ((class-sym (get-sym (concatenate 'string (context-target-namespace context) ":" |name|))))
                  (dprint "~a, context: ~a, Build type definition for: ~a~%" 
                          'building-complex-type context
                          class-sym)
                  (make-sxml-class 
                   class-sym
                   (flatten (mapcar #'(lambda (np) 
                                        (when (consp np) 
                                          (dprint "building ~a~%" np)
                                          (sxml-to-type np :context context)))
                                    child-elements)))))))

(defmethod sxml->type ((type (eql '|schema-set|:|group|)) sxml &key (context 0))
  "creates lisp types from group"
  (dprint "~a context: ~a, ~a~%"
          'building-group-type
          context
          sxml)
  (recur-elements-from-expr (rest sxml) :context context))

(defmethod sxml->type ((type (eql '|schema-set|:|choice|)) sxml &key (context 0))
  "creates lisp types from choice"
  (dprint "~a context: ~a,  ~a~%"
          'emit-choice-readers
          context
          sxml)
  (recur-elements-from-expr (rest sxml) :context context))

(defmethod sxml->type ((type (eql '|schema-set|:|sequence|)) sxml &key (context 0))
  "creates types from sequence"
  (dprint "~a context: ~a : ~a~%"
          'emit-sequence-readers-for
          context
          (rest sxml))
  (recur-elements-from-expr (rest sxml) :context context))

(defmethod sxml->type ((type (eql '|schema-set|:|schema|)) sxml &key (context 0))
  (declare (ignore context))
  nil)

(defmethod sxml->type ((type (eql '|schema-set|:|annotation|)) sxml &key (context 0))
  (declare (ignore context))
  nil)

(defmethod sxml->type ((type (eql '|schema-set|:|attribute|)) sxml &key (context 0))
  (declare (ignore context))
  (sxml->type '|schema-set|:|element| sxml))

(defmethod print-object ((obj context) stream)
  (with-slots (name level schema-type) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "name ~a level ~a schema-type ~a" name level schema-type))))

(defmethod print-object ((obj element) stream)
  (with-slots (name type arity) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "name ~a type ~a arity ~a" name type arity))))

(defmethod print-object ((obj xml-schema-objects:xs-string) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (xml-schema-objects::xs-string-val obj))))

(defmethod print-object ((obj xml-schema-objects:xs-boolean) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (xml-schema-objects::xs-boolean-val obj))))

(defun read-response (response)
  "reads XML response, reads type from that expression"
  (dprint "~a : response ~a ~%" 'read-response response)
  (let ((class-sym (get-sym (string (first (first response))))))
    (read-type response (element-type (gethash class-sym (get 'doc-root 'slot-elements))))))

(defun partition-types (predicate objects)
  "based on predicate, partition types into passed and failed lists"
  (let ((passed-test nil)
        (failed-test nil))
    (dolist (np objects)
      (if (funcall predicate np)
          (push np passed-test)
          (push np failed-test)))
    (values passed-test failed-test)))

(defun build-schema-classes (schema)
  "top level fn.  reads schema and creates types"
  (let ((tns (find-target-namespace schema)))
    (make-sxml-class 'doc-root 
                     (partition-types 
                      #'(lambda (x) (eql (type-of x) 'element))
                      (mapcar #'(lambda (np) 
                                  (when (consp np) 
                                    (dprint "building ~a~%" np)
                                    (sxml-to-type np 
                                                  :context (make-instance 'context
                                                                          :level 0
                                                                          :name 'doc-root
                                                                          :target-namespace tns))))
                              schema)))))

 

