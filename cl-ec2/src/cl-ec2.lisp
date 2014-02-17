;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-

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

(in-package :cl-ec2)

(use-package :xml-schema-objects)

;; Make sure the xsd based classes are built and are
;; entered in compiled fasl
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun state->fn (x)
    "function turns state X into a fetcher of instances in that state (e.g. running, pending, terminated)"
    `(defun ,(intern (concatenate 'string (string x) "-INSTANCES")) () (insts-by-state ,(string x))))

  (defvar *this-file* (load-time-value
                       (or #.*compile-file-pathname* *load-pathname*)))

  (build-schema-classes 
   (s-xml:parse-xml-file (make-pathname :name "ec2" :type "xsd" :version nil
                 :defaults *this-file*))))

(defvar *ec2-addr* "ec2.amazonaws.com")
(defvar *signature-version-string* "1")
(defvar *api-version-string* "2008-12-01")
(defvar *schema-uri* "http://www.w3.org/2001/XMLSchema")
(defvar *types* (make-hash-table))

(defun canonicalize-string (str)
  "simply remove all ampersands, question marks or equal signs before signing"
  (cl-ppcre:regex-replace-all "\\&|\\?|=" str ""))

(defun sha1-digest (secret-key-id str)
  "generates digest of string as an array of unsigned bytes"
  (let ((hmac (crypto:make-hmac (coerce (flexi-streams:string-to-octets secret-key-id)  '(simple-array (unsigned-byte 8) (*))) :sha1))
        (data (coerce (flexi-streams:string-to-octets str) ' (simple-array (unsigned-byte 8) (*)))))
    (crypto:update-hmac hmac data)
    (crypto:hmac-digest hmac)))

(defun base64-sha1-digest-string (secret-key-id str)
  "returns a base64 string of the digest of str"
  (cl-base64:usb8-array-to-base64-string (sha1-digest secret-key-id str)))

(defun partition-params (params predicate)
  "quick separation of parameters based on predicate test"
  (let ((passed-test nil)
        (failed-test nil))
    (dolist (np params)
      (if (funcall predicate np)
          (push np passed-test)
          (push np failed-test)))
    (list passed-test failed-test)))

(defun disambiguate-params (params)
  "turns repeated params into indexed params"
  (flet ((disambiguatex (params)
           (if (equal (length params) 1)
               params
               (let ((index 0))
                 (mapcar #'(lambda (x)
                             (prog1
                                 (cons (format nil "~a.~d" (car x) index) (cdr x))
                               (incf index)))
                         params)))))
    (when (car params)
      (progn 
        (let* ((key (first (first params)))
               (new-params (partition-params params #'(lambda (x) (string-equal key (car x))))))
          (concatenate 'list 
                       (disambiguatex (first new-params)) 
                       (disambiguate-params (second new-params))))))))
  
(defun build-request-url (access-key-id secret-key-id action &key (params '()) (secure nil) (port 80))
  "creates the full query url based on your AWS access key, secret key, action and necessary params"
  (flet ((canonicalize-params (params)
           (sort params #'string< :key #'(lambda (x) (string-downcase (car x)))))
         (build-query-string (canonical-params &optional (encoder 'identity))
           (format nil "?~{~a~^&~}" 
                   (map 'list
                        #'(lambda (x)
                            (format nil "~a=~a"
                                    (apply encoder (list (car x)))
                                    (apply encoder (list (cdr x))))) canonical-params))))
    (let* ((ec2-parameter-list `(("SignatureVersion" . ,*signature-version-string*)
                                 ("AWSAccessKeyId" . ,access-key-id )
                                 ("Version" . ,*api-version-string* )
                                 ("Timestamp" . ,(iso8601:format-iso8601-time (get-universal-time) t))
                                 ("Action" . ,action)))
           (canonical-params (canonicalize-params (concatenate 'list ec2-parameter-list (disambiguate-params params))))
           (signable-string (build-query-string canonical-params))
           (query-string (build-query-string canonical-params #'url-rewrite:url-encode))
           (digest-string (url-rewrite:url-encode (base64-sha1-digest-string secret-key-id (canonicalize-string signable-string))))
           (signed-query-string (format nil "~a&Signature=~a" query-string digest-string))
           (protocol (if secure "https" "http"))
           (url-port (cond ((not (eql port 80)) port)
                           (secure 443)
                           (t port))))
      (format nil "~a://~a:~a/~a" protocol *ec2-addr* url-port signed-query-string))))

(defun make-request (url)
  "execute the HTTP query request in url"
  (multiple-value-bind (body status) 
      (drakma:http-request url)
    (values body status)))

(defun make-simple-request (req cred &optional params)
  "sends HTTP query request and parses XML response"
  (declare (ignorable params))
  (s-xml:parse-xml-string 
   (make-request 
    (build-request-url 
     (cred-akid cred)
     (cred-secid cred)
     req :params params))))

(defmacro define-state-fns (&rest fn-names)
  `(progn
     ,@(mapcar #'state->fn fn-names)))

(define-state-fns running pending terminated)

(defclass credentials ()
  ((access-key-id :initarg :access-key-id :reader cred-akid)
   (secret-key-id :initarg :secret-key-id :reader cred-secid))
   (:documentation "Simple immutable to hold user's akid & skid"))

(defvar *cred* (make-instance 'credentials :access-key-id "" 
                              :secret-key-id ""))

(declaim (inline zip))
(defun zip (l1 l2)
  "takes two lists and returns 1 interleaved list"
  (let ((res (list)))
    (mapcar #'(lambda (l r) (push l res) (push r res)) l1 l2)  (nreverse res)))

(defmacro dive (expr)
  "if list passed, return first element, else return atom"
  (let ((diver (gensym)))
    `(let ((,diver ,expr))
       (if (consp ,diver)
           (first ,diver)
           ,diver))))

(defclass terminate-instances-info ()
  ((instance-id :initarg :instance-id :reader terminate-instances-info-instance-id)
   (previous-state :initarg :previous-state :reader terminate-instances-info-previous-state)
   (shutdown-state :initarg :shutdown-state :reader terminate-instances-info-shutdown-state))
  (:documentation "distills xml-mapped response to usable info about instances killed"))

(defmethod print-object ((obj terminate-instances-info) stream)
  (with-slots (instance-id previous-state shutdown-state) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "instance-id \"~a\", previous-state \"~a\", shutdown-state \"~a\"" 
              instance-id previous-state shutdown-state))))

(defun terminate-instances-unbundler (response)
  "turns xml-mapped response into terminate-instances-info objects"
  (let* ((term-info-type (xml-schema-objects:tns-terminate-instances-response-type-instances-set response))
         (term-items (xml-schema-objects:tns-terminate-instances-response-info-type-item (dive term-info-type)))
         (items (list)))

    (dolist (term-item term-items)
      (let ((instance-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-terminate-instances-response-item-type-instance-id term-item))))
            (previous-state (xml-schema-objects:xs-string-val 
                             (dive (xml-schema-objects:tns-instance-state-type-name
                                    (dive (xml-schema-objects:tns-terminate-instances-response-item-type-previous-state term-item))))))
            (shutdown-state (xml-schema-objects:xs-string-val 
                             (dive (xml-schema-objects:tns-instance-state-type-name
                                    (dive (xml-schema-objects:tns-terminate-instances-response-item-type-shutdown-state term-item)))))))
        (push (make-instance 'terminate-instances-info :instance-id instance-id :previous-state previous-state
                             :shutdown-state shutdown-state) items)))
    items))

(defun terminate-instances (&rest instance-ids)
  "API to kill instances"
  (let ((params nil))
    (dolist (np instance-ids)
      (unless (null np)
        (push `("InstanceId" . ,np) params)))
    (unless (null params)
      (let* ((xml-response 
              (make-simple-request "TerminateInstances" *cred* params)))
        (terminate-instances-unbundler (read-response xml-response))))))

(defclass image-desc ()
  ((image-id :initarg :image-id :reader image-desc-image-id)
   (image-state :initarg :image-state :reader image-desc-image-state)
   (image-location :initarg :image-location :reader image-desc-image-location)
   (is-public :initarg :is-public :reader image-desc-is-public)
   (owner-id :initarg :owner-id :reader image-desc-owner-id))
  (:documentation "distills information about registered images"))

(defmethod print-object ((obj image-desc) stream)
  (let* ((slot-names (xml-schema-objects:get-obj-slot-names obj))
         (slot-vals (mapcar
                     #'(lambda (x)
                         (if (equal (class-of (dive x)) (find-class 'xs-string))
                             (xs-string-val x)
                             (xs-boolean-val x)))
                     (xml-schema-objects:get-obj-slot-values obj))))
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "~{~a: ~a~^, ~}"  (zip slot-names slot-vals)))))

(defun image-response-item-unbundler (describe-images-response-item)
  "creates image-desc objects from xml-mapped response for describe-images"
  (let ((image-state
         (dive (xml-schema-objects:tns-describe-images-response-item-type-image-state describe-images-response-item)))
         (image-id
          (dive (xml-schema-objects:tns-describe-images-response-item-type-image-id describe-images-response-item)))
         (is-public
          (dive (xml-schema-objects:tns-describe-images-response-item-type-is-public describe-images-response-item)))
         (image-location
          (dive (xml-schema-objects:tns-describe-images-response-item-type-image-location describe-images-response-item)))
         (owner-id
          (dive (xml-schema-objects:tns-describe-images-response-item-type-image-owner-id describe-images-response-item))))
    (make-instance 'image-desc :image-id image-id :image-state image-state :image-location image-location
                   :is-public is-public :owner-id owner-id)))
    
(defun describe-images ()
  "API for describe-images"
  (let* ((xml-response 
          (make-simple-request "DescribeImages" *cred*))
         (items (list))
         (resp (read-response xml-response)))
    (mapcar #'(lambda (x) (push (image-response-item-unbundler x) items))
            (xml-schema-objects:tns-describe-images-response-info-type-item 
             (first (xml-schema-objects:tns-describe-images-response-type-images-set resp))))
    items))

(defclass instance-desc ()
  ((instance-id :initarg :instance-id :reader instance-desc-instance-id)
   (dns-name :initarg :dns-name :reader instance-desc-dns-name)
   (instance-state :initarg :instance-state :reader instance-desc-instance-state))
  (:documentation "useful information about instances"))

(defmethod print-object ((obj instance-desc) stream)
  (with-slots (instance-id dns-name instance-state) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "instance-id \"~a\", dns-name \"~a\", instance-state \"~a\"" instance-id dns-name instance-state))))

(defclass reservation-info ()
  ((owner-id :initarg :owner-id :reader reservation-info-owner-id)
   (reservation-id :initarg :reservation-id :reader reservation-info-reservation-id)
   (instance-descs :initarg :instance-descs :reader reservation-info-instance-descs))
  (:documentation "useful information about reservations"))

(defmethod print-object ((obj reservation-info) stream)
  (with-slots (owner-id reservation-id instance-descs) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "owner-id \"~a\", reservation-id \"~a\", instance-descs: ~{~a~%~}"
            owner-id reservation-id instance-descs))))

(defun instances-by-state (state reservation-info)
  "base information about reservation by given state"
  (let ((instances (reservation-info-instance-descs reservation-info)))
    (remove-if-not #'(lambda (x) (string-equal (symbol-name state) (xs-string-val (instance-desc-instance-state x)))) instances)))

(defun insts-by-state (state)
  (loop for id in 
        (mapcan #'reservation-info-instance-descs (describe-instances)) 
        when (string-equal 
               (instance-desc-instance-state id) state) collect id ))

(defun describe-instances-response-item-unbundler (reservation-info-type)
  "distills xml-mapped response to instance information from reservation"
  (let* ((owner-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-reservation-info-type-owner-id (dive reservation-info-type)))))
         (reservation-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-reservation-info-type-reservation-id (dive reservation-info-type)))))
         (instances-set (xml-schema-objects:tns-reservation-info-type-instances-set 
                         (dive reservation-info-type)))
         (instance-descs (list)))
    (dolist (np (xml-schema-objects:tns-running-instances-set-type-item (dive instances-set)))
      (let ((dns-name (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-running-instances-item-type-dns-name np))))
            (instance-state 
              (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-instance-state-type-name
                    (dive (xml-schema-objects:tns-running-instances-item-type-instance-state np))))))
            (instance-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-running-instances-item-type-instance-id np)))))
        (push 
         (make-instance 'instance-desc 
                        :instance-id instance-id :dns-name dns-name 
                        :instance-state instance-state) instance-descs)))
    (make-instance 'reservation-info :owner-id owner-id :reservation-id reservation-id 
                   :instance-descs instance-descs )))


(defun run-instances-response-item-unbundler (run-instances-response-type)
  "distills xml-mapped response to instance information from reservation"
  ;; BUGBUG for now, we ignore the requester-id.  It can occur 0 times and we aren't using it [cb]
  ;; BUGBUG (requester-id (xml-schema-objects:xs-string-val (xml-schema-objects:tns-run-instances-response-type-requester-id run-instances-response-type)))
  (let* ((request-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-run-instances-response-type-request-id (dive run-instances-response-type)))))
         (owner-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-run-instances-response-type-owner-id (dive run-instances-response-type)))))
         (reservation-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-run-instances-response-type-reservation-id (dive run-instances-response-type)))))
         (instances-set (xml-schema-objects:tns-run-instances-response-type-instances-set 
                         (dive run-instances-response-type)))
         (instance-descs (list)))
    (dolist (np (xml-schema-objects:tns-running-instances-set-type-item (dive instances-set)))
      (let ((dns-name (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-running-instances-item-type-dns-name np))))
            (instance-state 
              (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-instance-state-type-name
                    (dive (xml-schema-objects:tns-running-instances-item-type-instance-state np))))))
            (instance-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-running-instances-item-type-instance-id np)))))
        (push 
         (make-instance 'instance-desc 
                        :instance-id instance-id :dns-name dns-name 
                        :instance-state instance-state) instance-descs)))
    (make-instance 'reservation-info :owner-id owner-id :reservation-id reservation-id 
                   :instance-descs instance-descs )))

(defun describe-instances ()
  "API for describe instances"
  (let* ((xml-response 
          (read-response (make-simple-request "DescribeInstances" *cred*)))
         ;; response type here, after read
         ;; then get the request id as the first element of the response type
         ;; and then....
         ;; get reservation set
         (request-id (dive (xml-schema-objects:tns-describe-instances-response-type-request-id xml-response)))
         (reservation-set (dive (xml-schema-objects:tns-describe-instances-response-type-reservation-set 

                                 xml-response)))
         (reservation-infos (list)))
    (dolist (reservation (xml-schema-objects:tns-reservation-set-type-item reservation-set))
      (push (describe-instances-response-item-unbundler reservation) reservation-infos))
    reservation-infos))

(defclass security-group-info ()
  ((group-name :initarg :group-name :reader security-group-info-group-name)
   (owner-id :initarg :owner-id :reader security-group-info-owner-id)
   (group-description :initarg :group-description :reader security-group-info-group-description))
  (:documentation "useful information about security groups"))

(defmethod print-object ((obj security-group-info) stream)
  (with-slots (group-name owner-id group-description) obj
    (print-unreadable-object (obj stream :type t :identity t)
      (format stream "group-name \"~a\", owner-id \"~a\", group-description \"~a\"" 
              group-name owner-id group-description))))

(defun security-group-unbundler (security-groups-response-type)
  "distill security group information from xml-mapped response"
  (let* ((request-id (dive (xml-schema-objects:tns-describe-security-groups-response-type-request-id security-groups-response-type)))
         (sec-group-items
           (xml-schema-objects:tns-security-group-set-type-item
            (dive (xml-schema-objects:tns-describe-security-groups-response-type-security-group-info security-groups-response-type))))
         (items (list)))

    (dolist (sec-group sec-group-items)
      (let ((group-name (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-security-group-item-type-group-name sec-group))))
            (owner-id (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-security-group-item-type-owner-id sec-group))))
            (ip-permissions (dive (xml-schema-objects:tns-security-group-item-type-ip-permissions sec-group)))
            (group-description (xml-schema-objects:xs-string-val (dive (xml-schema-objects:tns-security-group-item-type-group-description sec-group)))))
        (push (make-instance 'security-group-info :owner-id owner-id :group-name group-name :group-description group-description) items)))
    items))
        
(defun describe-security-groups ()
  "API for describing security groups"
  (let* ((xml-response 
          (make-simple-request "DescribeSecurityGroups" *cred*)))
    (security-group-unbundler (read-response xml-response))))

(defun run-instances (image-id &optional (min-count 1) (max-count min-count))
  "API for running new instances"
  (let* ((params `(("ImageId" . ,image-id) 
                   ("MinCount" . ,(prin1-to-string min-count)) 
                   ("MaxCount" . ,(prin1-to-string max-count))))
         (xml-response 
          (make-simple-request "RunInstances" *cred* params)))
    (run-instances-response-item-unbundler (read-response xml-response))))
