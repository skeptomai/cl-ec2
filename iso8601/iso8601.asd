;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; The iso8601 ASDF system definition
;;;;

(defsystem iso8601
  :name "ISO8601"
  :author "Thomas Russ tar AT ISI.EDU"
  :version "1"
  :maintainer "Christopher Brown <skeptomai@mac.com>"
  :licence "Apache License"
  :description "ISO 8601 time and date routines"
  :long-description "ISO 8601 time and date routines"

  :components
  ((:module
    :src 
    :components ((:file "package")
                 (:file "iso8601" :depends-on ("package"))))))

;;;; eof
