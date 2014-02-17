;;;; -*- Mode: lisp; Syntax: Common-Lisp; indent-tabs-mode: nil; -*-
;;;;
;;;;
;;;; This is a Common Lisp implementation of ISO 8601 date and time routines.
;;;;

(defpackage :iso8601
  (:use :cl)
  (:export "FORMAT-ISO8601-TIME"
           "PARSE-ISO8601-TIME")
  (:documentation "An implementation of ISO 8601 date and time routines"))

;;;; eof
