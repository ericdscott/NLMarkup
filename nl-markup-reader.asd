;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;; Copyright 2011 Eric D. Scott (http://EricDScott.com)
;; Distributed under MIT License
;; http://www.opensource.org/licenses/mit-license.php

(in-package :cl-user)

(defpackage :nl-markup-reader-asd
  (:use :cl :asdf))

(in-package :nl-markup-reader-asd)

(defsystem :nl-markup-reader
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
	       (:file "nl-markup-reader")
	       (:file "lexicon")
	       ))


