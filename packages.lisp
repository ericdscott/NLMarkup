;; Copyright 2011 Eric D. Scott (http://EricDScott.com)
;; Distributed under MIT License
;; http://www.opensource.org/licenses/mit-license.php

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :agraph))

(defpackage :natural-lexicon.markup-reader
  (:use :common-lisp :db.agraph.user :triple-store :split-sequence
   )
  (:export :discourse-model
	   :db-path-of
	   :uri-of
	   :quickname-of
	   :sentences-of
	   :declarations-of
	   :annotations-of
	   :*discourse*
	   :graph-id-of
	   :initialize-discourse
	   :enable-natural-language-markup
	   :register-namespace
	   :*add-triple-hook*
	   :declare-triples
	   :with-discourse-db
	   ))


(defpackage :natural-lexicon.lexicon
  (:use :common-lisp :db.agraph.user :triple-store :split-sequence :natural-lexicon.markup-reader))
