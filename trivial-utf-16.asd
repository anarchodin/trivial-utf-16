(defpackage :trivial-utf-16-system
  (:use :cl :asdf))
(in-package :trivial-utf-16-system)

(defsystem "trivial-utf-16"
  :description "Basic functions to translate between UTF-32 and UTF-16."
  :author "Herbert Snorrason"
  :license "CC0"
  :components ((:file "package")
               (:file "trivial-utf-16")))
