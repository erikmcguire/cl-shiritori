(defpackage :shiritori-system
  (:use :cl :asdf))

(in-package :shiritori-system)

(asdf:defsystem shiritori
  :version "1.0"
  :author "Erik McGuire"
  :licence "GPL 3.0"
  :description "Shiritori"
  :long-description "Common Lisp implementation of the Japanese word-chaining game, shiritori."
  :depends-on (:hunchentoot :cl-who)
  :serial t
  :components ((:file "package")
               (:file "globals")
               (:file "web-gui")
               (:file "loader")
               (:file "converters")
               (:file "responses")
              ))
