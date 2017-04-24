;;; -*- mode: lisp -*-
;;; lispjam-2017-04-temporal.asd (for lack of better name for now)

(asdf:defsystem #:lispjam-2017-04-temporal
  :serial t
  :long-name "Yet unnamed game for Lisp Game Jam 2017-04"
  :author "Jacek Złydach"
  :version (:read-file-form "version.lisp" :at (1 2 2))

  :description "A game."                ;TODO
  :long-description "TODO"              ;TODO
  :license "MIT"

  :homepage "https://github.com/TeMPOraL/lispjam-2017-04"
  :bug-tracker "https://github.com/TeMPOraL/lispjam-2017-04/issues"
  :source-control (:git "https://github.com/TeMPOraL/lispjam-2017-04.git")
  :mailto "temporal.pl@gmail.com"

  :encoding :utf-8

  ;;
  :depends-on (#:alexandria
               #:parendeck2d)

  ;;
  :components ((:file "packages")
               (:file "version")

               (:file "debug")

               (:file "proto")
               (:file "screens")

               (:file "main")))
