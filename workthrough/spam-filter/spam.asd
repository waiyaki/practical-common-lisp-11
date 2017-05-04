(defpackage :waiyaki.spam-system (:use :asdf :cl))

(in-package :waiyaki.spam-system)

(defsystem waiyaki-spam
  :name "spam"
  :components
  ((:file "packages")
   (:file "spam" :depends-on ("packages")))
  :depends-on (:cl-ppcre :pathnames))
