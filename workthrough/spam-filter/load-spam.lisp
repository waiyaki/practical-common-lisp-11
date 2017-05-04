;;;; Load the files required for testing the spam filter on SLIME
;;;; Probably beats the purpose of using asdf, but asdf beats me. 😓
(setf asdf:*central-registry*
      '(*default-pathname-defaults*
        #p"./"
        #p"../../practicals/Chapter15/"
        #p"../libraries/cl-ppcre-1.2.3/"))

(load "../../practicals/Chapter15/packages.lisp")
(load "./spam.asd")
(load "./packages.lisp")

(asdf:operate 'asdf:load-op 'Waiyaki-spam)
