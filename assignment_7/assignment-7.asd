;;;
;;; Public license
;;; Maintainer: Arthur Niedzwiecki
;;;
;;; This file describes your current assignment as a compilable project. Here you
;;; define all the components and dependencies of your code. You can load this
;;; so called 'asdf system' by using the following commands while the cursor is in the REPL:
;;; , r-l-s RET
;;; assignment_7 RETURN
;;; assignment-7 RETURN
;;;
;;; Remember that you can always switch from the current EMACS window (also called 'buffer') to your REPL buffer
;;; by using the following command chain:
;;; CTRL-b repl RETURN
;;;
;;; Now type the following command into your REPL:
;;; (in-package assignment-5)
;;;
;;;

(asdf:defsystem assignment-7
  :author "gaya"
  
  :depends-on (roslisp
               actionlib
               actionlib_msgs-msg
               turtle_actionlib-msg
               geometry_msgs-msg
               std_msgs-msg
               turtlesim-srv
               cl-tf)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "turtlesim-action-client" :depends-on ("package"))
     (:file "turtlesim-tf" :depends-on ("package"))))))
