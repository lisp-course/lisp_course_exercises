;;;
;;; Public license
;;; Author: Arthur Niedzwiecki
;;;
;;; This file describes your current assignment as a compilable project. Here you
;;; define all the components and dependencies of your code. You can load this
;;; so called 'asdf system' by using the following commands while the cursor is in the REPL:
;;; , r-l-s RET
;;; assignment_5 RETURN
;;; assignment-5 RETURN
;;;
;;; Remember that you can always switch from the current EMACS window (also called 'buffer') to your REPL buffer
;;; by using the following command chain:
;;; CTRL-b repl RETURN
;;;
;;; Now type the following command into your REPL:
;;; (in-package assignment-6)
;;;
;;; 

(defsystem assignment-6
  :author "artnie"

  :depends-on (bullet-wrapper)
  ;; The bullet-wrapper package is in the same repo as the assignments.
  ;; It will be compiled and accessable before compiling the assignment-5 (this) package.

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "oop-world" :depends-on ("package"))
     (:file "treasure-hunt" :depends-on ("package"
                                         "oop-world"))))))
