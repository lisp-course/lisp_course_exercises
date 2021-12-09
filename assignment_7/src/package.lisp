;;;
;;; Public license
;;; Maintainance: Arthur Niedzwiecki
;;;
;;; This file defines the package of the current assignment. It is compiled by loading
;;; the assignment-7.asd system. How to load the project in your EMACS, look into the
;;; comments inside the assignment-7.asd file. After loading the system you can switch
;;; into the assignment-7 package by typing the following command:
;;;
;;; (in-package assignment-7)
;;;
;;; Now you are able to execute any function defined in this assignments lisp files
;;; from within your REPL. 

(in-package cl-user)

(defpackage assignment-7
  (:nicknames :tut7)
  (:use :cl :roslisp :actionlib :cl-tf))
