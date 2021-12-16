;;;
;;; Public license
;;; Author: Arthur Niedzwiecki
;;;
;;; This file defines the package of the current assignment. It is compiled by loading
;;; the assignment-extra.asd system. How to load the project in your EMACS, look into the
;;; comments inside the assignment-extra.asd file. After loading the system you can switch
;;; into the assignment-extra package by typing the following command:
;;;
;;; (in-package assignment-extra)
;;;
;;; Now you are able to execute any function defined in this assignments lisp files
;;; from within your REPL. 

(in-package :cl-user)

(defpackage assignment-extra
  (:use #:common-lisp)
  (:nicknames :ass-extra)
  (:export))
