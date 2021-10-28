;;;
;;; Copyright (c) 2017, Arthur Niedzwiecki <niedzwiecki@uni-bremen.de>
;;;                     Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors 
;;;       may be used to endorse or promote products derived from this software 
;;;       without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :btr-wrapper)

(defparameter *wall-id* 0)
(defparameter *treasure-id* 0)
(defparameter *turtle-id* 0)
(defparameter *world-initialized* nil)

(defun init-world ()
  (setf *wall-id* 0)
  (setf *treasure-id* 0)
  (setf *turtle-id* 0)
  (setf *world-initialized* nil)
  (setf btr:*current-bullet-world* (make-instance 'btr:bt-reasoning-world))
  (let ((mesh-keys (map 'list #'first btr::*mesh-files*)))
    (unless (and (member :turtle mesh-keys)
                 (member :wall mesh-keys)
                 (member :treasure mesh-keys))
      (add-objects-to-mesh-list)))
  (prolog:prolog '(and
                   (btr:bullet-world ?w)
                   (btr:debug-window ?w)
                   (assert (btr:object ?w :static-plane floor ((0.5 0.5 -.02) (0 0 0 1))
                            :normal (0 0 1) :constant 0)))))

(defun spawn (x y name type &optional (color '(0.2 0.2 0.2)))
  (case (alexandria:make-keyword type)
    ((:robot :r) (spawn-turtle x y name))
    ((:wall :w) (spawn-wall x y (intern (format nil "WALL~a-~a" x y))))
    ((:treasure :t :a :b :c :d :e :f) (spawn-treasure x y (intern (format nil "TREASURE~a-~a" x y)) color))
    (:depot (spawn-depot x y name color))
    (otherwise (warn "~a is no known object-type." type))))
    

(defun spawn-turtle (x y name)
  (let ((turtle-name (if name
                         name
                         (intern (concatenate 'string
                                              "TURTLE" 
                                              (write-to-string (incf *turtle-id*)))))))
  (prolog:prolog `(and
                   (btr:bullet-world ?w)
                   (assert (btr:object ?w :mesh ,turtle-name ((,x ,y 0) (0 0 0 1))
                                       :mass 0.2 :color (0.3 0.5 0.3) :mesh :turtle))))))

(defun spawn-wall (x y name)
  (let ((wall-name (if name
                       name
                       (intern (concatenate 'string
                                            "WALL" 
                                            (write-to-string (incf *wall-id*)))))))
    (prolog:prolog `(and
                     (btr:bullet-world ?w)
                     (assert (btr:object ?w :mesh ,wall-name ((,x ,y 0) (0 0 0 1))
                                         :mass 0.2 :color (0.2 0.2 0.2) :mesh :wall))))))
(defun spawn-treasure (x y name &optional color)
  (let ((color-values (case color
                        (:blue '(0 0 1))
                        (:red '(1 0 0))
                        (otherwise '(0 0 0))))
        (treasure-name (if name
                           name
                           (intern (concatenate 'string "TREASURE"
                                                (write-to-string (incf *treasure-id*)))))))
    (prolog:prolog `(and
                     (btr:bullet-world ?w)
                     (assert (btr:object ?w :mesh ,treasure-name ((,x ,y 1) (0 0 0 1))
                                         :mass 0.2 :color ,color-values :mesh :treasure))))))

(defun spawn-depot (x y name color)
  (let ((color-values (case color
                        (:blue '(0 0 1))
                        (:red '(1 0 0))
                        (otherwise '(0 0 0))))
        (depot-name (if name
                        name
                        (intern (concatenate 'string (symbol-name color) "DEPOT")))))
    (prolog:prolog `(and
                     (btr:bullet-world ?w)
                     (assert (btr:object ?w :mesh ,depot-name ((,x ,y -0.8) (0 0 0 1))
                                         :mass 0.2 :color ,color-values :mesh :wall))))))

(defun teleport-turtle (x y turtle-name &optional orientation treasure-name1 treasure-name2) ;;                   <<<<<<<<<< fix getting the treasure names and failure handling
  "Move the turtle to the given x and y coordinate on the map."
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let ((world-copy (btr::copy-world btr:*current-bullet-world*))
              (orientation-array (if orientation
                                     (case orientation
                                       (:NORTH '(0 0 0 1))
                                       (:SOUTH '(0 0 1 0))
                                       (:EAST '(0 0 -0.707d0 0.707d0))
                                       (:WEST '(0 0 0.707d0 0.707d0)))
                                     (with-slots (cl-tf:w cl-tf:x cl-tf:y cl-tf:z)
                                         (cl-tf::orientation (btr:pose turtle))
                                       (list cl-tf:x cl-tf:y cl-tf:z cl-tf:w)))))
          (prolog:prolog
           `(and (assert (btr:object-pose ,world-copy ,turtle-name ((,x ,y 0) (0 0 0 1))))))
          (if (not (is-turtle-in-collision turtle-name world-copy))
              ;; Seperating possible movement of treasures from robot, to prevent warnings.
              (when (progn (when treasure-name1
                             (prolog:prolog
                              `(and (btr:bullet-world ?w)
                                    (assert (btr:object-pose ?w ,treasure-name1
                                                             ((,x ,(+ y 0.085) 0.37) (0 0 0 1)))))))
                           (when treasure-name2
                             (prolog:prolog
                              `(and (btr:bullet-world ?w)
                                    (assert (btr:object-pose ?w ,treasure-name2
                                                             ((,x ,(- y 0.085) 0.37) (0 0 0 1)))))))
                           
                           (prolog:prolog
                            `(and (btr:bullet-world ?w)
                                  (assert (btr:object-pose ?w ,turtle-name
                                                           ((,x ,y 0) ,orientation-array))))))
                (format nil "Successfully moved the turtle to x:~a y:~a." x y))
              (warn "BANG! Collision!")))
        (warn "Robot with name ~a not found in the bullet world." turtle-name))))

(defun move-turtle (turtle-name &optional (forwards t))
  "Move the turtle of given `turtle-name' forwards. Can be moved backwards, by setting `forwards' to NIL."
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let* ((pose-offset (if forwards
                                (get-turtle-direction turtle-name)
                                (map 'list (lambda (coord) (* -1 coord)) (get-turtle-direction turtle-name))))
               (new-pose (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                             (cl-tf:origin (btr:object-pose turtle-name))
                           (list (+ cl-tf:x (first pose-offset)) (+ cl-tf:y (second pose-offset)) cl-tf:z)))
               (orientation (with-slots (cl-tf:w cl-tf:x cl-tf:y cl-tf:z)
                                (cl-tf::orientation (btr:pose turtle))
                              (list cl-tf:x cl-tf:y cl-tf:z cl-tf:w)))
               (world-copy (btr::copy-world btr:*current-bullet-world*)))
          (prolog:prolog
             `(and (assert (btr:object-pose ,world-copy ,turtle-name (,new-pose (0 0 0 1))))))
          (if (not (is-turtle-in-collision turtle-name world-copy))
              (when (prolog:prolog
                      `(and (btr:bullet-world ?w)
                            (assert (btr:object-pose ?w ,turtle-name (,new-pose ,orientation)))))
                     (format t "Successfully moved the turtle to x:~a y:~a." (first new-pose) (second new-pose)))
              (warn "BANG! Collision!")))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun get-turtle-direction (turtle-name)
  (let ((z-orientation (cl-tf::z (cl-tf::orientation (btr:pose (btr:object btr:*current-bullet-world* turtle-name))))))
    (alexandria:switch (z-orientation :test #'<)
      (-0.7d0 '(0 -1)) ;; East
      (0.1d0 '(1 0)) ;; North
      (0.71d0 '(0 1)) ;; West
      (otherwise '(-1 0))))) ;;South

(defun turn-turtle (turtle-name direction)
  "The direction is either :left or :right."
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (flet ((change-angle-upon-direction (angle repair-angle)
                 (when repair-angle
                   (setf angle (* -1 angle)))
                 (case direction
                    (:left (+ angle (* pi 0.5d0)))
                    (:right (- angle (* pi 0.5d0)))
                    (otherwise angle))))
          (let* ((turtle-pose
                   (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                       (cl-tf:origin (btr:object-pose turtle-name))
                     (list cl-tf:x cl-tf:y cl-tf:z)))
                 (turtle-orientation
                   (cl-tf::orientation (btr:pose turtle)))
                 (new-orientation
                   (with-slots (cl-tf:w cl-tf:x cl-tf:y cl-tf:z)
                       (cl-tf:axis-angle->quaternion
                        #(0 0 1)
                        (change-angle-upon-direction
                         (second (multiple-value-list
                                  (cl-tf:quaternion->axis-angle turtle-orientation)))
                         (< (slot-value turtle-orientation 'cl-tf:z) 0)))
                     (list cl-tf:x cl-tf:y cl-tf:z cl-tf:w))))
            (prolog:prolog `(and (btr:bullet-world ?w)
                                 (assert (btr:object-pose ?w
                                                          ,turtle-name
                                                          (,turtle-pose ,new-orientation)))))))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun turn-turtle-into-direction (turtle-name direction)
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let ((orientation (case direction
                             (:NORTH '(0 0 0 1))
                             (:SOUTH '(0 0 1 0))
                             (:EAST '(0 0 -0.707d0 0.707d0))
                             (:WEST '(0 0 0.707d0 0.707d0))))
              (turtle-pose
                   (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                       (cl-tf:origin (btr:object-pose turtle-name))
                     (list cl-tf:x cl-tf:y cl-tf:z))))
          (prolog:prolog `(and (btr:bullet-world ?w)
                                 (assert (btr:object-pose ?w
                                                          ,turtle-name
                                                          (,turtle-pose ,orientation)))))))))

(defun collect-treasure (turtle-name)
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let* ((pose-offset (get-turtle-direction turtle-name))
              (treasure-pose (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                                 (cl-tf:origin (btr:object-pose turtle-name))
                               (list (+ cl-tf:x (first pose-offset)) (+ cl-tf:y (second pose-offset)) cl-tf:z)))
              (world-copy (btr::copy-world btr:*current-bullet-world*)))
          (prolog:prolog
             `(and (assert (btr:object-pose ,world-copy ,turtle-name (,treasure-pose (0 0 0 1))))))
          (if (is-turtle-in-collision turtle-name world-copy :treasure)
              (mapcar #'delete-object (get-collision-objects turtle-name world-copy))
              (warn "No treasure to collect. Does the turtle look into the right direction?")))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun get-object-in-view (turtle-name)
  (let ((turtle (btr:object btr:*current-bullet-world* turtle-name)))
    (if turtle
        (let* ((pose-offset (get-turtle-direction turtle-name))
              (poi-pose (with-slots (cl-tf::x cl-tf::y cl-tf::z)
                                 (cl-tf:origin (btr:object-pose turtle-name))
                               (list (+ cl-tf:x (first pose-offset)) (+ cl-tf:y (second pose-offset)) cl-tf:z)))
              (world-copy (btr::copy-world btr:*current-bullet-world*)))
          (prolog:prolog
             `(and (assert (btr:object-pose ,world-copy ,turtle-name (,poi-pose (0 0 0 1))))))
          (get-collision-objects turtle-name world-copy))
        (warn "Robot with name ~a not found n the bullet world." turtle-name))))

(defun carry-treasure (turtle-name treasure-name)
  (let ((treasures-carrying (length (cut:force-ll
                                     (prolog:prolog `(and (btr:bullet-world ?w)
                                                          (btr:contact ?w ,turtle-name ?x)))))))
    (if (< treasures-carrying 2)
        (when (prolog:prolog `(and (btr:bullet-world ?w)
                                   (btr:object-pose ?w ,turtle-name ?x)
                                   (assert (btr:object-pose-on ?w ,treasure-name ?x))))
          (format t "Successfully collected treasure ~a!" treasure-name))
        (warn "Something went wrong collecting the treasure!~%
        The turtle already carries ~a objects." treasures-carrying))))

(defun is-turtle-in-collision (turtle-name &optional (world-to-check btr:*current-bullet-world*) (object-type :wall))
  (let ((collision-object-types
          (mapcar (lambda (collision-name)
                    (btr::item-types (btr:object world-to-check collision-name))) 
                  (get-collision-objects turtle-name world-to-check))))
    (member object-type (alexandria:flatten collision-object-types))))

(defun get-collision-objects (turtle-name &optional (world-to-check btr:*current-bullet-world*))
  (mapcar (lambda (collision) 
            (alexandria:assoc-value collision 'bullet-wrapper::?x)) 
                  (cut:force-ll (prolog:prolog `(and (btr:contact ,world-to-check ,turtle-name ?x))))))

(defun get-object-at-position (x y)
  (let ((world (btr::copy-world btr:*current-bullet-world*)))
    (prolog:prolog `(and
                     (assert (btr:object ,world :mesh test-object ((,x ,y 0.0) (0 0 0 1))
                                         :mass 0.2 :color (0.3 0.5 0.3) :mesh :treasure))))
    (get-collision-objects 'test-object world)))
           

(defun delete-object (object-name)
  (btr-utils:kill-object object-name))

(defun add-objects-to-mesh-list (&optional (ros-package "bullet_wrapper"))
  (mapcar (lambda (object-filename-and-object-extension)
            (declare (type list object-filename-and-object-extension))
            (destructuring-bind (object-filename object-extension)
                object-filename-and-object-extension
              (let ((lisp-name (roslisp-utilities:lispify-ros-underscore-name
                                object-filename :keyword)))
                (pushnew (list lisp-name
                               (format nil "package://~a/resource/~a.~a"
                                       ros-package object-filename object-extension)
                               nil)
                         btr::*mesh-files*
                         :key #'car)
                lisp-name)))
          (mapcar (lambda (pathname)
                    (list (pathname-name pathname) (pathname-type pathname)))
                  (directory (physics-utils:parse-uri
                              (format nil "package://~a/resource/*.*" ros-package))))))

