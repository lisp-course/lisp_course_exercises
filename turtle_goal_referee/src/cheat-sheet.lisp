
;; This is a cheat sheet for some common CRAM Language functions.
;; It's a subset of the following tutorials:
;; http://cram-system.org/tutorials/beginner/controlling_turtlesim_2
;; http://cram-system.org/tutorials/beginner/simple_plans
;; http://cram-system.org/tutorials/beginner/failure_handling

;; Include the following packages in your asd file:
;; roslisp cram-language turtlesim-msg turtlesim-srv cl-transforms geometry_msgs-msg roslisp-utilities

;; start your rosnode with
;; (roslisp-utilities:startup-ros)
;; which is more generic and includes startup/shutdown configurations

;; Create a fluent variable
(defvar *turtle-pose* (cpl:make-fluent :name :turtle-pose) "current pose of turtle")
(defparameter *pose-sub* nil) ;; pose subscriber

(defun init ()
  (setf *pose-sub*
        (roslisp:subscribe "turtle1/pose"
                           "turtlesim/Pose"
                           #'pose-cb)))
;; Each message on the pose topic executes 'pose-cb with the message as parameter
(defun pose-cb (msg)
  "Callback for pose values. Called by the pose topic subscriber."
  (setf (cpl:value *turtle-pose*) msg))
;; *turtle-pose* is now a fluent variable after calling the init function

;; Use fluent values with 'fl-funcall and 'value

;; get values
(value *turtle-pose*)

;; call function on fluent
;; e.g. get x-value from the pose
(fl-funcall #'turtlesim-msg:x *turtle-pose*)
;; turtlesim-msg:x is the getter for the x value
;; on a normal Pose message you would just call (turtlesim-msg:x pose)
;; here you apply the getter on the fluent object, to get the x as another fluent 

;; react to the values in a fluent with 'wait-for
(wait-for (< (fl-funcall #'turtlesim-msg:x *turtle-pose*)
             5.0))
;; blocks the current process until the turtle's x is smaller that 5

;; Create a command publisher for the turtle
(defvar *cmd-vel-pub* nil "velocity commands ROS publisher")
(defun init-cmd-pub ()
  (init)
  (setf *cmd-vel-pub* (advertise "turtle1/cmd_vel"
                                 "geometry_msgs/Twist")))
;; Execute this after some idle time, when the publisher expired.
;; We use this function to move the turtle
(defun send-vel-cmd (lin ang)
  "Function to send velocity commands."
  (roslisp:publish *cmd-vel-pub*
                   ;; short syntax:
                   ;; (make-message "geometry_msgs/Twist" (:x :linear) lin (:z :angular) ang)
                   ;; more understandable syntax:
                   (roslisp:make-message "geometry_msgs/Twist"
                                         :linear (roslisp:make-msg "geometry_msgs/Vector3" :x lin)
                                         :angular (roslisp:make-msg "geometry_msgs/Vector3" :z ang))))

;; This function runs the turtle in a circle endlessly
(defun draw-circle ()
  (loop do
    (send-vel-cmd 3 (/ pi 5))
    (wait-duration 0.1)))

;; Connect it with the fluent condition and execute the movement and condition as a plan
;; This will not be executable out of the box, it's only an example
;; (cpl:pursue
;;   (wait-for (> (fl-funcall #'turtlesim-msg:x *turtle-pose*)
;;                5.0))
;;   (loop do 
;;     (send-vel-cmd 1 (/ pi 10))
;;     (wait-duration 0.1)))

;; Now move your turtle to the left side with your arrow keys.

;; You need to wrap the call above in a top-level-plan to make it work:
(cpl:top-level
  (cpl:pursue
    (cpl:wait-for (> (cpl:fl-funcall #'turtlesim-msg:x *turtle-pose*)
                     5.0))
    (draw-circle)))
;; Pursue will terminate, as soon as one of the functions terminate
;; Here it terminates when x is greater than 5,
;; meaning, when the turtle reaches the right half of the screen

;; Include a 'finally' statement, which executes a code block after the plan,
;; with 'unwind-protect
(cpl:top-level
  (unwind-protect
       (cpl:pursue
         (wait-for (> (fl-funcall #'turtlesim-msg:x *turtle-pose*)
                      5.0))
         (draw-circle))
    (send-vel-cmd 0 0)))
;; The second body of unwind-protect:
;; (send-vel-cmd 0 0)
;; will be executed immediately, when pursue terminates,
;; preventing the turtle to move after the condition is met (x > 5).


;; You can catch a condition and execute something, to recover from an error
;; Lets quickly define a condition
(define-condition out-of-bounds-error (cpl:simple-plan-failure) ())

;; And a function that checks the condition for fluents
(defun out-of-bounds (fluent-pose)
  (roslisp:with-fields (x y) (cpl:value fluent-pose)
    (not (and (< 0.5 x 10.5)
              (< 0.5 y 10.5)))))

;; With the following we will throw an error without handling it
(cpl:top-level
  (cpl:pursue
    (cpl:whenever ((cpl:fl-funcall #'out-of-bounds *turtle-pose*))
      (cpl:fail 'out-of-bounds-error)) ;; throws the error
    (draw-circle)))

;; Lets handle the error and try to recover
(cpl:top-level
 (cpl:with-failure-handling
     ((out-of-bounds-error (e)
       (declare (ignore e))
       (roslisp:ros-warn (ooberror) "Out of bounds!")
       (send-vel-cmd -1 0)
       (sleep 1)
       (cpl:retry)))
   (cpl:pursue
    (cpl:whenever ((cpl:fl-funcall #'out-of-bounds *turtle-pose*))
                  (cpl:fail 'out-of-bounds-error)) ;; throws the error
    (draw-circle))))

;; another example for whenever
(cpl:par
    (cpl:whenever (*fluent*)
      (print "oh fluent is true")
      (setf (cpl:value *fluent*) NIL))
    (loop for i from 1 to 1000000 do 
      (cpl:sleep 1.0)
      (setf (cpl:value *fluent*) T)))

;; execute plans in order with try-in-order. seq is like progn
(cpl:top-level
  (cpl:try-in-order
    (cpl:seq
      (print "first one")
      (cpl:fail "oh nooes"))
    (cpl:seq
      (print "second one")
      (cpl:fail "oh noes again"))
    (print "all good")
    (print "some other stuff")))

;; execute all in order
(cpl:top-level
  (cpl:try-each-in-order (goal '(goal-1 goal-2 goal-3))
    (print goal)
    (when (oddp (random 2))
      (print "failing")
      (cpl:fail "oh no"))))

;; try all
(cpl:top-level
  (cpl:try-all
    (cpl:seq
      (print "first one")
      (cpl:fail "oh nooes"))
    (cpl:seq
      (print "second one")
      (cpl:fail "oh noes"))
    (print "all good")
    (print "some other stuff")))

(cpl:top-level
  (cpl:with-retry-counters ((my-counter 3))
    (cpl:with-failure-handling

      ((cpl:simple-plan-failure (e)
         (print e)
         (cpl:sleep 1.0)
         (cpl:do-retry my-counter
           (cpl:retry))
         (return)))

    (print "oh im failing")
    (cpl:fail "here's a failure"))))
