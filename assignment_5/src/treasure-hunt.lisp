;;;
;;; Welcome to assignment 5 of the 'Robot Programming with LISP' course.
;;; We are back in the simulation with our well known maze. In the previous assignments you
;;; teleported the robot to each treasure and collected it. This time is a bit more compilcated.
;;;
;;; The 10 treasures are colored :RED and :BLUE. There are 2 `depots' in the world, one is
;;; :RED, the other one :BLUE. The `depots' are stored as a hash-table in the `treasure-world'.
;;; The keys to the depots in the hash-table are :RED and :BLUE. 
;;;
;;; The robot can carry up to 2 treasures in his `trunk'. 
;;;
;;; Your task is to bring the `treasures' to the `depot' of their respective color.
;;; BUT you are not able to teleport anymore!
;;; You must find each path recursively and move step by step.
;;;

(in-package assignment-5)

;; Thrown when you try to put the robot at an occluded position.
(define-condition robot-collision (simple-error)
  ((actual-input :initarg :actual-input
                 :reader actual-input
                 :initform nil))
  (:report (lambda (condition stream)
             (format stream "~a is in collision!"
                     (actual-input condition)))))

;; Thrown when you try to pick up a treasure although the trunk is full.
(define-condition trunk-full (simple-error)
  ((trunk :initarg :trunk
                 :reader trunk
                 :initform nil))
  (:report (lambda (condition stream)
             (format stream "The trunk ~a is full!"
                     (trunk condition)))))

;; Describes the offset of each direction. Use the orientation as a key to get
;; the corresponding offset like this:
;; (alexandria:assoc-value +directions+ :NORTH)
;; or like this
;; (cdr (assoc :NORTH +directions+))
(alexandria:define-constant +directions+
    '((:NORTH 1 0)
      (:EAST 0 -1)
      (:SOUTH -1 0)
      (:WEST 0 1)) :test 'equal)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Generic definitions ;;
(defgeneric move (robot x y &optional orientation)
  (:documentation "Moves the robot if `valid-move' approves. Also counts the total of moves made.
Throws a ROBOT-COLLISION error when attempting to hit a wall."))

(defgeneric valid-move (robot x y orientation)
  (:documentation "Checks the attempted move. There are only two valid operations:
a) change the orientation while x and y are the robot's coordinates.
b) take one step towards the current orientation without changing the orientation.
In other words: either you turn or make a step forward."))

(defgeneric collect-treasure (robot)
  (:documentation "Collects the treasure laying in front of the robot, if there is any,
and puts it into a free slot in his trunk. Throws a TRUNK-FULL error when trying to
pick up a treasure although the trunk is full."))

(defgeneric deposit-treasure (robot)
  (:documentation "While standing in front of a depot, removes all treasures in the trunk
that match the color of the depot."))

(defgeneric valid-coord (x y world)
  (:documentation "Checks if the coordinates are within the maze boundaries and point to a free spot."))

(defgeneric goal-reached (x y orientation goal)
  (:documentation "Checks if the coordinates indicate, that a robot would look at the given goal (entity)."))

(defgeneric find-path (x y orientation goal &optional path)
  (:documentation "Recursively finds the path from the current x, y and orientation to the goal (entity).
Returns the path as a list of (x y orientation) entries. The path ends upon looking at the goal."))
;; END Generic definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; BEGIN visualize ;;
(defmethod move :after ((robot robot) x y &optional orientation)
  (declare (ignore orientation))
  (visualize-world (world robot)))

(defmethod collect-treasure :after ((robot robot))
  (visualize-world (world robot)))

(defmethod deposit-treasure :after ((robot robot))
  (visualize-world (world robot)))
;; END visualize ;;
;;;;;;;;;;;;;;;;;;;


(defmethod valid-move ((robot robot) x y orientation)
  ;; See generic documentation
  ;; TODO implement
  
  )

;;
;; MOVE 
;; TODO Embed the move method in a closure. Within this closure, increment a local variable
;;      for each successful move made. No defparameter, defvar etc. allowed, the variable
;;      must not be visible from the outside.
;; TODO Within the closure, alongside the move method, define a function that returns the
;;      value of the local movement counter.
;;
;; The goal is to monitor the amount of steps we made after taking a path, or multiple paths.
;; The variable resets with every compilation of the code.
;;
(defmethod move ((robot robot) x y &optional orientation)
  (if (not (valid-move robot x y orientation))
      (warn "The action is invalid. Either turn the robot or move forward.")
      (if (valid-coord x y (world robot))
          (progn (setf (coord robot) (make-coordinate :x x :y y))
                 (setf (orientation robot) orientation))
          (error (make-condition 'robot-collision
                                 :actual-input (name robot))))))

(defmethod collect-treasure ((robot robot))
  ;; See generic documentation
  ;; TODO implement
  
  )

(defmethod deposit-treasure ((robot robot))
  ;; See generic documentation
  ;; TODO implement
  
  )
    
;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Pathfinding ;;
(defmethod valid-coord (x y (world treasure-world))
  ;; See generic documentation
  ;; TODO implement
  
  )

(defmethod goal-reached (x y orientation (goal entity))
  ;; See generic documentation
  ;; TODO implement
  
  )

(defun turn (orientation direction)
  "Returns the orientation after turning left or right.
ORIENTATION is a keyword like the orientation of a robot.
DIRECTION is either :LEFT or :RIGHT."
  ;; TODO implement

  )

(defun forward (x y orientation)
  "Returns the position and orientation after moving into ORIENTATION direction.
Returns x, y and orienation as VALUES."
  ;; TODO implement
  
  )

(defun in-path (x y orientation path)
  "Checks if the given x, y and orientation is in the path,
where path is a list of (x y orientation) entries."
  ;; TODO implement
  
  )


(defmethod find-path (x y orientation (goal entity) &optional path)
  ;; See generic documentation
  ;; TODO implement
  
  )
  
;;
;; FIND-PATH extra
;; Embed the FIND-PATH method in a closure, like you did for the MOVE method, and define a local variable.
;; Increase the value of the variable every time a step hits a dead end.
;;
;; END Pathfinding ;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Test your code ;;
(defgeneric follow-path (robot path)
  (:documentation "After calculating the path with find-path, move along the given path."))

(defmethod follow-path ((robot robot) path)
  (mapcar (lambda (step)
            (sleep 0.2)
            (move robot (first step) (second step) (third step))) path))

(defgeneric test-path (world)
  (:documentation "Go to a random treasure in the world and put it to the right depot."))

(defmethod test-path ((world treasure-world))
  (let* ((treasure (nth (random (length (treasures world))) (treasures world)))
         (depot (gethash (color treasure) (depots world)))
         (treasure-path (find-path (coordinate-x (coord (robot world)))
                                   (coordinate-y (coord (robot world)))
                                   (orientation (robot world))
                                   treasure)))
    (when treasure-path
      (follow-path (robot world) treasure-path)
      (collect-treasure (robot world))
      (follow-path (robot world) (find-path (coordinate-x (coord (robot world)))
                                            (coordinate-y (coord (robot world)))
                                            (orientation (robot world))
                                            depot))
      (deposit-treasure (robot world)))))

(defun test-path-with-fresh-world ()
  (btr-wrapper::init-world)
  (test-path (initialize-world)))
;; END Test your code ;;
;;;;;;;;;;;;;;;;;;;;;;;;
