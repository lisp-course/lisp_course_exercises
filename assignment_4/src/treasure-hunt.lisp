;;; Welcome to Assignment 4 of the "Robot Programming with Lisp" course.
;;; 
;;; Your task is to imlement the methods in the section 'Method definition'.
;;; DO NOT use the LOOP macro for any solution, instead
;;; stick to higher-order functions, recursions and functional style in general.
;;;
;;; The robot will decide to always collect the treasure closest to him.

(in-package assignment-4)

;; This is an error condition, to be thrown when the robot is in collision.
(define-condition robot-collision (simple-error)
  ((actual-input :initarg :actual-input
                 :reader actual-input
                 :initform nil))
  (:report (lambda (condition stream)
             (format stream "~a is in collision!"
                     (actual-input condition)))))

;; Describes the offset of each direction. Use the orientation as a key to get
;; the corresponding offset like this:
;; (alexandria:assoc-value +directions+ :NORTH)
;; or
;; (cdr (assoc :NORTH +directions+))
(alexandria:define-constant +directions+
    '((:NORTH 1 0)
      (:SOUTH -1 0)
      (:EAST 0 -1)
      (:WEST 0 1)) :test 'equal)

;;;;;;;;;;;;;;;;;;;;;;;
;; Method definition ;;
(defgeneric move (robot x y &optional orientation)
  (:documentation "Moves the robot to the given x and y coordinates if possible.
  Changes the robot's orientation, if specified.
  Throws a ROBOT-COLLISION error if the robot is in collision.
  Use `mapcar' and `member' to check for possible collision with `obstacles'.
  Check for the usual constraints for coordinates and orientation.")
  (:method ((robot robot) x y &optional orientation)
    (let ((obstacles (append (walls (world robot))
                           (treasures (world robot)))))
    ;; TODO Implement
    )))

(defgeneric collect-treasure (robot)
  (:documentation "Collects the treasure laying in front of the robot, if there is any.
  Use `delete' or `delete-if' to modify the list of treasures in the world.")
  (:method ((robot robot))
    ;; TODO Implement
    ))

(defgeneric distance (entity1 entity2)
  (:documentation "Calculates the geometric distance between two entities.")
  (:method ((entity1 entity) (entity2 entity))
    ;; TODO Implement
    ))

(defgeneric closer (robot entity1 entity2)
  (:documentation "Returns the entity closer to the robot, using the `distance' method.")
  (:method ((robot robot) (obj1 entity) (obj2 entity))
    ;; TODO Implement
    ))

(defgeneric sort-treasures-by-distance (world)
  (:documentation "Sorts the treasure list of the world ascending with their distance to the robot.
  Use the function `sort' to sort the list of treasures.
  Example of sort:
    Given a list of strings called str-list.
    Sort the list ascending regarding each string's length:
      (sort str-list #'< :key #'length)
    The function used as :key must always provide a comparable number.
    Since the `sort' function is destructive, assign the returned value to the treasures list.
  Check for the `distance' between two entities.
  Use `alexandria:curry' to modify the `distance' method, such that it always takes the
  robot as first argument.")
  (:method ((world treasure-world))
    ;; TODO Implement
    ))

(defgeneric get-access-pose (treasure)
  (:documentation "Provides a robot's potential position to access the given treasure.
  Calling this method provides x y and orientation as values,
  that you can bind with `multiple-value-bind'.
  Check for a free place nearby the given treasure.
  Use the `world' slot to get the other walls and treasures in the world.
  The values are
  - The X coordinate of the free space.
  - The Y coordinate of the free space.
  - :NORTH :EAST :SOUTH or :WEST, the direction the robot has to look at to access the treasure.
    Check out `+directions+' at the top of this file. 
  If no place can be found, return NIL.
  
  HINT: Using this method is a quick check if the treasure is even accessible.
        It returns NIL if not, but evaluates to T if any place is found.")
  (:method ((treasure treasure))
    ;; TODO Implement
    ))

(defgeneric closest-accessible-treasure (world)
  (:documentation "Returns the treasure closest to the robot. The treasure must be reachable.
  From all `treasures' in the `world' reduce the list to the one, that is closest to the `robot'.
  But only check accessible treasures! (see HINT)
  To reduce the treasures to the closest one, use the function `reduce'.
  Modify the `closer' method with `alexandria:curry'.
  
  HINT: The function `remove-if' takes two arguments: a boolean function and a list.
        For each entry in the list `remove-if' applies the given function,
        and if the function evaluates positively, the entry is removed,
        BUT it does not change the original list.
        In other words: remove-if is non-destructive, unlike push or delete.")
  (:method ((world treasure-world))
    ;; TODO Implement
    ))

(defgeneric discover-world (world)
  (:documentation "Call `initialize-world' to get a world object.
  Use the methods you implemented above to make this method as short as possible.
  Use `multiple-value-bind' to bind the values of `get-access-pose' to local variables.
  Bind values directly from a callback function:
    (multiple-value-bind (x y ori) (get-access-pose treasure)
      (print (listx y ori)))
  Loop while there are still treasures in the world and collect them all.")
  (:method ((world treasure-world))
    (loop while (treasures world)
          do ;; TODO Implement
             (print "remove me")
          )))
;; Method definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method surroundings ;;
(defmethod move :before ((robot robot) x y &optional orientation)
  ;; Wait a moment.
  (declare (ignore orientation))
  (sleep 0.3))

(defmethod move :after ((robot robot) x y &optional orientation)
  (declare (ignore orientation))
  (visualize-simulation (world robot)))

(defmethod collect-treasure :before ((robot robot))
  ;; Wait a moment.
  (sleep 0.3))

(defmethod collect-treasure :after ((robot robot))
  (visualize-simulation (world robot)))
;; Method surroundings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
