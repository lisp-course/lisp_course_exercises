;;; This code will move the turtle through the world to collect the treasures.
;;; YOU ARE NOT ALLOWED TO USE THE 'LOOP' MACRO.
;;; TAKE ADVANTAGE OF HIGHER-ORDER FUNCTIONS INSTEAD.

;;; We are in the 'assignment-4' package.
(in-package assignment-4)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic definitions and documentations of the methods to implement. ;;
(defgeneric move (robot x y &optional orientation)
  (:documentation "Moves the robot to the given x and y coordinates if possible.
Changes the robot's orientation, if specified.
Throws a ROBOT-COLLISION error if the robot is in collision."))

(defgeneric collect-treasure (robot)
  (:documentation "Collects the treasure laying in front of the robot, if there is any."))

(defgeneric distance (entity1 entity2)
  (:documentation "Calculates the geometric distance between two entities."))

(defgeneric closer (robot entity1 entity2)
  (:documentation "Returns the entity closer to the robot, using the `distance' method."))

(defgeneric sort-treasures-by-distance (world)
  (:documentation "Sorts the treasure list of the world ascending with their distance to the robot."))

(defgeneric get-access-pose (treasure)
  (:documentation "Provides the robot's position to access the treasure with 3 values,
x y and the orientation."))

(defgeneric closest-accessible-treasure (world)
  (:documentation "Returns the treasure closest to the robot. The treasure must be reachable."))
;; Generic definition ;;
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


(defmethod distance ((entity1 entity) (entity2 entity))
  ;; See documentation in the generic method.

  ;; TODO Implement
  )

(defmethod closer ((robot robot) (obj1 entity) (obj2 entity))
  ;; See documentation in the generic method.

  ;; TODO Implement
  )

(defmethod move ((robot robot) x y &optional orientation)
  ;; See documentation in the generic function.
  ;; Prevent loops and code-duplication.
  ;; Use `mapcar' and `member' to check for possible collision with `obstacles'.
  ;; Check for the usual constraints for coordinates and orientation.

  ;; TODO Implement
  (let ((obstacles (append (walls (world robot))
                           (treasures (world robot)))))

    ))

(defmethod collect-treasure ((robot robot))
  ;; See documentation in the generic function.
  ;; Prevent code-duplication and loops.
  ;; Use `delete' or `delete-if' to modify the treasures list.
  
  ;; TODO Implement
  )

(defmethod sort-treasures-by-distance ((world treasure-world))
  ;; See documentation in the generic function.
  ;; Prevent code-duplication and loops.  
  ;; Use the function `sort' to to sort the list of treasures.
  ;; Example (sort): Given a list of strings 'str-list'.
  ;;                 Sort the list acsending with the string's length.
  ;;                 (sort str-list '< :key 'length)
  ;;                 The function provided after :key must always provide a comparable number
  ;;                 Since the `sort' function is destructive, assign the return value to the treasures list.
  ;; Use the `distance' method, checking the distance between two entities.
  ;; Use `alexandria:curry' to modify the `distance' method, so it always takes the robot as first argument.

  ;; TODO Implement
  )

(defmethod get-access-pose ((treasure treasure))
  ;; See documentation in the generic function.
  ;; Prevent code-duplication and loops.
  ;; Calling this method provides 3 values, that you can bind to variables using `multiple-value-bind'.
   ;; Check for a free place nearby the given treasure.
  ;; Use the `world' slot to get the other walls and treasures in the world.
  ;; The values are
  ;; - The X coordinate of the free space.
  ;; - The Y coordinate of the free space.
  ;; - The direction the robot has to look at, when accessing the treasure (keyword, see orientation of the robot).
  ;; If no place can be found, return NIL.
  
  ;; HINT: Using this method is a quick check if the treasure is even accessible. It returns NIL if not,
  ;; but evaluates to T if any place is found.

  ;; TODO Implement
  )

(defmethod closest-accessible-treasure ((world treasure-world))
  ;; From all `treasures' in the `world' reduce the list to the one, that is closest to the `robot'.
  ;; But only check accessible treasures! (see HINT)
  ;; To reduce the treasures to the closest one, use the function `reduce'.
  ;; Modify the `closer' method with `alexandria:curry'.
  
  ;; HINT: The function `remove-if' takes two arguments: a boolean function and a list.
  ;;       For each entry in the list `remove-if' applies the given function,
  ;;       and if the function evaluates positively, the entry is removed,
  ;;       BUT it does not change the original list.
  ;;      (In other words: remove-if is non-destructive, unlike push or delete.).

  ;; TODO Implement
  )

(defmethod discover-world ((world treasure-world))
  ;; Call `initialize-world' to get a world object.
  ;; Use the methods you implemented to get a brief implementation of `discover-world'.
  ;; Use `multiple-value-bind' to bind the outcome of `get-access-pose' to local variables.
  ;; This works:
  ;; (multiple-value-bind (x y ori) (get-access-pose treasure)
  ;;   (format nil "~a ~a ~a" x y ori))
  ;; This doesn't:
  ;; (let ((myposes (get-access-pose treasure)))
  ;;   (multiple-value-bind (x y ori) myposes
  ;;     (format nil "~a ~a ~a" x y ori)))
  ;; Loop while there are still treasures in the world.
  ;; Collect
  (loop while (treasures world)
        do ;; TODO Implement
           (print "remove me")
        )) 
