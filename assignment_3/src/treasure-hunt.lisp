;;; This code will move the turtle through the world to collect the treasures.

;;; Please implement the 'oop-world' first, as you can't move anyhing that you can't see.
;;; As soon as the 'visualize-simulation' function makes your world instance appear in the simulation,
;;; you can start moving the robot around.

;;; We are in the 'assignment-3' package.
(in-package assignment-3)

;; TODO: Implement the custom error ROBOT-COLLISION that provides an error message.
;;       For its usage, see the MOVE method below.


;; The MOVE method is for robots only. Implement the teleportation of the robot here,
;; but this time you can not stand under treasures! Put the robot at the given x and y coordinates,
;; with the given orientation.
;; If the robot is about to collide with an obstacle, throw a ROBOT-COLLISION error defined above.

(defgeneric move (robot x y orientation)
  (:documentation "Moves the robot to the given x and y coordinates if possible.
Throws a ROBOT-COLLISION error if the robot is in collision."))

;; TODO: Implement the MOVE method for robot objects.

;; After each call of the MOVE function we want to visualize the world in the simulation again.
;; Overload the method MOVE so the VISUALIZE-SIMULATION method will be called :AFTER each call
;; of the MOVE method.
;; TODO: Overload the MOVE method.


;; TODO: Implement helper function `get-access-pose' for treasure objects.
(defgeneric get-access-pose (treasure)
  (:documentation "Returns a list containing x, y and orientation, from which a robot can access the treasure.
If no position can be found, return NIL."))



;; TODO: The COLLECT-TREASURE method is used by robot instances only.
;; It collects the treasure laying in front of it, meaning, the robot can only
;; collect a treasure, when the position right in front of the robot contains a treasure.
;; This method may change the TREASURES slot of the world.

(defgeneric collect-treasure (robot)
  (:documentation "Collects the treasure laying in front of the robot, if there is any."))


;; Like in the MOVE method, overload the COLLECT-TREASURE method, so the VISUALIZE-SIMULATION method
;; will be called :AFTER each call of the COLLECT-TREASURE method.
;; TODO: Overload the COLLECT-TREASURE method.


(defgeneric discover-world (world)
  (:documentation "Uses the `robot' in the `world' to `move's to each `treasure', to then `collect-treasure'.
The robot can `get-access-pose' to know, where to land to collect a treasure."))

(defmethod discover-world ((world treasure-world))
  ;; TODO: Collect all the TREASURES in the WORLD.
  ;; In contrast to assignment-2 you now have all the treasures as a list in a slot of the world instance.
  ;; You have no limitation by a battery.
  ) 
