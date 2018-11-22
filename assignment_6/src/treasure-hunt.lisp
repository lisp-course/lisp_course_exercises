;;;
;;; Welcome to assignment 6 of the 'Robot Programming with LISP' course.
;;; Previously you implemented the depth-first-search algorith to find paths to trasures.
;;; Since DFS most likely delivers a really subotimal solution, we want to find an optimal path,
;;; meaning the shortest.
;;;
;;; The breadth-first-search algorithm and pseudocode can be found here:
;;; https://en.wikipedia.org/wiki/Breadth-first_search#Pseudocode
;;; Your task is to adapt the pseudocode to our problem.
;;;
;;; Further you will need to implement the robot's strategy to collect and deliver all treasures.
;;; Since we can now find the distance to each treasure and depot, you can think of something efficient. 
;;;

;;; Same as in the previous assignment 5:
;;; The 10 treasures are colored :RED and :BLUE. There are 2 `depots' in the world, one is
;;; :RED, the other one :BLUE. The `depots' are stored as a hash-table in the `treasure-world'.
;;; The keys to the depots in the hash-table are :RED and :BLUE. 
;;;
;;; The robot can carry up to 2 treasures in his `trunk'. 
;;;
;;; Your task is to bring the `treasures' to the `depot' of their respective color.
;;;

(in-package assignment-6)

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
(alexandria:define-constant +directions+
    '((:NORTH 1 0)
      (:EAST 0 -1)
      (:SOUTH -1 0)
      (:WEST 0 1)) :test 'equal)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic definitions ;;
(defgeneric move (robot x y &optional orientation)
  (:documentation "Moves the robot if `valid-move' approves. Also counts the total of moves made.
Throws a ROBOT-COLLISION error when attempting to hit a wall."))

(defgeneric valid-move (robot x y orientation)
  (:documentation "Checks the attempted move. The following operations are valid:
a) Stand still, meaning the given x y and orientation match the robot's current pose.
a) Change the orientation by 90 degree left or right, while x and y stay the same.
b) Take one step towards the current orientation without changing the orientation.
   (Use the function `forward' to check this).
In other words: either the robot stands still, turns left/right or makes a step forward."))

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
;; End generic definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method surroundings ;;
(defmethod move :after ((robot robot) x y &optional orientation)
  (declare (ignore orientation))
  (visualize-world (world robot)))

(defmethod collect-treasure :after ((robot robot))
  (visualize-world (world robot)))

(defmethod deposit-treasure :after ((robot robot))
  (visualize-world (world robot)))
;; End method surroundings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod valid-move ((robot robot) x y orientation)
  ;; See generic documentation
  "Checks if the robot is allowed to do the given move."
  (if (and (eq (coordinate-x (coord robot)) x)
           (eq (coordinate-y (coord robot)) y))
      (member orientation `(,(orientation robot)
                            ,(turn (orientation robot) :RIGHT)
                            ,(turn (orientation robot) :LEFT)))
      (when (eq (orientation robot) orientation)
        (multiple-value-bind (new-x new-y) (forward (coordinate-x (coord robot))
                                                    (coordinate-y (coord robot))
                                                    (orientation robot))
          (and (eq new-x (+ (coordinate-x (coord robot))
                            (second (assoc (orientation robot) +directions+))))
               (eq new-y (+ (coordinate-y (coord robot))
                            (third (assoc (orientation robot) +directions+)))))))))


(let ((steps 0))
  (defmethod move ((robot robot) x y &optional orientation)
    ;; See generic definition
    "Moves the robot to the given pose if the move is valid."
    (if (not (valid-move robot x y orientation))
        (warn "The action is invalid. Either turn the robot or move forward.")
        (if (valid-coord x y (world robot))
            (progn (incf steps)
                   (setf (coord robot) (make-coordinate :x x :y y))
                   (setf (orientation robot) orientation))
            (error (make-condition 'robot-collision
                                   :actual-input (name robot))))))
  (defun steps () steps))  


(defmethod collect-treasure ((robot robot))
  ;; See generic documentation
  "Collects treasure in front of robot into trunk, if there is any.
Throws `trunk-full' exception if trunk is full."
  (let ((trunk-slot (position nil (coerce (trunk robot) 'list))))
    (unless trunk-slot
      (error (make-condition 'trunk-full
                             :trunk (trunk robot))))
    (multiple-value-bind (x y) (forward (coordinate-x (coord robot))
                                        (coordinate-y (coord robot))
                                        (orientation robot))
      (let ((treasure (find-if (lambda (treasure)
                                 (equalp (make-coordinate :x x :y y) (coord treasure)))
                               (treasures (world robot)))))
        (setf (aref (trunk robot) trunk-slot) treasure)
        (setf (treasures (world robot))
              (delete treasure (treasures (world robot))))))))


(defmethod deposit-treasure ((robot robot))
  ;; See generic documentation
  "Removes all treasures from the trunk that match the color of
the depot in front of the robot."
  (multiple-value-bind (x y) (forward (coordinate-x (coord robot))
                                      (coordinate-y (coord robot))
                                      (orientation robot))
    (let* ((depot-found (find-if (lambda (dep) (equalp (make-coordinate :x x :y y)
                                                      (coord dep)))
                                 (alexandria:hash-table-values (depots (world robot))))))
      (when depot-found
        (when (aref (trunk robot) 0)
          (when (eq (color (aref (trunk robot) 0)) (color depot-found))
            (setf (aref (trunk robot) 0) nil)))
        (when (aref (trunk robot) 1)
          (when (eq (color (aref (trunk robot) 1)) (color depot-found))
            (setf (aref (trunk robot) 1) nil)))))))

(defmethod valid-coord (x y (world treasure-world))
  ;; See generic documentation
  (let ((obstacles (mapcar 'coord (append (walls world) (treasures world)
                                          (alexandria:hash-table-values (depots world))))))
    (and (< 0 x 15)
         (< 0 y 15)
         (not (member (make-coordinate :x x :y y) obstacles :test 'equalp)))))

(defmethod goal-reached (x y orientation (goal entity))
  ;; See generic documentation
  "Calculates the position in front of the given x, y and orientation.
Evaluates positively, if goal is at calculated pose."
  (multiple-value-bind (new-x new-y) (forward x y orientation)
    (equalp (make-coordinate :x new-x :y new-y) (coord goal))))

(defun turn (orientation direction)
  "Returns the orientation after turning left or right.
ORIENTATION is a keyword like those of a robot.
DIRECTION is either :LEFT or :RIGHT."
  (nth (mod (+ (position orientation (mapcar #'car +directions+))
               (case direction
                 (:RIGHT 1)
                 (:LEFT -1)
                 (otherwise 0)))
            4)
       (mapcar #'car +directions+)))

(defun forward (x y orientation)
  "Returns the position and orientation after moving into ORIENTATION direction.
Returns x, y and orienation as VALUES."
  (let ((xy-offset (alexandria:assoc-value +directions+ orientation)))
    (values (+ x (car xy-offset)) (+ y (cadr xy-offset)) orientation)))

(defun in-path (state path)
  "Checks if the given x, y and orientation is in the path,
where path is a list of (x y orientation) entries."
  (member state path :test 'equalp))

;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN ASSIGNMENT ;;

(defun expand (state)
  "Takes a `state' which consists of '(x y orientation) and returns
a list of three states: forward, turn left and turn right."
  ;; Example:
  ;; (expand '(1 2 :NORTH))
  ;; => '((2 2 :NORTH) (1 2 :EAST) (1 2 :WEST))
  ;; TODO Implement
  )

(defmethod optimal-path ((goal entity))
  "Takes an entity and returns the shortest path through breadth-first-search.
See pseudocode here: https://en.wikipedia.org/wiki/Breadth-first_search#Pseudocode"
  ;; TODO Implement
  )

(defmethod remove-unreachable-treasures ((world treasure-world))
  "Deletes treasures from the world, that are unreachable by `optimal-path'."
  ;; TODO Implement
  )

(defmethod discover-world ((world treasure-world))
  "Implements the robot's strategy for collecting and depositing the treasures in the world.
Calling this method with a fresh world will collect all treasures."
  ;; You can always pick the closest possible goal (treasure or depot).
  ;; Keep in mind, that you trunk can only fit two treasures,
  ;; so if the trunk is full, you need to unload before going to the next treasures.

  (remove-unreachable-treasures world)
  ;; TODO Implement
  (loop while (or (treasures world) 
                  (remove nil (coerce (trunk (robot world)) 'list))) 
        return "Do something while there are treasures in the world or in the trunk")
  )

;; END ASSIGNMENT ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN Test single paths ;;
(defgeneric follow-path (robot path)
  (:documentation "After calculating the path with find-path, move along the given path."))

(defmethod follow-path ((robot robot) path)
  (mapcar (lambda (step)
            (sleep 0.5)
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
