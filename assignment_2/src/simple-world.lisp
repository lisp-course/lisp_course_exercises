;;; Welcome to Assignment 2 of the "Robot Programming with Lisp" course.

;;; In this assignment you are a robot living in a 2D world.
;;; The world consist of ground, where you can stand, and walls, where you can not.
;;; From your current location you can fly anywhere but you can only land
;;; on the ground (not walls).
;;; There are little treasures spread around the world.
;;; Your goal is to collect all the treasures by teleporting the robot under the treasures.

(in-package assignment-2)

;;; Below is a skeleton code of the program. You can add new variables and
;;; new helper functions. The main function is DISCOVER-WORLD.
;;; Please implement all the functions containing a TODO.

(defvar *world-map*
  #2A((w w w w w w w w w w w w w w w w)
      (w w g g g g g w g g g w g g g w)
      (w w g g g g g w g g g w g g g w)
      (w w w w g w w w w g w w w w g w)
      (w g g g g g g g g g g g g g g w)
      (w w w w g w w w w w w g w w w w)
      (w g g w g w g g g w g g g w w w)
      (w g g w g w g g g w g g g w w w)
      (w g g g g g g g g w g g g w w w)
      (w g g w g w g g g w w w w w w w)
      (w w w w g w w w w w w w w w w w)
      (w g g w g w g g g w w w w w w w)
      (w g g g g w g g g w w w w w w w)
      (w g g w g g g g g w w w w w w w)
      (w w w w w w w w w w w w w w w w))
  "2D grid world.
The following symbols are used to represent things in the world:
- W for wall cell
- G for ground
- R for robot
- T for treasure.")

(defun initialize-walls ()
  "Cleans the *world-map* array and initializes the walls and the ground."
  (setf *world-map* (make-array '(15 16) :initial-element 'g))
  (loop for i from 0 to 14
        do (setf (aref *world-map* i 0) 'w)
           (setf (aref *world-map* i 15) 'w)
           (setf (aref *world-map* 0 i) 'w)
           (setf (aref *world-map* 14 i) 'w))
  (mapcar (lambda (x-y)
            (setf (aref *world-map* (first x-y) (second x-y)) 'w))
          '((0 15)
            (1 1) (1 7) (1 11)
            (2 1) (2 7) (2 11) (3 1) (3 2) (3 3) (3 5) (3 6) (3 7) (3 8) (3 10) (3 11) (3 12) (3 13)
            (5 1) (5 2) (5 3) (5 5) (5 6) (5 7) (5 8) (5 9) (5 10) (5 12) (5 13) (5 14)
            (6 3) (6 5) (6 9) (6 13) (6 14)
            (7 3) (7 5) (7 9) (7 13) (7 14)
            (8 9) (8 13) (8 14)
            (9 3) (9 5) (9 9) (9 10) (9 11) (9 12) (9 13) (9 14)
            (10 1) (10 2) (10 3) (10 5) (10 6) (10 7) (10 8) (10 9) (10 10) (10 11) (10 12) (10 13) (10 14)
            (11 3) (11 5) (11 9) (11 10) (11 11) (11 12) (11 13) (11 14)
            (12 5) (12 9) (12 10) (12 11) (12 12) (12 13) (12 14)
            (13 3) (13 9) (13 10) (13 11) (13 12) (13 13) (13 14)
            (14 15))))

(defconstant +battery-capacity+ 50
  "Power of the robots battery in digits.")

(defconstant +treasure-num+ 4
  "The number of treasures that exist inthe world.")

(defparameter *treasures* '()
  "The list of treasures in the world.
Call the function `reset-treasures-list' to reset the `*treasures*' list.
It will then contain 4 randomly chosen symbols from the list '(a b c d e f).")

(defun reset-treasures-list ()
  (let ((symbols '(a b c d e f)))
    (setf symbols (remove (nth (random (length symbols)) symbols) symbols))
    (setf symbols (remove (nth (random (length symbols)) symbols) symbols))
    (setf *treasures* (alexandria:shuffle symbols))))

(defvar *treasures-found* 0
  "Determines the current amount of treasures found by the robot.
Increase this whenever a treasure is collected.")

(defvar *battery-left* +battery-capacity+
  "Describes how much power is left in the robots battery.
Decrease this value by 1 for every move made.")

(defvar *robot-coords* '(nil nil)
  "List of length 2, containing the x and y coordinate of the robot.")

(defun place-object (x y symbol)
  "If the given coordinate (x y) contains ground, replace it with the `symbol'."
  ;; TODO implement
  )

(defun place-robot (x y)
  "Uses `place-object' to set the robot symbol 'r in the `*world-map*'
and updates the robot's coordinates `*robot-coords*'.
The robot can only be placed on the ground."
  ;; TODO Implement
  )

(defun initialize-world ()
  "This function initializes the 2D array map.
Write (initialize-world) and (visualize-simulation) to see the current world status.
Or just write *world-map* to see the 2D array.
1. It puts the symbols in the list `*treasures*' at random positions on the map.
If the treasure falls on a wall, the robot would not be able to collect it, so a wall stays a wall,
and you can only put treasures on the ground. After calling this function, there should be 4 symbols
in the world, a subset of the list '(a b c d e f).
2. It sets the robot on a random place on the ground in the `*world-map*'.
3. It resets all global variables so that one could play the treasure hunting game multiple
times in a row."
  (initialize-walls)
  (btr-wrapper::init-world)
  ;; ToDo: implement
  )


(defun symbol-at-pos (x y symbol)
  "Checks, if the symbol at position x and y is equal to the given symbol `symbol'."
  ;; TODO Implement
  )

(defun find-object-coordinates (symbol)
  "Searches the 2D `*world-map*' for the given `symbol' and returns the coordinates."
  ;; TODO Implement
  )

(defun move-robot (x y)
  "Flies the robot to the grid cell (x, y) in the world (indexing starts with 0).
If the cell is a wall or a treasure, anything except ground, the position is blocked
and nothing happens. Also, if `x' or `y' are outside the map boundaries, nothing should happen.
If the robot moves, it loses one battery charge. If the robot stands below a treasure,
you should incease the `*treasures-found*' counter.
This function will alter the world state (value of *world-map*).
Finally the simulation is updated."
  ;; ToDo: implement

  ;; Visualize
  (visualize-simulation))


(defun discover-world ()
  "Autonomously flies the robot in the world until all the treasure is collected.
The robot can only fly `+battery-capacity+' times.
The robot has access to the world state `*world-map*' and can read the coordinates of treasures.
Use `move-robot' to travel to each treasure, which collects the treasure above the robot."
  ;; ToDo: implement
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visualize the world ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun visualize-simulation ()
  "Visualizes the current 2D-Array world in the simulation.
Call this function, every time the simulation needs to be updated."
    (let ((x-max (- (first (array-dimensions *world-map*)) 1))
          (y-max (- (second (array-dimensions *world-map*)) 1)))
      (if (not btr-wrapper::*world-initialized*)
          (progn 
            (btr-wrapper::init-world)
            (loop for x to x-max
                  do (loop for y to y-max
                           unless (eq (aref *world-map* x y) 'g)
                             do (btr-wrapper::spawn x y nil (aref *world-map* x y) :red)))
            (setf btr-wrapper::*world-initialized* t))
          (progn (if (first *robot-coords*)
                     ;; Teleport the robot
                     (btr-wrapper::teleport-turtle (first *robot-coords*)
                                                   (second *robot-coords*)
                                                   'turtle1)
                     (warn "Please make sure, that *robot-coords* contains the x and y coordinate of the robot"))
                 (when (<= -2 (- +treasure-num+ *treasures-found*) +treasure-num+)
                   ;; Remove treasures if necessary
                   (loop for x to x-max
                         do (loop for y to y-max
                                  when (and (btr:object btr:*current-bullet-world*
                                                        (intern (format nil "TREASURE~a-~a" x y)))
                                            (eq 'g (aref *world-map* x y)))
                                    do (btr-utils:kill-object (intern (format nil "TREASURE~a-~a" x y))))))))))
