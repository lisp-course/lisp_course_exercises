;;; Welcome to Assignment 3 of the "Robot Programming with Lisp" course.

;;; We are back with the grid world.
;;; Only this time there will be no global variables or other similar ugly things.
;;; Your task is to define classes to represent the structure of the world,
;;; initialize instances of classes and implement generic functions for them.
;;; We will represent our world in a very similar way to how they are represented
;;; in computer game physics engines, only, of course, much simpler.

;;; We are currently in the 'assignment-3' package. Look into the package.lisp
;;; and assignment-3.asd file to read more about packages and systems.
(in-package assignment-3)

;;; In this scenario there are TREASUREs laying around in the WORLD, again, and the ROBOT
;;; is still able to collect TREASUREs. But watch out for the WALLs! 

;;; Each description of a struct and class follows this format:
;;; -------------------------------------
;;;                NAME
;;; -------------------------------------
;;; slot-name | slot-type | initial-value
;;; ...
;;; -------------------------------------

;;; Our world is a 2D world inhabited by robots and obstacles.
;;; Robots and obstacles have positions in the world,
;;; so first define a structure called COORDINATE that will look like this:
;;;      -----------------
;;;          COORDINATE
;;;      -----------------
;;;       x | integer | 0
;;;       y | integer | 0
;;;      -----------------
;;; where x and y are the names of the slots, INTEGER is the type and 0 is the default value.

(defstruct coordinate
  ;; TODO: implement struct "coordinate"
  )

;;; Next is a representation of the world. It will be a class named TREASURE-WORLD:
;;;      -------------------------------------------
;;;                  TREASURE-WORLD
;;;      -------------------------------------------
;;;       robot     | robot      | NIL
;;;       walls     | list       | empty list
;;;       treasures | list       | empty list
;;;      -------------------------------------------
;;; ROBOT is a slot of type ROBOT (see definition of ROBOT below).
;;; WALLS is a list of WALL objects (also see below).
;;; TREASURES is a list of TREASURE objects (also see below).
;;; Each slot should have a getter and setter of the same name as the slot.
;;; Use the specifier ":type" to specify the type of the slot.
;;; Also, make sure you document each slot using the ":documentation" specifier.
;;; The class itself should also have a documentation.

(defclass treasure-world ()
  ;; TODO implement class
  ()
  (:documentation "TODO: documentation string"))

;;; Next is a parent class ENTITY and its child classes: WALL, TREASURE and ROBOT.
;;; Important: The accessor and initarg of the coordinate slot are coord and :coord. 
;;;      --------------------------------------------------------------------------------
;;;          ENTITY
;;;      --------------------------------------------------------------------------------
;;;       coord    | coordinate     | empty (default) coordinate
;;;       world    | treasure-world | NIL
;;;       name     | symbol         | NIL
;;;      --------------------------------------------------------------------------------
;;;
;;;      --------------------------------------------------------------------------------
;;;          WALL  (inherits ENTITY)
;;;      --------------------------------------------------------------------------------
;;;      --------------------------------------------------------------------------------
;;;
;;;      --------------------------------------------------------------------------------
;;;          TREASURE  (inherits ENTITY)
;;;      --------------------------------------------------------------------------------
;;;       color   | keyword | NIL (either :RED or :BLUE)
;;;      --------------------------------------------------------------------------------
;;;
;;;      --------------------------------------------------------------------------------
;;;          ROBOT   (inherits ENTITY)
;;;      --------------------------------------------------------------------------------
;;;       orientation | keyword           | :NORTH (either :NORTH, :EAST, :SOUTH or :WEST)
;;;      --------------------------------------------------------------------------------
;;;
;;; Don't forget the getters and setters and the doc strings.

;; TODO: Implement classes ENTITY, WALL, TREASURE and ROBOT.
(defclass entity ()
  ;; TODO: Implement class ENTITY
  ()
  (:documentation "TODO: docstring"))

;; TODO: complete class WALL
(defclass wall ()
  ()
  (:documentation "TODO: docstring"))

;; TODO: complete class TREASURE
(defclass treasure ()
  ()
  (:documentation "TODO: docstring"))

;; TODO complete class ROBOT
(defclass robot ()
  ()
  (:documentation "TODO: docstring"))

;; Now let's implement the methods to add subclasses of ENTITY to the TREASURE-WORLD.
;; The methods need two parameters, the entity object and the world object.
;; Implement the methods for each subclass of `entity': `wall', `treasure' and `robot'.
;; `wall': Append the wall object to the list of walls.
;; `treasure': If the coordinates are not occupied by a wall jet, add the treasure to the list.
;; `robot': If the coordinate within the object is free, set the robot in the world.

(defgeneric add-to-world (obj world)
  (:documentation "Returns NIL if the `obj' coordinates point to an invalid position.
Adds the `entity' object to the respective slot of the `world' object."))

;; TODO: Implement method `add-to-world' for the classes `wall', `treasure' and `robot'.

(defmethod add-to-world ((obj wall) (world treasure-world))
  (print "Add WALL to world.")
  ;; TODO
  )


(defun initialize-world ()
  "Tests the OOP world implementation.
This is already implemented. You can uncomment the code parts for spawning the walls,
treasures and the robot to test if those things work.

Initializes the simulation and a world and resets global variables (if you use some).
Fills it with walls with the coordinates provided by `wall-coords'.
Creates 1 robot with different randomly generated coordinates
in this 15x16 world, making sure they're not sitting on walls.
Creates 10 treasures with random coordinates and adds them to the world. 
Also launches the visualization.
You have two local varables here defined in the let block: `world' and `wall-coords'.
The `world' is the object to be filled with entities. The `wall-coord' contain the
coordinates of all walls. You will have to use `mapcar' to apply a function for each
of those coordinates. Fill the world with walls, treasures and a robot."
  ;; ----------------
  ;; As soon as you implemented the classes and coordinate struct above,
  ;; uncomment this function and remove this comment. The function
  ;; will not compile unless the classes are implemented. To uncomment multiple lines,
  ;; first select all the lines you want to uncomment. Start your selection
  ;; by pressing C-SPACE, then use your arrow keys up or down. Having the lines
  ;; selected, press M-; (or on a german keyboard M-SHIFT-,) to toggle the comment.
  ;; ----------------
  (btr-wrapper::init-world)
  (let* ((world (make-instance 'treasure-world))
        ;; wall-coords contains a list of all coordinates of the walls.
        (wall-coords (append
                       ;; the outer frame of walls
                       (loop for i to 14 
                             collect `(0 ,i)
                             collect `(14 ,(- 14 i))
                             collect `(,(- 14 i) 0)
                             collect `(,i 15))
                       ;; the room structure
                       '((0 15)
                         (1 1) (1 7) (1 11)
                         (2 1) (2 7) (2 11) (3 1) (3 2) (3 3) (3 5)
                         (3 6) (3 7) (3 8) (3 10) (3 11) (3 12) (3 13)
                         (5 1) (5 2) (5 3) (5 5) (5 6) (5 7) (5 8) (5 9)
                         (5 10) (5 12) (5 13) (5 14)
                         (6 3) (6 5) (6 9) (6 13) (6 14)
                         (7 3) (7 5) (7 9) (7 13) (7 14)
                         (8 9) (8 13) (8 14)
                         (9 3) (9 5) (9 9) (9 10) (9 11) (9 12) (9 13) (9 14)
                         (10 1) (10 2) (10 3) (10 5) (10 6) (10 7)
                         (10 8) (10 9) (10 10) (10 11) (10 12) (10 13) (10 14)
                         (11 3) (11 5) (11 9) (11 10) (11 11) (11 12) (11 13) (11 14)
                         (12 5) (12 9) (12 10) (12 11) (12 12) (12 13) (12 14)
                         (13 3) (13 9) (13 10) (13 11) (13 12) (13 13) (13 14)
                         (14 15))))
         (all-coords (alexandria:map-product 'list (alexandria:iota 15) (alexandria:iota 16)))
         (ground-coords (alexandria:shuffle (set-difference all-coords wall-coords :test 'equalp))))
    
    ;; UNCOMMENT, to spawn WALLS.
    ;; (mapcar (lambda (xy)
    ;;           (add-to-world (make-instance 'wall
    ;;                                        :name (intern (format nil "WALL~a-~a" (first xy) (second xy)))
    ;;                                        :coord (make-coordinate :x (first xy) :y (second xy))
    ;;                                        :world world) world)) wall-coords)

    ;; UNCOMMENT to spawn TREASURES.
    ;; (dotimes (i 10 t)
    ;;   (funcall (lambda (xy)
    ;;              (add-to-world (make-instance 'treasure
    ;;                                           :name (intern (format nil "TREASURE~a-~a" (first xy) (second xy)))
    ;;                                           :coord (make-coordinate :x (first xy) :y (second xy))
    ;;                                           :color (if (< i 5) :RED :BLUE)
    ;;                                           :world world) world)) (pop ground-coords)))
    
    ;; UNCOMMENT to spawn the ROBOT.
    ;; (let ((xy (pop ground-coords)))
    ;;   (add-to-world (make-instance 'robot
    ;;                                :name 'turtle1
    ;;                                :coord (make-coordinate :x (first xy) :y (second xy))
    ;;                                :orientation :NORTH
    ;;                                :world world) world))
    
   (visualize-simulation world)
    world))

(defgeneric visualize-simulation (world)
  (:documentation "Traverses through the the `walls' and `treasures' list, and the `robot',
and spawns all those entities at their respective `coordinate' in the simulation.
This implementation depends heavily on the class `world' to be implemented correctly.
Use this function to see, if you did something wrong in your setup of the world."))

(defmethod visualize-simulation ((world treasure-world))
  ; -----------------
  ;; Like for the 'initialize-world' function, uncomment the body of this function as
  ;; soon as your coordinate struct and the classes are implemented. To see
  ;; how it's done look at the comment in the 'initialize-world' function.
  ;; By compiling this function you can get hints about errors in your implementation.
  ;; -----------------

  
  ;; (unless btr-wrapper::*world-initialized*
  ;;   (btr-wrapper::init-world)
  ;;   (flet ((spawn-entity (entity)
  ;;            (with-slots (name coordinate) entity
  ;;              (btr-wrapper::spawn (slot-value coordinate 'x)
  ;;                                  (slot-value coordinate 'y)
  ;;                                  name
  ;;                                  (type-of entity)))))
  ;;     (with-slots (robot walls treasures) world
  ;;       (mapcar #'spawn-entity
  ;;               (append walls
  ;;                       treasures
  ;;                       (list robot))))
  ;;     (setf btr-wrapper::*world-initialized* t)))
  ;; (btr-wrapper::teleport-turtle (slot-value (coord (robot world)) 'x)
  ;;                                  (slot-value (coord (robot world)) 'y)
  ;;                                  (name (robot world))
  ;;                                  (orientation (robot world)))
  ;; (let ((treasure-coords (mapcar 'coord (treasures world))))
  ;;   (loop for x to 14
  ;;         do (loop for y to 15
  ;;                  do (unless (member (make-coordinate :x x :y y) treasure-coords :test 'equalp)
  ;;                       (when (btr:object btr:*current-bullet-world*
  ;;                                         (intern (format nil "TREASURE~a-~a" x y)))
  ;;                         (btr-utils:kill-object (intern (format nil "TREASURE~a-~a" x y))))))))
  ;;
  )
