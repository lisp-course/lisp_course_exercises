;;; This is the object world we use for assignment 5.

(in-package assignment-5)

(defstruct coordinate
  (x 0 :type integer)
  (y 0 :type integer))

(defclass treasure-world ()
  ((robot :type robot
           :initform nil
           :initarg :robot
           :accessor robot)
   (walls :type list
          :initform (list)
          :initarg :walls
          :accessor walls)
   (treasures :type list
              :initform (list)
              :initarg :treasures
              :accessor treasures)
   (depots :type hash-table
           :initform (make-hash-table)
           :initarg :depots
           :accessor depots))
  (:documentation "The world as created by the almighty LISP coders."))

(defclass entity ()
  ((coordinate :type coordinate
               :initform (make-instance 'coordinate)
               :initarg :coordinate
               :accessor coord)
   (world :type treasure-world
          :initform NIL
          :initarg :world
          :accessor world)
   (name :type symbol
         :initform NIL
         :initarg :name
         :accessor name))
  (:documentation "A standard object in the world, containing its coordinate, name and the world it belongs to."))

(defclass wall (entity)
  ()
  (:documentation "An obstacle in the world only needs what the super-class provides."))

(defclass treasure (entity)
  ((color :type keyword
          :initform :RED
          :initarg :color
          :accessor color))
  (:documentation "Treasures have a color like :BLUE or :RED."))

(defclass robot (entity)
  ((trunk :type array
          :initform (make-array 2 :initial-contents '(nil nil))
          :initarg :trunk
          :accessor trunk)
   (orientation :type keyword
                :initform :NORTH
                :initarg :orientation
                :accessor orientation))
  (:documentation "A robot has orientation to :NORTH, :EAST, :SOUTH or :WEST and
room for two treasures in his trunk."))

(defclass depot (entity)
  ((color :type keyword
          :initform nil
          :initarg :color
          :accessor color))
  (:documentation "Depots have a color like :BLUE or :RED."))

(defun add-object-to-world (type world name coordinate
                            &key (orientation :NORTH) (trunk #(nil nil)) color)
  (let ((object-exists (btr:object btr:*current-bullet-world* name))
        (entity (make-instance type :name name
                                    :world world
                                    :coordinate coordinate
                                    :orientation orientation
                                    :trunk trunk
                                    :color color :allow-other-keys t)))
    (if (or object-exists
            (not (member type '(robot treasure wall depot)))) 
        (warn "Object with name ~a already exists in the world!" name)
        (case type
          (robot (setf (robot world) entity))
          (depot (setf (gethash name (depots world)) entity))
          (otherwise (let ((accessor (intern (concatenate 'string (write-to-string type) "S"))))
                       (setf (slot-value world accessor)
                             (append (slot-value world accessor) (list entity)))))))))

(alexandria:define-constant +wall-coords+
    (append (alexandria:map-product 'list '(0) (alexandria:iota 15))
            (alexandria:map-product 'list '(14) (alexandria:iota 14 :start 1))
            (alexandria:map-product 'list (alexandria:iota 15) '(15))
            (alexandria:map-product 'list (alexandria:iota 15) '(0))
            '((1 1) (1 7) (1 11)
              (2 1) (2 7) (2 11) (3 1) (3 2) (3 3) (3 5)
              (3 6) (3 7) (3 8) (3 10) (3 11) (3 12) (3 13)
              (5 1) (5 2) (5 3) (5 5) (5 6) (5 7) (5 8) (5 9)
              (5 10) (5 12) (5 13) (5 14)
              (6 3) (6 5) (6 9) (6 13) (6 14)
              (7 3) (7 5) (7 9) (7 13) (7 14)
              (8 9) (8 13) (8 14)
              (9 3) (9 5) (9 9) (9 10) (9 11) (9 12) (9 13) (9 14)
              (10 1) (10 2) (10 3) (10 5) (10 6) (10 7) (10 8) (10 9)
              (11 3) (11 5) (11 9)
              (12 5) (12 9)
              (13 3) (13 9))) :test 'equal)

(defun initialize-world (&optional (random-scene nil))
  "Initializes the simulation and world and resets global variables.
Fills it with walls with the coordinates of the grid world.
Also creates 1 robot in the 15x16 world.
Spawns 10 treasures.
Also launches the visualization."
  (btr-wrapper::init-world)
  (let* ((world (make-instance 'treasure-world))
         (scene0 '(((2 2) (8 12))
                   (1 3)
                   ((1 2) (1 4) (6 1) (9 2) (11 2) (13 1) (13 8) (11 8) (1 8) (12 12))))
         (scene1 '(((2 2) (8 12))
                   (1 3)
                   ((2 12) (1 2) (7 7) (6 1) (9 2) (11 2) (13 1) (13 8) (11 8) (12 12))))
         (scene2 '(((8 12) (2 2))
                   (1 3)
                   ((1 2) (7 7) (6 1) (9 2) (11 2) (13 1) (13 8) (11 8) (1 8) (12 12))))
         (scene (if random-scene
                    (case (random 3)
                      (0 scene0)
                      (1 scene1)
                      (2 scene2))
                    scene0)))
    (mapcar (lambda (xy)
              (add-object-to-world 'wall world (intern (format nil "WALL~a-~a" (first xy) (second xy)))
                                   (make-coordinate :x (first xy) :y (second xy))))
            +wall-coords+)
    (add-object-to-world 'depot world :RED
                         (make-coordinate :x (first (first (first scene))) :y (second (first (first scene))))
                         :color :RED)
    (add-object-to-world 'depot world :BLUE
                         (make-coordinate :x (first (second (first scene))) :y (second (second (first scene))))
                         :color :BLUE)
    (dotimes (i 10 t)
      (funcall (lambda (xy)
                 (add-object-to-world 'treasure world
                                      (intern (format nil "TREASURE~a-~a" (first xy) (second xy)))
                                      (make-coordinate :x (first xy) :y (second xy))
                                      :color (if (oddp i) :RED :BLUE)))
               (nth i (third scene))))
    (add-object-to-world 'robot world 'turtle1
                         (make-coordinate :x (first (second scene)) :y (second (second scene)))
                         :orientation :EAST :trunk #(nil nil))
    (visualize-world world)
    world))



(defmethod visualize-world ((world treasure-world))   
  (unless btr-wrapper::*world-initialized*
    (flet ((spawn-entity (entity)
             (with-slots (name coordinate) entity
               (btr-wrapper::spawn (slot-value coordinate 'x)
                 (slot-value coordinate 'y)
                 name
                 (type-of entity)
                 (when (or (eq (type-of entity) 'depot)
                           (eq (type-of entity) 'treasure))
                   (color entity))))))
      (with-slots (robot walls treasures depots) world
        (mapcar #'spawn-entity
                (append walls
                        treasures
                        (alexandria:hash-table-values depots)
                        (list robot))))
      (setf btr-wrapper::*world-initialized* t)))
  (let ((treasure-coords (mapcar 'coord (append (treasures world)
                                                (remove-if-not 'identity
                                                               (coerce (trunk (robot world)) 'list))))))
    (loop for x to 14
          do (loop for y to 15
                   do (unless (member (make-coordinate :x x :y y) treasure-coords :test 'equalp)
                        (when (btr:object btr:*current-bullet-world*
                                          (intern (format nil "TREASURE~a-~a" x y)))
                          (btr-utils:kill-object (intern (format nil "TREASURE~a-~a" x y))))))))
  (btr-wrapper::teleport-turtle (coordinate-x (coord (robot world)))
                                (coordinate-y (coord (robot world)))
                                (name (robot world))
                                (orientation (robot world))
                                (when (aref (trunk (robot world)) 0)
                                  (name (aref (trunk (robot world)) 0)))
                                (when (aref (trunk (robot world)) 1)
                                  (name (aref (trunk (robot world)) 1))))) 
