(in-package turtleref)

(defvar *transform-listener* nil)
(defvar *transform-publisher* nil)
(defvar *goal-picked-publisher* nil)

(defvar *active-goal-transforms* nil)

(defun finalize ()
  (ros-info finalize "Killing node")
  (setf *transform-listener* nil)
  (setf *transform-publisher* nil)
  (setf *goal-picked-publisher* nil)
  (setf *active-goal-transforms* nil)
  (when (eq (roslisp:node-status) :RUNNING)
    (roslisp:shutdown-ros-node)))


(defun init ()
  (finalize)
  (ros-info init "initializing node")
  (setf *active-goal-transforms* (copy-alist +rooms-list+))
  (let ((trash
          `((trash1 . ,(cl-tf:make-pose
                        (cl-tf:make-3d-vector 0.824278354645 -5.60038948059 0.00247192382812)
                        (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) (/ (- pi) 2))))
            (trash2 . ,(cl-tf:make-pose
                        (cl-tf:make-3d-vector 0.824278354645 -2.13942718506 0.00247192382812)
                        (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) (/ (- pi) 2))))
            (trash3 . ,(cl-tf:make-pose
                        (cl-tf:make-3d-vector 3.69784045219 1.37294614315 0.00247192382812)
                        (cl-tf:make-identity-rotation)))
            (trash4 . ,(cl-tf:make-pose
                        (cl-tf:make-3d-vector 7.37604141235 1.37294614315 0.00247192382812)
                        (cl-tf:make-identity-rotation))))))
    (setf *active-goal-transforms*
          (append (subseq (alexandria:shuffle +rooms-list+) 0 5)
                  trash)))
  (setf roslisp::*xmlrpc-timeout* 5.0)
  (roslisp:start-ros-node "referee")
  (sleep 1.0)
  (setf *transform-listener* (make-instance 'cl-tf:transform-listener))
  (setf *transform-publisher* (make-transform-broadcaster))
  (setf *goal-picked-publisher* (roslisp:advertise "goal_picked_up" "std_msgs/String"))
  (sleep 1.0))


(defun rooms->transforms (rooms)
  (mapcar (lambda (pose)
            (cl-tf:transform->transform-stamped
             *frame-id*
             (format nil "goal_~a" (car pose))
             0.0
             (cl-tf:pose->transform (cdr pose))))
          rooms))

(defun publish-goal-collect (goal-name)
 (publish *goal-picked-publisher* (make-msg "std_msgs/String" :data goal-name)))



(defun referee ()
  (init)
  ;; ensure the robot is published before continue
  (loop until (handler-case
                  (when (lookup-transform *transform-listener*
                                          "base_footprint"
                                          *frame-id*
                                          :time (roslisp:ros-time)
                                          :timeout 5)
                    (roslisp:ros-info referee "Robot's /base_footprint found!") T)
                (cl-tf:timeout-error ()
                  (roslisp:ros-warn referee "Waiting for robot to be published on TF.") NIL)))
  ;; main infinite loop of the referee
  (let ((active-goals-tmp (copy-alist *active-goal-transforms*))
        (treasures-in-trunk 0))

    (loop-at-most-every 0.5
      ;; (ros-info loop "~a goals left. ~a treasures in trunk"
      ;;           (+ (length *active-goal-transforms*) 1)
      ;;           treasures-in-trunk)

      ;; publish existing treasures
      (publish *transform-publisher*
               (transforms->tf-msg
                (append (rooms->transforms *active-goal-transforms*)
                        `(,*depot-transform*))))

      ;; loop through goal tf-frames and remove a goal when base_footprint is nearby
      (when (< treasures-in-trunk 2)
        (loop for room in *active-goal-transforms*
              for transform = (lookup-transform
                               *transform-listener*
                               "base_footprint"
                               (format nil "goal_~a" (car room))
                               :timeout 5.0) ; timeout to make sure listener got published tf
              do (when (and ;; (< treasures-in-trunk 2) ; already checked in the IF above
                        (< (v-dist (translation transform)
                                   (make-3d-vector 0 0 0))
                           1)
                        (< (angle-between-quaternions (rotation transform)
                                                      (make-identity-rotation))
                           (/ pi 4)))
                   (ros-info found "Goal ~a found." (car room))
                   (publish-goal-collect (format nil "goal_~a" (car room)))
                   (setf active-goals-tmp
                         (remove (assoc (car room) active-goals-tmp) active-goals-tmp))
                   (when (> (incf treasures-in-trunk) 1)
                     (ros-info full "Trunk is full.")))))

      ;; reset *active-goal-transforms* for the loop
      (setf *active-goal-transforms* (copy-alist active-goals-tmp))

      ;; clear counter, when nearby the depot
      (when (> treasures-in-trunk 0)
        (let ((transform (lookup-transform *transform-listener*
                                           "base_footprint"
                                           "goal_depot"
                                           :timeout 5.0)))
          (when (< (v-dist (translation transform) (make-3d-vector 0 0 0)) 1)
            (ros-info depot "Unloading all treasures.")
            (setf treasures-in-trunk 0)))))))


(defun referee-demo ()
  (referee))



(defun example-subscriber ()
  (unless (eq (roslisp:node-status) :RUNNING)
    (roslisp:start-ros-node "subscriber"))
  (subscribe "goal_picked_up"
             "std_msgs/String"
             (lambda (msg)
               (roslisp:with-fields (data) msg
                 (roslisp:ros-info goal-sub "Yeah, the treasure ~a was collected!" data)))))

;; (defun get-marker-publisher ()
;;   (unless *marker-publisher*
;;     (setf *marker-publisher*
;;           (roslisp:advertise "~location_marker" "visualization_msgs/Marker")))
;;   *marker-publisher*)

;; (defun publish-pose (pose &key (parent "map") id)
;;   (let ((point (cl-transforms:origin pose))
;;         (rot (cl-transforms:orientation pose))
;;         (current-index 0))
;;     (roslisp:publish (get-marker-publisher)
;;                        (roslisp:make-message "visualization_msgs/Marker"
;;                                              (std_msgs-msg:stamp header) 
;;                                              (roslisp:ros-time)
;;                                              (std_msgs-msg:frame_id header)
;;                                              (typecase pose
;;                                                (cl-tf:pose-stamped (cl-tf:frame-id pose))
;;                                                (t parent))
;;                                              ns "goal_locations"
;;                                              id (or id (incf current-index))
;;                                              type (roslisp:symbol-code
;;                                                    'visualization_msgs-msg:<marker> :arrow)
;;                                              action (roslisp:symbol-code
;;                                                      'visualization_msgs-msg:<marker> :add)
;;                                              (x position pose) (cl-transforms:x point)
;;                                              (y position pose) (cl-transforms:y point)
;;                                              (z position pose) (cl-transforms:z point)
;;                                              (x orientation pose) (cl-transforms:x rot)
;;                                              (y orientation pose) (cl-transforms:y rot)
;;                                              (z orientation pose) (cl-transforms:z rot)
;;                                              (w orientation pose) (cl-transforms:w rot)
;;                                              (x scale) 0.3
;;                                              (y scale) 0.3
;;                                              (z scale) 0.3
;;                                              (r color) 1.0
;;                                              (g color) 0.0
;;                                              (b color) 0.0
;;                                              (a color) 1.0))))




