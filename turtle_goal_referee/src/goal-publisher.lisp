(in-package turtleref)

;; (defparameter *marker-publisher* nil)
(defparameter *transform-listener* nil)
(defparameter *reached-goals* nil)

(defparameter *active-goal-transforms* (copy-alist +rooms-list+))

(defun init ()
  (setf *active-goal-transforms* (copy-alist +rooms-list+))
  (setf *transform-listener* nil)
  (setf *reached-goals* nil)
  (kill-tf-listener))
  

(defun kill-tf-listener ()
  (setf *transform-listener* nil))

(defun get-transform-listener ()
  (unless *transform-listener*
    (setf *transform-listener* (make-instance 'transform-listener)))
  *transform-listener*)

(defun rooms->transforms (rooms)
  (mapcar (lambda (pose)
            (cl-tf:transform->transform-stamped
             *frame-id* 
             (format nil "goal_~a" (car pose))
             0.0 
             (cl-tf:pose->transform (cdr pose))))
          rooms))

(defun publish-goal-collect (goal-name)
  (unless (eq (roslisp:node-status) :RUNNING)
    (roslisp:start-ros-node "publisher"))
  (let ((publisher (roslisp:advertise "goal_picked_up" "std_msgs/String")))
    (publish publisher (make-msg "std_msgs/String" data goal-name))))

(defun example-subscriber ()
  (unless (eq (roslisp:node-status) :RUNNING)
    (roslisp:start-ros-node "subscriber"))
  (subscribe "goal_picked_up" "std_msgs/String" 
             (lambda (msg) 
               (roslisp:with-fields (data) msg
                 (roslisp:ros-info goal-sub "Yeah, the treasure ~a was collected!" data)))))

(defun referee ()
  (kill-tf-listener)
  (with-ros-node ("referee")
    (get-transform-listener)
    (publish-goal-collect "test_init") ;; initialize publisher
    (sleep 1) 
    ;; ensure the robot is published before continue
    (loop until (handler-case 
                    (when (lookup-transform (get-transform-listener)
                                             "base_footprint"
                                             *frame-id*
                                             :time (roslisp:ros-time)
                                             :timeout 2)
                           (roslisp:ros-info referee "Robot's /base_footprint found!") T)
                  (cl-tf:timeout-error ()
                    (roslisp:ros-warn referee "Waiting for robot to be published on TF.") NIL)))

    
    (let ((active-goals-tmp (copy-alist *active-goal-transforms*))
          (treasures-in-trunk 0)
          (tf-broadcaster (make-transform-broadcaster)))
      (loop-at-most-every 0.51
        (cl-tf::with-tf-broadcasting-list (tf-broadcaster
                                           (append (rooms->transforms *active-goal-transforms*)
                                                   `(,*depot-transform*)))
          
          (sleep 0.3)
           ;; wait for updated tf
          ;; loop through goal tf-frames and stop publishing, when base_footprint is nearby
          (when (< treasures-in-trunk 2)
              (loop for room in *active-goal-transforms*
                    for transform = (lookup-transform (get-transform-listener)
                                                      "base_footprint"
                                                      (format nil "goal_~a" (car room))
                                                      :timeout 1)
                    when (and (< treasures-in-trunk 2)
                              (< (v-dist (translation transform) (make-3d-vector 0 0 0)) 1)
                              (< (angle-between-quaternions (rotation transform)
                                                            (make-identity-rotation)) (/ pi 4)))
                      do (ros-info found "Goal ~a found." (car room))
                         (publish-goal-collect (format nil "goal_~a" (car room)))
                         (setf active-goals-tmp
                               (remove (assoc (car room) active-goals-tmp) active-goals-tmp))
                         (when (> (incf treasures-in-trunk) 1)
                           (ros-info full "Trunk is full."))))

          ;; clear counter, when nearby the depot
          (when (> treasures-in-trunk 0)
            (let ((transform (lookup-transform (get-transform-listener)
                                               "base_footprint"
                                               "goal_depot"
                                               :timeout 1)))
              (when (< (v-dist (translation transform) (make-3d-vector 0 0 0)) 1)
                (ros-info depot "Unloading all treasures.")
                (setf treasures-in-trunk 0))))
                  
            (sleep 0.2)) ;; wait for safety
          (setf *active-goal-transforms* (copy-alist active-goals-tmp))))))

(defun referee-demo ()
  (let ((trash `((11 . ,(cl-tf:make-pose
                             (cl-tf:make-3d-vector 0.824278354645 -5.60038948059 0.00247192382812)
                             (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) (/ (- pi) 2))))
                 (22 . ,(cl-tf:make-pose
                             (cl-tf:make-3d-vector 0.824278354645 -2.13942718506 0.00247192382812)
                             (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) (/ (- pi) 2))))
                 (33 . ,(cl-tf:make-pose
                             (cl-tf:make-3d-vector 3.69784045219 1.37294614315 0.00247192382812)
                             (cl-tf:make-identity-rotation)))
                 (44 . ,(cl-tf:make-pose
                             (cl-tf:make-3d-vector 7.37604141235 1.37294614315 0.00247192382812)
                             (cl-tf:make-identity-rotation))))))
    (setf *active-goal-transforms*
          (append (subseq (alexandria:shuffle +rooms-list+) 0 5)
                  trash))
    (referee)))

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




