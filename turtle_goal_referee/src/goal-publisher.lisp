(in-package turtleref)

;; (defparameter *marker-publisher* nil)
;; (defparameter *transform-publisher* nil)
(defparameter *transform-listener* nil)
(defparameter *frame-id* "world")
(defparameter *reached-goals* nil)

(defparameter *active-goal-transforms* (copy-alist +rooms-list+))

(defun kill-tf-listener ()
  (setf *transform-listener* nil))

;;(roslisp-utilities:register-ros-cleanup-function kill-tf-listener)

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

(defun referee ()
  (kill-tf-listener)
  (with-ros-node ("referee")
    (get-transform-listener)
    (sleep 1) ;; give the listener some time
    (let ((active-goals-tmp (copy-alist *active-goal-transforms*)))
      (loop-at-most-every 0.51
        (cl-tf::with-tf-broadcasting-list ((make-transform-broadcaster)
                                           (rooms->transforms *active-goal-transforms*))
          (sleep 0.3) ;; wait for updated tf
          (setf active-goals-tmp
                (remove-if (lambda (room)
                             (let ((transform (lookup-transform (get-transform-listener)
                                                                "turtle1"
                                                                (format nil "goal_~a" (car room)))))
                               (< (v-dist (translation transform) (make-3d-vector 0 0 0)) 1)))
                           active-goals-tmp))
          (sleep 0.2)) ;; wait for safety
        (setf *active-goal-transforms* (copy-alist active-goals-tmp))))))

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




