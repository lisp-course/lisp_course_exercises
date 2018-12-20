(in-package turtleref)

(defparameter *frame-id* "world")
(defvar *depot-transform* (cl-tf:make-transform-stamped
                         *frame-id*
                         "goal_depot"
                         0.0
                         (make-3d-vector 5 5 0)
                         (make-identity-rotation)))

(defvar +rooms-list+
    `(;; (53 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 0.614088031054 -16.6795520782 -0.00534057617188)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
      ;; (55 . ,(cl-tf:make-pose
      ;;         (cl-tf:make-3d-vector 0.683276593685 -7.04426813126 0.00247192382812)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
      ;; (58 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 1.0835776329 2.3194694519 -0.00143432617188)
      ;;         (cl-tf:make-quaternion 0 0 1 1)))
      ;; (84 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 42.419921875 1.48207437992 0.00247192382812)
      ;;         (cl-tf:make-quaternion 0 0 1 1)))
      ;; (90 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 21.803314209 0.788441538811 -0.00143432617188)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 4.71)))
      ;; (57 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 0.938675024509 -0.0111182741821 0)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
      ;; (56 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 0.737103843689 -4.05178689957 0)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
      ;; (54 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 0.641882010698 -12.4274196625 0)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 3.14)))
      ;; (59 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 1.4972666502 -12.4794921875 0)
      ;;         (cl-tf:make-quaternion 0 0 0 1)))
      ;; (60 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 1.45447194576 -16.5756034851 0)
      ;;         (cl-tf:make-quaternion 0 0 0 1)))
      ;; (75 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 5.48008775711 1.74138748646 0)
      ;;         (cl-tf:make-quaternion 0 0 1 1)))
      ;; (76 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 9.53154468536 1.7540487051  0)
      ;;         (cl-tf:make-quaternion 0 0 1 1)))
      ;; (77 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 13.9252710342 1.6554543972 0)
      ;;         (cl-tf:make-quaternion 0 0 1 1)))
      ;; (92 . ,(cl-tf:make-pose 
      ;;         (cl-tf:make-3d-vector 13.9603891373 0.794801950455 0)
      ;;         (cl-tf:axis-angle->quaternion (cl-tf:make-3d-vector 0 0 1) 4.71)))
      ;; remove the below, only for testing in turtlesim
      (1 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 1.5 1.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (2 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 7.0 4.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (3 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 9.0 7.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (4 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 9.0 1.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (5 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 1.0 8.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (6 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 5.0 5.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (7 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 1.0 3.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (8 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 5.0 12.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (9 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector -5.0 5.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      (10 . ,(cl-tf:make-pose 
              (cl-tf:make-3d-vector 5.0 -3.0 0.0)
              (cl-tf:make-quaternion 0 0 1 1)))
      ))
