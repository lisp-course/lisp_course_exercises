(defsystem tutorial
  :depends-on (roslisp
               cram-language
               turtlesim-msg
               turtlesim-srv
               cl-transforms
               geometry_msgs-msg
               roslisp-utilities)
  :components
  ((:module "src"
            :components
            ((:file "tutorial-package")
             ;; (:file "cheat-sheet" :depends-on ("tutorial-package"))
             ))))
