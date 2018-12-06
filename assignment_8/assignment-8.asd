
(asdf:defsystem assignment-8
  :depends-on (roslisp actionlib actionlib_msgs-msg turtle_actionlib-msg
                       geometry_msgs-msg std_msgs-msg
                       turtlesim-srv
                       cl-tf)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "turtlesim-action-client" :depends-on ("package"))
             (:file "turtlesim-tf" :depends-on ("package"))))))
