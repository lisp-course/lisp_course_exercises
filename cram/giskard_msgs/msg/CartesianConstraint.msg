# definitions of possible values to use as type
string UNDEFINED=
string TRANSLATION_3D=CartesianPosition
string ROTATION_3D=CartesianOrientationSlerp
string POSE_6D=CartesianPose

# use of the above constants to indicate the type of controller
string type

# Name of the link that is at the start of the controlled kinematic chain
string root_link

# Name of the link that is at the end of the controlled kinematic chain
string tip_link 

# controller goal, interpretation depends on value in type field
# TRANSLATION_3D: desired translation of tip_link relative to root_link
# ROTATION_3D: desired orientation of tip_link relative to root_link
geometry_msgs/PoseStamped goal
