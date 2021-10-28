# definitions of possible values to use as type
string UNDEFINED=
string JOINT=JointPositionList

# use of the above constants to indicate the type of controller
string type

# controller goal, interpretation depends on value in type field
# JOINT: reads the position values for all joints in the kinematic chain between root_link and tip_link
sensor_msgs/JointState goal_state
