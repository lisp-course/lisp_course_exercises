# definitions of possible values to use as type
uint8 UNDEFINED=0
uint8 JOINT=1
uint8 TRANSLATION_3D=2
uint8 ROTATION_3D=3

# use of the above constants to indicate the type of controller
uint8 type

# Name of the link that is at the start of the controlled kinematic chain
string root_link

# Name of the link that is at the end of the controlled kinematic chain
string tip_link 

# Proportional gain used by the controller
float64 p_gain

# Weight assigned to the controller within the cost-function solved in each cycle
float64 weight

# Maximum speed
# m/s if type==TRANSLATION_3D
# rad/s if type==ROTATION_3D
# min(max_speed, joint_velocity_limit_specified_in_urdf) if type==JOINT
float64 max_speed

# controller goal, interpretation depends on value in type field
# JOINT: ignores this field
# TRANSLATION_3D: desired translation of tip_link relative to root_link
# ROTATION_3D: desired orientation of tip_link relative to root_link
geometry_msgs/PoseStamped goal_pose

# controller goal, interpretation depends on value in type field
# JOINT: reads the position values for all joints in the kinematic chain between root_link and tip_link
# TRANSLATION_3D: ignores this field
# ROTATION_3D: ignores this field
sensor_msgs/JointState goal_state
