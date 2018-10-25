# A command message for robotic manipulators that exposes a couple
# of alternative ways of specifying commands.

# Definitions of possible values used for type
uint8 IGNORE_GOAL=0 # Ignore the rest of this command.
uint8 CARTESIAN_GOAL=1 # Use the PoseStamped.
uint8 JOINT_GOAL=2 # Use the list of floats.

# use one of the above constants to indicate the type of command
uint8 type

# Cartesian goal for the arm.
# Note: The header will be used to resolve goal using tf.
geometry_msgs/PoseStamped goal_pose

# Joint goal for the arm.
float64[] goal_configuration

# List of internal double expressions with thresholds to decide convergence
giskard_msgs/SemanticFloat64[] convergence_thresholds
