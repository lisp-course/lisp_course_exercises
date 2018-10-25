# Feedback provided by giskard controllers.

# Header to communicate timestamps, and being compatible with message-filters.
std_msgs/Header header
# Flag to indicate whether the watchdog is active
bool watchdog_active
# Commands issued to motor controllers, expressed with semantics hints.
sensor_msgs/JointState commands
# Slack values of task functions, expressed with semantics hints.
giskard_msgs/SemanticFloat64[] slacks
# Internal double expressions used to decide convergence
giskard_msgs/SemanticFloat64[] convergence_features
# Internal double expressions wired through for debugging
giskard_msgs/SemanticFloat64[] doubles
# Internal vector expressions wired through for debugging
giskard_msgs/SemanticVector3[] vectors

# Command that the controller is currently pursuing
giskard_msgs/WholeBodyCommand current_command
# Hash of the current command calculated by the controller
uint64 current_command_hash
