# only using timestamp for the moment
std_msgs/Header header

# feature values used to make decisions about succeeded actions
duration running_time
float64 left_arm_max_vel
float64 right_arm_max_vel
float64 torso_vel
giskard_msgs/SemanticFloat64[] convergence_values # internal values used to decide convergence

# classification results used to decide whether action succeeded
bool motion_started # true, as soon low-level controller has started execution the command
bool motion_old # true, if time passed since start of motion is above threshold
bool torso_moving # true, if torso velocity is above threshold
bool left_arm_moving # true, if velocity of any joint of left arm is above threshold
bool right_arm_moving # true, if velocity of any joint of right arm is above threshold
giskard_msgs/SemanticBool[] convergence_flags # true, if the corresponding feature values are below a threshold
