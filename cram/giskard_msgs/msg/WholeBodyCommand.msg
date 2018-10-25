# A whole-body command message for a mobile manipulator with a morphology 
# compatible with the PR2 robot. The message was designed to support alternative
# ways of specifying a command.

# definitions of possible values to use as type
uint8 STANDARD_CONTROLLER=0
uint8 YAML_CONTROLLER=1

# use of the above constants to indicate the type of command
uint8 type

# standard controllers
giskard_msgs/ArmCommand right_ee # command for right arm
giskard_msgs/ArmCommand left_ee # command for left arm

# yaml controllers
string yaml_spec # complete specification of controller
giskard_msgs/SemanticFloat64[] convergence_thresholds # thresholds used to decide convergence of motion
