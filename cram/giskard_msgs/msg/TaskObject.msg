# A representation of an object which is part of an action.
# Used for querying task-specific constraints for giskard controllers.

# definitions of possible values for field 'role'
uint8 INVALID_ROLE=0 # make sure users set a proper role
uint8 SOURCE_ROLE=1 # object acts as the source of the action
uint8 TARGET_ROLE=2 # object acts as the target of the action

# unique identifier of the object
string name

# perceived pose of the object
geometry_msgs/PoseStamped pose

# role the object will play within a task
uint8 role

# in case the object is filled with a liquid
float64 liquid_volume # in cubic-meters
