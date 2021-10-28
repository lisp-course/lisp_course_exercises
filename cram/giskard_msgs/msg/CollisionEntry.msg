# definitions of possible collision types to use for value
uint8 AVOID_COLLISION=0
uint8 ALLOW_COLLISION=1
uint8 AVOID_ALL_COLLISIONS=2
uint8 ALLOW_ALL_COLLISIONS=3
uint8 ATTRACT=4
uint8 REPELL=5
string ALL=

# use one of the above constants as collision type
uint8 type

# enforced minimum distance between the closest points on both surfaces
float64 min_dist

# link of the controlled robot, if left empty all links will be selected
string[] robot_links

# name of body B, i.e. the second body in the described collision, can also be the controlled robot
string body_b
# optional: link of the particular link on body B
## note: only works if body B is a multibody with a link with that name
# note: if left empty, the entire body B is selected
string[] link_bs

