Welcome to the last assignment of the "Robot Programming with Lisp" course!

This assignment is for practicing with the Actionlib and TF libraries.
The task is to have one turtle autonomously drawing shapes in turtlesim and
the other one autonomously following it.

---------------------
Step 1: launch file
---------------------
You will need to have a bunch of ROS nodes running for this task.
The best way to manage these nodes is to write a launch file for them.
In the `assignment_8` directory create a `launch` subdirectory.
In there, create a launch file that would start the following nodes:
 * the turtlesim simulator
 * the keyboard teleoperation node (you might need it for debugging)
 *  the "shape_server" node of the "turtle_actionlib" ROS package
 * two TF transforms broadcasters, for "turtle1" and "turtle2",
    the ROS package is "turtle_tf", the node is called "turtle_tf_broadcaster"
    and it accepts a parameter "turtle" which is the turtle name.
Look in the "turtle_tf_demo.launch" file of the "turtle_tf" ROS package
for an example of such a launch file:
```
$ roscd turtle_tf/launch
```

`roslaunch` the resulting file and make sure it spawns everything without errors.


--------------------------
Step 2: ActionLib client
--------------------------
In the root of `assignment-8` directory there is an ASDF system definition for
this assignment. It already has all the necessary dependencies listed for you.
(You can, of course, add new dependencies if needed.)
The components of the system are 3 files:
"package.lisp" with the Lisp namespace definition, it is already there for you,
"turtlesim-action-client.lisp" which will contain the ActionLib client code and
"turtlesim-tf" which will implement the follower behaviour for the turtles.

Create the "turtlesim-action-client.lisp" and write a function
CALL-SHAPE-ACTION-IN-A-LOOP that accepts no arguments and calls the "turtle_actionlib"
"ShapeAction" in an endless loop with different number of edges and with radius = 2.
Choose the number of edges randomly between 2 and 5.

You may reuse the code from the ActionLib client tutorial:
ActionLib client: http://wiki.ros.org/actionlib_lisp/Tutorials/actionlibBasicUsage

Open the REPL, load the `assignment-8` system and run CALL-SHAPE-ACTION-IN-A-LOOP.

TIPS:
- Don't forget to start a ROS node every time you want to communicate with other
  nodes from Lisp (ROSLISP:START-ROS-NODE).
- If your action client is dead try reinitializing it (INIT-ACTION-CLIENT from
  the tutorial).
- If you get weird ActionLib errors at runtime, try restarting your ROS node
  (ROSLISP:START-ROS-NODE) or the action server (restarting the launch file).


------------------------------------
Step 3: TF and navigation commands
------------------------------------
Now we would like to subscribe to the TF topic, read the coordinates of turtles
and send some navigation commands to one of the turtles.

Now our REPL is busy looping the action goals, fire up a fresh Emacs (REPL).
Spawn a new turtle called "turtle2" in the turtlesim: you may use the command
line or the SPAWN-TURTLE function from the previous assignment:
```
$ rosservice call /spawn TAB ENTER
```
where TAB stands for the TAB key and ENTER for the ENTER key.


To check if the TF publishing is working use
```
$ rostopic echo /tf
```
or better
```
$ rosrun tf tf_echo turtle1 turtle2
```


Create a file "turtlesim-tf.lisp" and write there a function FOLLOW-TURTLE-IN-A-LOOP
which accepts 2 arguments: FOLLOWER-NAME and FOLLOWEE-NAME, which are turtle names,
e.g. the strings "turtle1" and "turtle2".
This function
 * initializes a new CL-TF:TRANSFORM-LISTENER and a new publisher
    for turtle command velocities (see the SET-TURTLE-VELOCITY function from
    assignment_6) that will send commands to the turtle called FOLLOWER-NAME.
 * in an endless loop with 100 Hz frequency (every 0.01 seconds) it looks
    up the coordinates of FOLLOWEE-NAME relative to FOLLOWER-NAME and gives
    FOLLOWER-NAME a navigation command.

The navigation command consists of
- a linear velocity V, in 2D we are only interested in Vx - the X component,
- and angular velocity W, we are only interested in the angle around the Z axis - Wz.
Vx defines how fast our turtle moves, Wz defines the direction, i.e. the turning angle.
Wz = 0 means the turtle moves straight, Wz = PI means it goes backwards,
Wz = PI/2 means it turns left (right hand rule), and so on.

```
            Y
            ^
            |
         Ty +........T
            |      . .
            |   .    .
            | .Theta .
    --------+--------|----------> X
          0 |        Tx
            |
```

In the illustration above Theta is the angle between the X axis and the
coordinates of T (T for Turtle).

For the linear velocity you can choose a constant value, e.g. 1.0.

TIPS:
- Each time the communication between your TF listener and the publishers
  (that you start through the launch file) breaks, your TF listener freezes.
  Make sure you restart the ROS node each time the launch file / ROS master dies
  (ROSLISP:START-ROS-NODE).
- You can see the commands your Lisp program is sending to the turtle by listening
  to the corresponding topic (`$ rostopic echo /turtle2/cmd_vel`)
  
  
  
