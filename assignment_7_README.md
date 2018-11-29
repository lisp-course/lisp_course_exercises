Welcome to Assignment 7 of the "Robot Programming with Lisp" course.

In this assignment you will create two ROS packages, one for learning
the ROS Lisp API through simple examples, and the other one
for playing around with the `turtlesim` simulator.
There is a roslisp tutorial that will guide you through the process.
However, in order to be able to follow the tutorial, you first have to
go through a couple of other introductory tutorials for getting comfortable
with ROS. Those are the beginner tutorials number
1.1.2, 1.1.3, 1.1.4, 1.1.5, 1.1.6, 1.1.7, 1.1.8 and 1.1.10
of the official ROS documentation page:
http://wiki.ros.org/ROS/Tutorials#Beginner_Level
and if the Website is down, use the archive:
https://web.archive.org/web/20181118205753/wiki.ros.org/ROS/Tutorials#Beginner_Level

Once you're done with these, you can get to the roslisp tutorial:
http://wiki.ros.org/roslisp/Tutorials/OverviewVersion

When executing the commands from the tutorial, please make sure that you
don't have any typos and that you execute the commands in the correct directory.
Some of the descriptions in the tutorial are very concise which means
that you need to understand what's going on in order to be able to follow it.
If you were attentive in the beginner tutorials that should not be a problem.
Here are a couple of hints for you, just in case:
 - The `package.xml` and `CMakeLists.txt` files are always located in the
   root directory of your ROS package, e.g. if you have a package called
   `tutorial_ros_package`, the above-mentioned files will be
   `tutorial_ros_package/package.xml` and `tutorial_ros_package/CMakeLists.txt`.
   This holds for any ROS package.
 - The `.lisp` files are usually located under the `src` directory of your package
   or in a subdirectory thereof, e.g. `tutorial_ros_package/src/some-file.lisp`
   or `lisp_turtles/src/turtles/some-other-file.lisp`.
 - `.msg` and `.srv` files are always located in their own directories
   under the root of your ROS package, e.g. `tutorial_ros_package/msg/MyAwesome.msg`
   or `tutorial_ros_package/srv/MyAwesome.srv`.
 - If you get errors in your Lisp shell it might help to restart the REPL completely.
 - You need to restart REPL or sometimes even Emacs completely each time you
   recompile your ROS workspace (`catkin_make`).
 - You can have multiple REPLs inside of one Emacs. Starting a new REPL is done
   with "<Alt>-x slime", see the tutorial. However, you might be better off
   with starting two different Emacs instances from two different terminals.

Once you're done with the tutorial and your turtle is successfully creating
amazing pieces of turtle art, the only thing you will need to add is:
(1) In the directory `lisp_turtles/src/turtles/` create a new file `turtle-party.lisp`.
    Make sure this file is loaded when the `lisp-turtles` system is loaded, i.e.,
    add the corresponding entry into the `lisp-turtles.asd` file.
(2) In the file `lisp_turtles/src/turtles/turtle-party.lisp` define a function
    DRAW that accepts one argument - TURTLE-ID - such that when
    `REPL> (draw 1)` is executed the turtle with the name "turtle1" would start
    drawing random things, just as it is done in the very end of the roslisp tutorial.
    When `REPL> (draw 2)` is executed the turtle named "turtle2" starts drawing.
(3) In the same file `lisp_turtles/src/turtles/turtle-party.lisp`
    create a function TURTLE-PARTY that takes one argument - TURTLE-COUNT -
    and does the following:
   * starts a new ROS node
   * spawns (TURTLE-COUNT - 1) number of turtles
   * and calls DRAW on TURTLE-COUNT number of turtles.
 
That is, `REPL> (turtle-party 5)` spawns 4 new turtles and sends 5 turtles
that are by then living in the turtlesim drawing.
For this to work, you need to modify the DRAW function such that
LOOP-AT-MOST-EVERY is called only once per TURTLE-PARTY.

You have successfully completed the homework if:
- your ROS packages compile with `catkin_make`
- your ROS systems successfully load in the Lisp REPL through `,ros-load-system`
- executing `LTURTLE> (turtle-party 5)` in a fresh REPL with freshly loaded
  `lisp_turtles` package sends 5 turtles in a running turtlesim drawing.

