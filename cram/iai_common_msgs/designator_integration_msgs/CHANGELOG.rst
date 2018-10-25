^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Changelog for package designator_integration_msgs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

0.0.6 (2016-04-21)
------------------
* Changelog files for release
* Contributors: Jan Winkler

0.0.5 (2015-04-24)
------------------

0.0.4 (2015-03-19)
------------------
* Allowing to encode float arrays in designator message
* Added 3 more value types to designator-integration-msgs.
* Added support for designators of type humans to designator_integration_msgs.
* Changed internal format of floating point numbers from 32 bit to 64 bit
* Contributors: Daniel Beßler, Georg Bartels, Jan Winkler

0.0.3 (2014-10-14)
------------------

0.0.1 (2014-05-16)
------------------
* Cleaned up CMakeLists.txt
* Added package descriptions and fixed licenses where applicable
* Added new types into message definition
* Removed faulty feedback entry in the ROS service
* Some more clean up of catkin make related files: message generation explicitly invokes all possible languages.
* fixed CMakeLists.txt and package.xml dependencies.
* Changed the `value_data' field to a much more meaningful char array rather than a float32 array
* Added type `geometry_msgs::Pose'
* Abstracted the type of requests and responses for deginator services
* Added message types for communication via designators
* Contributors: Jan Winkler, Jan-Hendrik Worch, Wiedemeyer
