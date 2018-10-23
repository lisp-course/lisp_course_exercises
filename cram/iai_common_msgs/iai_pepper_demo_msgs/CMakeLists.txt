cmake_minimum_required(VERSION 2.8.3)
project(iai_pepper_demo_msgs)

find_package(catkin REQUIRED COMPONENTS
    message_generation std_msgs geometry_msgs
    )


add_service_files(FILES
  PepperComm.srv
)


generate_messages(
  DEPENDENCIES
    std_msgs
    geometry_msgs)

catkin_package(
  CATKIN_DEPENDS std_msgs geometry_msgs)
