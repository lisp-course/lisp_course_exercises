cmake_minimum_required(VERSION 2.8.3)
project(designator_integration_msgs)

find_package(catkin REQUIRED COMPONENTS geometry_msgs message_generation)

add_message_files(FILES
  Designator.msg
  KeyValuePair.msg
  DesignatorRequest.msg
  DesignatorResponse.msg
)

add_service_files(FILES
  DesignatorCommunication.srv
)

generate_messages(
  DEPENDENCIES geometry_msgs
)

catkin_package(
  CATKIN_DEPENDS geometry_msgs message_runtime
)
