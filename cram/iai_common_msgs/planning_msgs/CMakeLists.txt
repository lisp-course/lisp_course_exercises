cmake_minimum_required(VERSION 2.8.3)
project(planning_msgs)

find_package(catkin REQUIRED COMPONENTS message_generation)

add_message_files(FILES
  Binding.msg
  Plan.msg
  Step.msg
)

add_service_files(FILES
  Planning.srv
)

generate_messages(
  DEPENDENCIES
)

catkin_package(
  CATKIN_DEPENDS message_runtime
)
