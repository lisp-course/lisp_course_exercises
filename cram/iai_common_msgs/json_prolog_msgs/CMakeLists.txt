cmake_minimum_required(VERSION 2.8.3)
project(json_prolog_msgs)

find_package(catkin REQUIRED COMPONENTS message_generation)

add_service_files(DIRECTORY srv
  FILES
  PrologFinish.srv
  PrologNextSolution.srv
  PrologQuery.srv)

generate_messages()

catkin_package(CATKIN_DEPENDS message_runtime)
