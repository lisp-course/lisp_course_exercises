cmake_minimum_required(VERSION 2.8.3)
project(bullet_wrapper)
find_package(catkin REQUIRED COMPONENTS
  cram_bullet_reasoning_belief_state
  cram_executive)
catkin_package()
include_directories(
  ${catkin_INCLUDE_DIRS}
)
