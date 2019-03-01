cmake_minimum_required(VERSION 3.13)
project(BullAndCows)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "-Wall -Wsign-conversion -Wconversion -Wextra -Werror")

add_executable(BullAndCows main.cpp FBullCowGame.h FBullCowGame.cpp)