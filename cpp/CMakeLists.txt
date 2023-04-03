cmake_minimum_required(VERSION 3.10)

project(ApproShapley VERSION 1.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

# for more speed
set(CMAKE_CXX_FLAGS "-O3")

add_executable(appro_shapley main.cpp)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)
target_link_libraries(appro_shapley PRIVATE Threads::Threads)