cmake_minimum_required(VERSION 3.0.0)
project(cxlox VERSION 0.1.0)

include(CTest) # Allows us to use CTest
enable_testing() # Adds a 'test' target

# Conditionally do if test target is ran
# if (BUILD_TESTING)
# add_subdirectory(test)
# Libraries to lookup : Catch2, GoogleTest
# Link: https://cliutils.gitlab.io/modern-cmake/chapters/testing.html
# endif()
# add_test(testname executable arg1 arg2) # Performs only if test target is ran

include_directories(cxlox inc)

set(INCLUDES inc/common.h inc/memory.h inc/chunk.h inc/debug.h inc/value.h inc/vm.h)
set(SOURCES src/memory.c src/chunk.c src/debug.c src/value.c src/vm.c)

add_executable(cxlox
  src/main.c
	${SOURCES}
	${INCLUDES}
)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
