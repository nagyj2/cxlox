cmake_minimum_required(VERSION 3.0.0)
project(cxlox LANGUAGES C VERSION 0.1.0)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

# Collect files
file(GLOB_RECURSE sources "src/*.c")
file(GLOB_RECURSE headers "inc/*.h")
set(libraries)

# Remove CLI main file
list(FILTER sources EXCLUDE REGEX "main.c")

# Set the include directory
include_directories(cxlox inc/)
# set(INCLUDE_DIR inc/) # incorrect

set(CMAKE_C_FLAGS "-Wall -Werror -g")
# set(CMAKE_C_FLAGS_DEBUG "-g")

include(CTest) # Allows us to use CTest
enable_testing() # Adds a 'test' target

# Conditionally do if test target is ran
# if (BUILD_TESTING)
# add_subdirectory(test)
# Libraries to lookup : Catch2, GoogleTest
# Link: https://cliutils.gitlab.io/modern-cmake/chapters/testing.html
# endif()
# add_test(testname executable arg1 arg2) # Performs only if test target is ran

# Replaced by the file(GLOB_RECURSE... above
# set(INCLUDES inc/common.h inc/memory.h inc/chunk.h inc/debug.h inc/value.h inc/vm.h inc/compiler.h inc/scanner.h inc/object.h inc/table.h inc/xstdlib.h inc/util.h)
# set(SOURCES src/memory.c src/chunk.c src/debug.c src/value.c src/vm.c src/compiler.c src/scanner.c src/object.c src/table.c src/xstdlib.c src/util.c)

add_executable(cxlox
  src/main.c
	${sources}
	${headers}
)



set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
