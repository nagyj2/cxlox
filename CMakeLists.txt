cmake_minimum_required(VERSION 3.0.0)
project(cxlox VERSION 0.1.0)

set(CMAKE_C_STANDARD 99) # 99 for portability (and I dont need 11 or 17's features)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS ON)

# Collect files
file(GLOB_RECURSE sources "src/vm/*.c") # All files needed to compile and execute xlox
file(GLOB_RECURSE modules "src/modules/*.c") # Modules that can be loaded into xlox
file(GLOB_RECURSE cli "src/cli/*.c") # Command line interface code
file(GLOB_RECURSE headers "inc/*.h") # Headers for all files (includes subfolders)
# set(libraries)

# Remove CLI main file

# Set the include directory
include_directories(cxlox inc/)
# set(INCLUDE_DIR inc/) # incorrect

set(CMAKE_C_FLAGS "-Wall -Werror")
# set(CMAKE_C_FLAGS_DEBUG "-g")

# include(CTest) # Allows us to use CTest
# enable_testing() # Adds a 'test' target

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
  ${cli}
	${sources}
	${modules}
	${headers}
)



set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
# include(CPack)
