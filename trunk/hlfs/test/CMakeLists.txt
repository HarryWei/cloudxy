# CMAKE by Zhang bo.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.
# Just for our HLFS project.
# Modified by Kanghua <kanghua151@gmail.com>
# Kelvin Wang <senwang@linux.vnet.ibm.com>
# Used for cache testing

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.
#Be sure that you have made a build dir under hlfs/test
#before you execute this CMakeLists.txt.

SET(CMAKE_BUILD_TYPE "Debug") 
SET(CMAKE_CXX_FLAGS_DEBUG "$ENV{CXXFLAGS} -Wall -g")
SET(CMAKE_CXX_FLAGS_RELEASE "$ENV{CXXFLAGS} -Wall")
SET(CMAKE_VERBOSE_MAKEFILE ON)

PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
#CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
SET(WORK_HOME "$ENV{PWD}/../../")
MESSAGE(STATUS "Root directory of project:" ${WORK_HOME})

MESSAGE(STATUS "Probe Glib Info")
find_package(PkgConfig REQUIRED)
pkg_check_modules(GLIB_PKG glib-2.0)
if (GLIB_PKG_FOUND)
    message(Found glib-2.0)
include_directories(${GLIB_PKG_INCLUDE_DIRS})
endif(GLIB_PKG_FOUND)

SET(JAVA_HOME  $ENV{JAVA_HOME}/)
MESSAGE(STATUS "Probe Java Home:" ${JAVA_HOME})
# include these files for building.
INCLUDE_DIRECTORIES(
    ${WORK_HOME}/src/include
)

IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
LINK_DIRECTORIES(
    ${WORK_HOME}/output/lib32
    ${JAVA_HOME}/jre/lib/i386/server)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
    ${WORK_HOME}/output/lib64
    ${JAVA_HOME}/jre/lib/amd64/server)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(LIBS glib-2.0
         hlfs
         log4c
         snappy
)

file(GLOB testfiles "test_*.c")
foreach(testfile ${testfiles})
#GET_FILENAME_COMPONENT(basename ${testname} ${testfile} NAME)
	GET_FILENAME_COMPONENT(testname ${testfile} NAME_WE)
	ADD_EXECUTABLE(${testname} ${testfile})
        SET_TARGET_PROPERTIES(${testname} PROPERTIES COMPILE_FLAGS "-g")
        #ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
	TARGET_LINK_LIBRARIES(${testname} ${LIBS})
endforeach(testfile)
