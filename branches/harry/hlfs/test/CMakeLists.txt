#CMAKE by Zhang bo.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.
# Just for our LHDFS project.

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.
SET(CMAKE_BUILD_TYPE "Debug") 
SET(CMAKE_CXX_FLAGS_DEBUG "$ENV{CXXFLAGS} -O0 -Wall -g")
SET(CMAKE_CXX_FLAGS_RELEASE "$ENV{CXXFLAGS} -O3 -Wall")
SET(CMAKE_VERBOSE_MAKEFILE ON)
PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
# include these files for building.
SET(WORK_HOME $ENV{PWD}/../../)
MESSAGE(STATUS "This is dir" ${WORK_HOME})
INCLUDE_DIRECTORIES(
       /usr/include/glib-2.0 
       /usr/lib/glib-2.0/include 
       ${WORK_HOME}/src/include
       ${WORK_HOME}/3part/log/include
)
IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
LINK_DIRECTORIES(
    ${WORK_HOME}/output/lib32
    ${WORK_HOME}/3part/log/lib32)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
    ${WORK_HOME}/output/lib64
    ${WORK_HOME}/3part/log/lib64)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(LIBS glib-2.0
         hlfs
	     log4c)

SET(TESTSRC lectures/g_option.c)
ADD_EXECUTABLE(test ${TESTSRC})
TARGET_LINK_LIBRARIES(test ${LIBS})
