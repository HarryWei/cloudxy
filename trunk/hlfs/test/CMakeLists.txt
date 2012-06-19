# CMAKE by Zhang bo.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.
# Just for our HLFS project.
# Modified by Kanghua <kanghua151@gmail.com>
# Kelvin Wang <senwang@linux.vnet.ibm.com>
# Used for cache testing

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.

SET(CMAKE_BUILD_TYPE "Debug") 
SET(CMAKE_CXX_FLAGS_DEBUG "$ENV{CXXFLAGS} -O0 -Wall -g")
SET(CMAKE_CXX_FLAGS_RELEASE "$ENV{CXXFLAGS} -O3 -Wall")
SET(CMAKE_VERBOSE_MAKEFILE ON)

PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
#CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
SET(WORK_HOME "$ENV{PWD}/../")
MESSAGE(STATUS "Root directory of project:" ${WORK_HOME})
#SET(JAVA_HOME 	/usr/lib/jvm/java-6-sun)
SET(JAVA_HOME 	/mnt/jdk1.7.0)
MESSAGE(STATUS "JAVA HOME:" ${JAVA_HOME})
#SET(JAVA_HOME 	/usr/lib/jvm/java-1.6.0-openjdk)

# include these files for building.
INCLUDE_DIRECTORIES(
	/usr/include/glib-2.0 
	/usr/lib/glib-2.0/include
	/usr/lib/i386-linux-gnu/glib-2.0/include
	${JAVA_HOME}/include/
	${JAVA_HOME}/include/linux
	${WORK_HOME}/src/include
	${WORK_HOME}/3part/log/include
)

IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
LINK_DIRECTORIES(
    ${WORK_HOME}/output/lib32
	${JAVA_HOME}/jre/lib/i386/client
	${JAVA_HOME}/jre/lib/i386/server
    ${WORK_HOME}/3part/log/lib32)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
    ${WORK_HOME}/output/lib64
    ${WORK_HOME}/3part/log/lib64)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(LIBS glib-2.0
		 hlfs
		 log4c
		 gcov
)

file(GLOB testfiles "test_*.c")
foreach(testfile ${testfiles})
#GET_FILENAME_COMPONENT(basename ${testname} ${testfile} NAME)
	GET_FILENAME_COMPONENT(testname ${testfile} NAME_WE)
	ADD_EXECUTABLE(${testname} ${testfile})
    SET_TARGET_PROPERTIES(${testname} PROPERTIES COMPILE_FLAGS "-g -U_FORTIFY_SOURCE")
    #ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
	TARGET_LINK_LIBRARIES(${testname} ${LIBS})

endforeach(testfile)
