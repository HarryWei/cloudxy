# CMAKE by Zhang bo.
# CMAKE updated by Harry Wei.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.
# Just for our LHDFS project.

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.
SET(CMAKE_VERBOSE_MAKEFILE ON)
PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# include these files for building.
SET(WORK_HOME   "$ENV{PWD}/../../../../")
SET(HADOOP_HOME ${WORK_HOME}/3part/hadoop)
#SET(HADOOP_INSTALL /home/kanghua/hadoop-0.20.2/build/c++/Linux-amd64-64)
SET(JAVA_HOME   /usr/lib/jvm/java-6-sun)
#SET(HADOOP_HOME /root/jiawei/hadoop/hadoop-0.20.0)
INCLUDE_DIRECTORIES(
       /usr/include/glib-2.0 
       ${JAVA_HOME}/include/
       ${JAVA_HOME}/include/linux
       /usr/lib/glib-2.0/include 
       ${HADOOP_HOME}/include
       ${WORK_HOME}/src/include
	   ${WORK_HOME}/3part/log/include
       include)
# Use SRCS present all the sources.
# If it has subdirectory, use following commands for adding.


#----------------------- Make tools ---------------------------#

IF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
        ${HADOOP_HOME}/lib64
        ${WORK_HOME}/output/lib64
        ${WORK_HOME}/3part/log/lib64
        ${JAVA_HOME}/jre/lib/amd64/server
        )
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 4)
LINK_DIRECTORIES(
        ${HADOOP_HOME}/lib32
        ${WORK_HOME}/output/lib32
        ${WORK_HOME}/3part/log/lib64
	    ${JAVA_HOME}/jre/lib/i386/client
	    ${JAVA_HOME}/jre/lib/i386/server
        )
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 8)


SET(LIBS     glib-2.0
             hadooppipes
             hadooputils
             pthread
             ssl
	         hdfs
             hlfs)

SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(SRCS mr_clean_route.cpp)
ADD_EXECUTABLE(mr_segcalc.hlfs ${SRCS})
TARGET_LINK_LIBRARIES(mr_segcalc.hlfs ${LIBS})
SET_TARGET_PROPERTIES(mr_segcalc.hlfs PROPERTIES COMPILE_FLAGS "-g")
