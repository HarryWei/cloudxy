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
SET(WORK_HOME   $ENV{PWD}/../../../../)
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
       include)
# Use SRCS present all the sources.
# If it has subdirectory, use following commands for adding.


#----------------------- Make tools ---------------------------#

LINK_DIRECTORIES(
        ${JAVA_HOME}/jre/lib/amd64/server
        ${HADOOP_HOME}/lib64
        ${WORK_HOME}/output/lib64
        )

SET(LIBS     glib-2.0
             hadooppipes
             hadooputils
             pthread
             ssl
	         hdfs
             hlfs)

SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(SRCS mr_clean_route.cpp)
ADD_EXECUTABLE(mr_cleaner ${SRCS})
TARGET_LINK_LIBRARIES(mr_cleaner ${LIBS})
SET_TARGET_PROPERTIES(mr_cleaner PROPERTIES COMPILE_FLAGS "-g")
