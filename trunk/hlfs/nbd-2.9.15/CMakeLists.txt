# CMAKE by Zhang bo.
# CMAKE updated by Harry Wei.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.
# Just for our LHDFS project.

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.

SET(CMAKE_VERBOSE_MAKEFILE ON)
PROJECT(NBD)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

#SET(WORK_HOME   /home/kanghua/workshop2/trunk/hlfs)
SET(WORK_HOME   $ENV{PWD}/../../)
MESSAGE(STATUS "Path is " ${WORK_HOME})
#SET(WORK_HOME  ../../hlfs)
# include these files for building.
SET(HADOOP_HOME ${WORK_HOME}/3part/hadoop)
SET(LOG_HOME ${WORK_HOME}/3part/log)
#SET(HADOOP_HOME /root/jiawei/hadoop/hadoop-0.20.0)
SET(JAVA_HOME   /usr/lib/jvm/java-6-sun)
INCLUDE_DIRECTORIES(
       /usr/include/glib-2.0 
       /usr/lib/glib-2.0/include 
       /usr/lib/i386-linux-gnu/glib-2.0/include
       ${LOG_HOME}/include
       ${JAVA_HOME}/include/
       ${JAVA_HOME}/include/linux
       ${HADOOP_HOME}/include
       #${WORK_HOME}/src/include
       ${WORK_HOME}/src/include
       ${WORK_HOME}/src/include/api)

ADD_CUSTOM_TARGET(all)
#----------------------ndb server-------------------------------#
ADD_CUSTOM_TARGET(nbdserver)
IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
LINK_DIRECTORIES(
	${LOG_HOME}/lib32
    	${HADOOP_HOME}/lib32
        ${WORK_HOME}/output/lib32
        ${JAVA_HOME}/jre/lib/i386/server)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
	${LOG_HOME}/lib64
    	${HADOOP_HOME}/lib64
        ${WORK_HOME}/output/lib64
        ${JAVA_HOME}/jre/lib/amd64/server)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(NBDSLIBS glib-2.0
             hlfs
	     log4c
	     hdfs
             jvm)

SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(NBDSSRCS nbd-server.c)
ADD_EXECUTABLE(nbd-server ${NBDSSRCS})
TARGET_LINK_LIBRARIES(nbd-server ${NBDSLIBS})
SET_TARGET_PROPERTIES(nbd-server PROPERTIES COMPILE_FLAGS "-g")
ADD_DEPENDENCIES(nbdserver nbd-server)

#----------------------ndb client-------------------------------#
ADD_CUSTOM_TARGET(nbdclient)
IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
LINK_DIRECTORIES(
	${LOG_HOME}/lib32
    	${HADOOP_HOME}/lib32
        ${WORK_HOME}/output/lib32)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
	${LOG_HOME}/lib64
    	${HADOOP_HOME}/lib64
        ${WORK_HOME}/output/lib64)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(NBDSLIBS glib-2.0
             hlfs
	     log4c
	     hdfs
             )

SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(NBDCSRCS nbd-client.c)
ADD_EXECUTABLE(nbd-client ${NBDCSRCS})
TARGET_LINK_LIBRARIES(nbd-client ${NBDCLIBS})
SET_TARGET_PROPERTIES(nbd-client PROPERTIES COMPILE_FLAGS "-g")
ADD_DEPENDENCIES(nbdclient nbd-client)

ADD_DEPENDENCIES(all nbdserver nbdclient)


