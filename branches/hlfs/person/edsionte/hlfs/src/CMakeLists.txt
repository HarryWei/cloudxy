# CMAKE by Zhang bo.
# CMAKE updated by Harry Wei.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.
# Just for our LHDFS project.

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.
SET(CMAKE_VERBOSE_MAKEFILE ON)
PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
#CMAKE_MINIMUM_REQUIRED(VERSION 2.6)

SET(WORK_HOME   $ENV{PWD}/..)
MESSAGE(STATUS "This is dir " ${WORK_HOME})
#SET(WORK_HOME   /home/kanghua/workshop3.bak/hlfs)
# include these files for building.
SET(HADOOP_HOME ${WORK_HOME}/3part/hadoop)
SET(LOG_HOME ${WORK_HOME}/3part/log)
#MESSAGE(STATUS "This is dir " ${HADOOP_HOME})
SET(JAVA_HOME 	/usr/lib/jvm/java-6-sun)
#SET(JAVA_HOME 	/usr/lib/jvm/java-6-openjdk)
#SET(JAVA_HOME 	/usr/lib/jvm/java-1.6.0-openjdk.x86_64)
#SET(JAVA_HOME /mnt/jdk1.7.0)
INCLUDE_DIRECTORIES(
       /usr/include/glib-2.0 
       /usr/lib/glib-2.0/include 
	   /usr/lib/i386-linux-gnu/glib-2.0/include
       ${JAVA_HOME}/include/
       ${JAVA_HOME}/include/linux
       ${HADOOP_HOME}/include
       ${LOG_HOME}/include
       include
       .)
# Use SRCS present all the sources.
# If it has subdirectory, use following commands for adding.

#AUX_SOURCE_DIRECTORY(utils SRCS)
#AUX_SOURCE_DIRECTORY(api SRCS)
#AUX_SOURCE_DIRECTORY(cleaner SRCS)
#AUX_SOURCE_DIRECTORY(blockcache SRCS)
#AUX_SOURCE_DIRECTORY(flusher SRCS)
#AUX_SOURCE_DIRECTORY(fuse SRCS)


#---------------------- Make all ------------------------------#
ADD_CUSTOM_TARGET(all)
#----------------------- Make libhlfs -------------------------#

ADD_CUSTOM_TARGET(libhlfs)
AUX_SOURCE_DIRECTORY(storage LHFSSRCS)
AUX_SOURCE_DIRECTORY(logger  LHFSSRCS)
AUX_SOURCE_DIRECTORY(backend LHFSSRCS)
AUX_SOURCE_DIRECTORY(clean LHFSSRCS)
AUX_SOURCE_DIRECTORY(utils LHFSSRCS)
AUX_SOURCE_DIRECTORY(snapshot LHFSSRCS)
AUX_SOURCE_DIRECTORY(cache LHFSSRCS)
AUX_SOURCE_DIRECTORY(icache LHFSSRCS)
AUX_SOURCE_DIRECTORY(clone LHFSSRCS)

IF(CMAKE_SIZEOF_VOID_P EQUAL 4)
	SET(LIBRARY_OUTPUT_PATH ${WORK_HOME}/output/lib32)
	LINK_DIRECTORIES(
    	${HADOOP_HOME}/lib32
	${LOG_HOME}/lib32
    	${JAVA_HOME}/jre/lib/i386/server)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
	SET(LIBRARY_OUTPUT_PATH ${WORK_HOME}/output/lib64)
	LINK_DIRECTORIES(
    	${HADOOP_HOME}/lib64
	${LOG_HOME}/lib64
    	${JAVA_HOME}/jre/lib/amd64/server)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(LHFLIBS   glib-2.0
              #-pthread 
              gthread-2.0 
              rt
	          hdfs
		      log4c
     	      jvm
			  )

ADD_LIBRARY(hlfs SHARED ${LHFSSRCS})
# Set linked libs with target.¡¡
TARGET_LINK_LIBRARIES(hlfs ${LHFLIBS})
# Set compile property.
SET_TARGET_PROPERTIES(hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
# Set linked property.
# SET_TARGET_PROPERTIES(hlfs PROPERTIES LINK_FLAGS "")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(hlfs -fprofile-arcs -ftest-coverage)
ADD_DEPENDENCIES(libhlfs hlfs)

#----------------------- Make tools ---------------------------#
ADD_CUSTOM_TARGET(tools)
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

SET(TOOLLIBS glib-2.0
             hlfs
	         log4c
	         hdfs
			 )

#SET(CMAKE_INSTALL_PREFIX /usr)
SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(TOOLSRCS tools/hlfs_mkfs.c)
ADD_EXECUTABLE(mkfs.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(mkfs.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(mkfs.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(mkfs.hlfs -fprofile-arcs -ftest-coverage)
#INSTALL(TARGETS mkfs.hlfs RUNTIME DESTINATION bin)
ADD_DEPENDENCIES(tools mkfs.hlfs)

#SET(CMAKE_INSTALL_PREFIX /usr)
SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(TOOLSRCS tools/hlfs_clone.c)
ADD_EXECUTABLE(clone.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(clone.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(clone.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(mkfs.hlfs -fprofile-arcs -ftest-coverage)
#INSTALL(TARGETS mkfs.hlfs RUNTIME DESTINATION bin)
ADD_DEPENDENCIES(tools clone.hlfs)

#SET(CMAKE_INSTALL_PREFIX /usr)
SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(TOOLSRCS tools/hlfs_take_snapshot.c)
ADD_EXECUTABLE(snapshot.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(snapshot.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(snapshot.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(snapshot.hlfs -fprofile-arcs -ftest-coverage)
#INSTALL(TARGETS mkfs.hlfs RUNTIME DESTINATION bin)
ADD_DEPENDENCIES(tools snapshot.hlfs)

SET(TOOLSRCS tools/hlfs_seg_usage_calc.c)
ADD_EXECUTABLE(segcalc.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(segcalc.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(segcalc.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(segcalc.hlfs -fprofile-arcs -ftest-coverage)
ADD_DEPENDENCIES(tools segcalc.hlfs)


SET(TOOLSRCS tools/hlfs_seg_clean.c)
ADD_EXECUTABLE(segclean.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(segclean.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(segclean.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(segclean.hlfs -fprofile-arcs -ftest-coverage)
ADD_DEPENDENCIES(tools segclean.hlfs)

SET(TOOLSRCS tools/nbd_ops.c)
ADD_EXECUTABLE(nbd_ops ${TOOLSRCS})
TARGET_LINK_LIBRARIES(nbd_ops ${TOOLLIBS})
SET_TARGET_PROPERTIES(nbd_ops PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(nbd_ops -fprofile-arcs -ftest-coverage)
ADD_DEPENDENCIES(tools nbd_ops)

SET(TOOLSRCS tools/tapdisk_ops.c)
ADD_EXECUTABLE(tapdisk_ops ${TOOLSRCS})
TARGET_LINK_LIBRARIES(tapdisk_ops ${TOOLLIBS})
SET_TARGET_PROPERTIES(tapdisk_ops PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
#ADD_DEFINITIONS(-fprofile-arcs -ftest-coverage)
#TARGET_LINK_LIBRARIES(tapdisk_ops -fprofile-arcs -ftest-coverage)
ADD_DEPENDENCIES(tools tapdisk_ops)

#---------------------- Make Test ------------------------------#


#--------------------- Make all end -----------------------------#
ADD_DEPENDENCIES(all hlfs mkfs.hlfs test1)
