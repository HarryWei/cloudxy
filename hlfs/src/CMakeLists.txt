# CMAKE by Zhang bo.
# CMAKE updated by Harry Wei.
# Time & Place: 2011, 3, 26, @ linux lab in xiyou.

# This *ON* can let cmake procedure output into console.
# _So_that we can debug it.
SET(CMAKE_VERBOSE_MAKEFILE ON)
PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
#CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
SET(WORK_HOME   $ENV{PWD}/..)
MESSAGE(STATUS "Current Work Dir :" ${WORK_HOME})
#SET(HADOOP_HOME ${WORK_HOME}/3part/hadoop)
#SET(LOG_HOME ${WORK_HOME}/3part/log)
#SET(SNAPPY_HOME ${WORK_HOME}/3part/snappy)
#SET(JAVA_HOME 	/usr/lib/jvm/java-7-oracle)
MESSAGE(STATUS "Probe Glib Info")
find_package(PkgConfig REQUIRED)
pkg_check_modules(GLIB_PKG glib-2.0)
if (GLIB_PKG_FOUND)
    message(Found glib-2.0)
include_directories(${GLIB_PKG_INCLUDE_DIRS})
endif(GLIB_PKG_FOUND)


SET(JAVA_HOME  $ENV{JAVA_HOME}/)
MESSAGE(STATUS "Probe Java Home:" ${JAVA_HOME})

INCLUDE_DIRECTORIES(

		#/usr/lib/glib-2.0/include 
		#/usr/lib/i386-linux-gnu/glib-2.0/include
                #/usr/lib/x86_64-linux-gnu/glib-2.0/include
		#${JAVA_HOME}/include
		#${JAVA_HOME}/include/linux
		#${HADOOP_HOME}/include
		#${LOG_HOME}/include
		#${SNAPPY_HOME}/include
                /usr/include/
		include
		.)
# Use SRCS to save all the sources.
# If it has subdirectory, use following commands for adding.


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
        ${JAVA_HOME}/jre/lib/i386/server/)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
    SET(LIBRARY_OUTPUT_PATH ${WORK_HOME}/output/lib64)
    LINK_DIRECTORIES(
        ${JAVA_HOME}/jre/lib/amd64/server/)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(LHFLIBS   
    glib-2.0
    gthread-2.0 
    rt
    hdfs
    log4c
    snappy
    jvm
    )

ADD_LIBRARY(hlfs SHARED ${LHFSSRCS})
# Set linked libs with target.��
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
        ${WORK_HOME}/output/lib32)
ELSEIF(CMAKE_SIZEOF_VOID_P EQUAL 8)
LINK_DIRECTORIES(
        ${WORK_HOME}/output/lib64)
ENDIF(CMAKE_SIZEOF_VOID_P EQUAL 4)

SET(TOOLLIBS glib-2.0
             hlfs
	     log4c
	     hdfs
             snappy)

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

#SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(TOOLSRCS tools/hlfs_stat.c)
ADD_EXECUTABLE(stat.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(stat.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(stat.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools stat.hlfs)

SET(TOOLSRCS tools/hlfs_rmfs.c)
ADD_EXECUTABLE(rmfs.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(rmfs.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(rmfs.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools rmfs.hlfs)

#SET(CMAKE_INSTALL_PREFIX /usr)
SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(TOOLSRCS tools/hlfs_clone.c)
ADD_EXECUTABLE(clone.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(clone.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(clone.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools clone.hlfs)

#SET(CMAKE_INSTALL_PREFIX /usr)
SET(EXECUTABLE_OUTPUT_PATH ../output/bin)
SET(TOOLSRCS tools/hlfs_take_snapshot.c)
ADD_EXECUTABLE(snapshot.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(snapshot.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(snapshot.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools snapshot.hlfs)

SET(TOOLSRCS tools/hlfs_seg_usage_calc.c)
ADD_EXECUTABLE(segcalc.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(segcalc.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(segcalc.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools segcalc.hlfs)


SET(TOOLSRCS tools/hlfs_seg_clean.c)
ADD_EXECUTABLE(segclean.hlfs ${TOOLSRCS})
TARGET_LINK_LIBRARIES(segclean.hlfs ${TOOLLIBS})
SET_TARGET_PROPERTIES(segclean.hlfs PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools segclean.hlfs)

SET(TOOLSRCS tools/nbd_ops.c)
ADD_EXECUTABLE(nbd_ops ${TOOLSRCS})
TARGET_LINK_LIBRARIES(nbd_ops ${TOOLLIBS})
SET_TARGET_PROPERTIES(nbd_ops PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools nbd_ops)

SET(TOOLSRCS tools/tapdisk_ops.c)
ADD_EXECUTABLE(tapdisk_ops ${TOOLSRCS})
TARGET_LINK_LIBRARIES(tapdisk_ops ${TOOLLIBS})
SET_TARGET_PROPERTIES(tapdisk_ops PROPERTIES COMPILE_FLAGS "-g -Wstrict-prototypes")
ADD_DEPENDENCIES(tools tapdisk_ops)
#--------------------- Make all end -----------------------------#

install(TARGETS hlfs
    RUNTIME DESTINATION hlfs/bin
    LIBRARY DESTINATION hlfs/lib
    ARCHIVE DESTINATION hlfs/lib
    )
install(DIRECTORY ../src/include DESTINATION hlfs
        FILES_MATCHING PATTERN "*.h"
        PATTERN ".git" EXCLUDE
        PATTERN ".svn" EXCLUDE
)

install(DIRECTORY ../output/bin DESTINATION hlfs
        PATTERN ".svn" EXCLUDE
)
