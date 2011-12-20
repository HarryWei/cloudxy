PROJECT(HLFS)
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

SET(WORK_HOME $ENV{PWD}/../../../../)

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

SET(TESTSRC append_ss_delmark_test.c)
ADD_EXECUTABLE(asdtest ${TESTSRC})
TARGET_LINK_LIBRARIES(asdtest ${LIBS})

ADD_EXECUTABLE(ss2texttest ss2text_test.c)
TARGET_LINK_LIBRARIES(ss2texttest ${LIBS})

ADD_EXECUTABLE(dump_sstest dump_ss_test.c)
TARGET_LINK_LIBRARIES(dump_sstest ${LIBS})

ADD_EXECUTABLE(load_sstest load_ss_test.c)
TARGET_LINK_LIBRARIES(load_sstest ${LIBS})
