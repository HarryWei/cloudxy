				HLFS (HDFS-based Log-structured File System)

These are the introduction of HLFS. Please read them before you start your jou-
rney to HLFS, as they tell you what the HLFS is and how to install it.

What is HLFS
============

HLFS(HDFS-based Log-structured File System) is the back-end storage system for
cloudxy, a elastic cloud computing platform. The basic theory is, HLFS impleme-
nted a log-structured file system like block-level storage system on a virtual 
disk, emulated by HDFS files. You can learn more at:

http://code.google.com/p/cloudxy/w/list

How to make contribution
========================

If you are intrested in HLFS and willing to join us, you can subscribe HLFS's
Mailing List, or talk with us on IRC channels or QQ. Here are some useful imfo-
mation:

Owner: Kang Hua <kanghua151@gmail.com>
Owner: Chen Lijun <cljcore@gmail.com>
Home Page: http://code.google.com/p/cloudxy
Mailing List: cloudxy@googlegroups.com
QQ: 92625284
IRC: server-Freenode channel-#hlfs

Install
=======

 - HLFS depends on glib and JAVA, you should install them first.
 - Download the source code from the google code.
 	# svn checkout http://cloudxy.googlecode.com/svn/trunk/ cloudxy-read-only
 	# git clone git://github.com/kelvin-xupt/hlfs.git
 	# Download hlfs.tar.gz
 - Modify the CMakeList.txt. 
 	# Set the variant JAVA_HOME to your own.
 	# Add your own path of include headers to INCLUDE_DIRECTORIES.
 - Generate Makefile:
 	# Change the current directory to $HLFS_HOME/build.
 	# Execute the command "cmake ../src"
 - Generate libhlfs.so
	# make


 Documentation v1: Wang Sen <kelvin.xupt@gmail.com> 2012.8.12
