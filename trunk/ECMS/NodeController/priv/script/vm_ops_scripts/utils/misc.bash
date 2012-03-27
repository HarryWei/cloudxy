#!/bin/bash

function build_workdir_by_vmid(){
if [ $# != 1 ];then
    LOG_MSG "$* parameter error"
return 255
fi
local VMID=$1
local WORKDIR="$VMS_WORK_DIR/VM-$VMID"
#Step 1. Check if the dir exists. if so, delete it and 
#recreate it. if not, create it directly.
if [ -d $WORKDIR ]; then
	if rm -rf $WORKDIR >/dev/null 2>&1
	then 
                LOG_MSG "Remove dir Successfully";
		mkdir -p $WORKDIR >/dev/null 2>&1
	else
		LOG_MSG "Fail to remove dir, check your dir's permission!";
		return 255;
	fi
else
	mkdir $WORKDIR >/dev/null 2>&1
fi
}

function lock_req(){
if [ $# != 1 ];then
   LOG_MSG "$* parameter error"
fi
local LOCKDIR=$1
local counter=0
while :;do
   if mkdir $LOCKDIR > /dev/null 2>&1
   then
   	LOG_MSG "get lock:$LOCKDIR"
   	return 0
   else
   	LOG_MSG "can not get lock:$LOCKDIR"
   	BASEDIR=`dirname $LOCKDIR`
   	DIR=`basename $LOCKDIR`
   	TMP=`find $BASEDIR -mmin +1 -name "$DIR"`
        if [ "$TMP" != "" ]
      	then
            LOG_MSG "$LOCKDIR expire"
            rm -rf $LOCKDIR
            break
        else
            LOG_MSG "wait some time"
            counter=`expr $counter + 1`
    	    if [ $counter -eq 60 ];then
        	LOG_MSG "log req  timeout"
        	return 255;
    	    fi
            sleep 1
        fi
   fi
done
}

function lock_release(){
if [ $# != 1 ];then
   LOG_MSG "$* parameter error"
fi
local LOGDIR=$1
rm -rf $LOGDIR
}
#LOGFILE=$VM_OPS_LOG_DIR/vm_ops.log
#function LOG_MSG()
#{
#echo "[`date "+%Y/%m/%d %H:%M:%S"`] $*" >> $LOGFILE
#}

#lock_req /tmp/locktest
#lock_release /tmp/locktest
#lock_req /tmp/locktest

