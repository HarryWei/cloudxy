#!/bin/bash
LOGFILE=$VM_OPS_LOG_DIR/vm_ops.log
function LOG_MSG()
{
echo "[`date "+%Y/%m/%d %H:%M:%S"`] $*" >> $LOGFILE
}
#LOG_MSG $*
