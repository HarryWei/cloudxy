#!/usr/bin/env bash

# A OneKey Install Kit for hlfs and concerned
#
# --dest-dir        	dest workdir 	default .
# --source-hlfs 			hlfs_dir 		  default https://cloudxy.googlecode.com/svn/trunk/hlfs/
# --source-qemu 			qemu_dir		  default	git://git.qemu.org/qemu.git
# --source-libvirt		libvirt_dir	  default git://libvirt.org/libvirt.git
# --source-backup		  back_dir		  default work_dir/backup
# --probe             true          default false

set -x 
CUR_DIR=$(pwd)

TOP_DIR=$(cd $(dirname $0) && pwd)

DEFAULT_DIR=$(cd .. && pwd)

LOG_DIR=$TOP_DIR/log
[[ -e $LOG_DIR && -d $LOG_DIR ]] || mkdir $LOG_DIR || exit 1

CUR_LOG=$LOG_DIR/$(date "+%Y%m%d-%H%M%S")

cd $CUR_DIR

source $TOP_DIR/functions

function usage()
{
        cat << END
DESCRIPTION
        hlfs.sh is a so called onekey install kit for hlfs block storage
        system, with add it's driver in qemu, libvirt so far openstack   
OPTIONS
         -d, --dest-dir
                Specify the dest workspace directory, in witch source code will
                download and build, if you would not specify it, where 
                cloudxy will be used as default workspace.
                                                        
         -h, --source-hlfs
                Provide local directory, if you have svn checkout it from
                https://cloudxy.googlecode.com/svn/trunk/hlfs， hlfs.sh
                would not destory it, only copy deeply it.
                otherwise, hlfs.sh will checkout it a brand new

         -q, --source-qemu
                Provide the faster sources or local place on which your have 
                git clone, otherwise,hlfs.sh will git clone it from the offical
                site  git://git.qemu.org/qemu.git
                                
         -l, --source-libvirt
                And the like two of above, git clone libvirt is a long time
                in GFW China Side, fortunately, hlfs.sh will git clone it for
                you and keep it clean in workspace/backup with hlfs and qemu,
               default .
               
         -p, --probe
                Thanks for my lovely and Powerful GOV, it make the new Berlin 
                Wall, Thanks for the Roaring Beast Fang, if you Encounter the
                same net problem, such as sudo apt-get update failure, can't
                install package, try it. I do think you need this, because I
                encounter with the hell hundreds.
                 
END
}

TEMP=$(getopt -o d:h:q:l:p --long dest-dir-long:,source-hlfs-long:,source-qemu-long:,source-libvirt-long:,probe-long -n 'hlfs.sh' -- "$@")
if [ $? != 0 ] ; then echo "Terminating..." >&2 ;  usage;  exit 1 ; fi

eval set -- "$TEMP"

while true
do
	case "$1" 
	in
		-d|--dest-dir-long)           DEST_DIR="$2";    shift 2;;
		-h|--source-hlfs-long)    HLFS_SRC="$2";    shift 2;;
		-q|--source-qemu-long)    QEMU_SRC="$2";    shift 2;;
		-l|--source-libvirt-long) LIBVIRT_SRC="$2"; shift 2;; 
		-p|--probe-long)          PROBE=true;       shift 1;;
		--) shift; break ;;
		*) echo "Internal error!"; usage; exit 1;;
	esac
done


:${DEST_DIR:="$DEFAULT_DIR"}
[[ -e $DEST_DIR && -d $DEST_DIR ]] || mkdir -p $DEST_DIR
Exit_on_Failure "DEST_DIR: $DEST_DIR is Ready"

DEST_DIR=$(cd $DEST_DIR && pwd)
cd $CUR_DIR


[[ -d $HLFS_SRC ]] && HLFS_SRC=$(cd $HLFS_SRC && pwd)
:${HLFS_SRC:="https://cloudxy.googlecode.com/svn/trunk/hlfs"}
cd $CUR_DIR

[[ -d $QEMU_SRC ]] && QEMU_SRC=$(cd $QEMU_SRC && pwd)
:${QEMU_SRC:="git://git.qemu.org/qemu.git"}
cd $CUR_DIR

[[ -d $LIBVIRT_SRC ]] && LIBVIRT_SRC=$(cd $LIBVIRT_SRC && pwd)
:${LIBVIRT_SRC:="git://libvirt.org/libvirt.git"}
cd $CUR_DIR

PROBE=${PROBE:-false}

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg' no effective" ; done



SETUP_DIR=$TOP_DIR/setup

source $SETUP_DIR/base_setup.sh $TOP_DIR $DEST_DIR $PROBE
Exit_on_Failure "source $SETUP_DIR/base_setup.sh $TOP_DIR $DEST_DIR $PROBE"

source $SETUP_DIR/hlfs_setup.sh $TOP_DIR $DEST_DIR $HLFS_SRC
Exit_on_Failure "source $SETUP_DIR/hlfs_setup.sh $TOP_DIR $DEST_DIR $HLFS_SRC"

source $SETUP_DIR/qemu_setup.sh $TOP_DIR $DEST_DIR $QEMU_SRC
Exit_on_Failure "source $SETUP_DIR/qemu_setup.sh  $TOP_DIR $DEST_DIR $QEMU_SRC"

source $SETUP_DIR/libvirt_setup.sh $TOP_DIR $DEST_DIR $LIBVIRT_SRC 
Exit_on_Failure "source $SETUP_DIR/libvirt_setup.sh $TOP_DIR $DEST_DIR $LIBVIRT_SRC"

TEST_DIR=$TOP_DIR/test

source $TEST_DIR/hlfs_test.sh

cd $CUR_DIR
