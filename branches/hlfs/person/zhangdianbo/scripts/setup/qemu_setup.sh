#set -x

TOP_DIR=${1:-$(cd $(dirname $0)/.. && pwd)}
DEST_DIR=${2:-$(cd $TOP_DIR/.. && pwd)}
QEMU_SRC=${3:-"git://git.qemu.org/qemu.git"}

source $TOP_DIR/functions

qemu_setup()
{
  TOP_DIR="$1"
  DEST_DIR="$2" 
  QEMU_SRC="$3"

  ################
  # install qemu #
  ################

  dealwith_source_or_Exit git clone $QEMU_SRC qemu
  Exit_on_Failure "dealwith_source $QEMU_SRC qemu"
  
  cd $DEST_DIR
  cd qemu
  
  # patch has been applied
  if  ! are_all_files_exist block/hlfs.c
  then
      git reset --hard v1.3.0
      Exit_on_Failure "git reset --hard v1.3.0"
  
      Wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch
      Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch"
      
      git apply hlfs_driver_for_qemu_1.3.0.patch
      Exit_on_Failure "git apply hlfs_driver_for_qemu_1.3.0.patch" 
  fi
  
  
  ./configure --enable-hlfs --with-coroutine=gthread
  Exit_on_Failure "qemu configure"
  
  make -j $(cpu_cores_minus_one)
  Exit_on_Failure "qemu make"
  
  sudo make install
  Exit_on_Failure "qemu install"
  
  sudo ldconfig
  Exit_on_Failure "ldconfig for qemu"
  
  cd $CUR_DIR
}

qemu_setup $TOP_DIR $DEST_DIR $QEMU_SRC
Exit_on_Failure "qemu_setup $TOP_DIR $DEST_DIR $QEMU_SRC"