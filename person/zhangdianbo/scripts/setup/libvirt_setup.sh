#set -x
  
TOP_DIR=${1:-$(cd $(dirname $0)/.. && pwd)}
DEST_DIR=${2:-$(cd $TOP_DIR/.. && pwd)}
LIBVIRT_SRC=${3:-"git://libvirt.org/libvirt.git"}

source $TOP_DIR/functions
 
libvirt_setup()
{
    TOP_DIR="$1" 
    DEST_DIR="$2"
    LIBVIRT_SRC="$3" 
  
  ###########
  # libvirt #
  ###########

  dealwith_source_or_Exit git clone $LIBVIRT_SRC libvirt
  Exit_on_Failure "dealwith_source_or_Exit git clone $LIBVIRT_SRC libvirt"
  
  cd $DEST_DIR
  cd libvirt
  
  if ! are_all_files_exist\
    tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.args\
    tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.xml\
    src/storage/storage_backend_hlfs.c\
    src/storage/storage_backend_hlfs.h\
    tests/storagebackendhlfstest.c\
    tests/storagepoolxml2xmlin/pool-hlfs.xml\
    tests/storagepoolxml2xmlout/pool-hlfs.xml\
    tests/storagevolxml2xmlin/vol-hlfs.xml\
    tests/storagevolxml2xmlout/vol-hlfs.xml
  then
    #patch_1      
    git reset --hard v1.0.1
    Exit_on_Failure "git reset --hard v1.0.1"
    rm tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.args
    rm tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.xml
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch" 
    git apply hlfs_driver_for_libvirt_network_disk.patch
    Exit_on_Failure "git apply hlfs_driver_for_libvirt_network_disk.patch" 
     
    #patch_2 
    rm src/storage/storage_backend_hlfs.c
    rm src/storage/storage_backend_hlfs.h
    rm tests/storagebackendhlfstest.c
    rm tests/storagepoolxml2xmlin/pool-hlfs.xml
    rm tests/storagepoolxml2xmlout/pool-hlfs.xml
    rm tests/storagevolxml2xmlin/vol-hlfs.xml
    rm tests/storagevolxml2xmlout/vol-hlfs.xml   
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch"
    git apply hlfs_driver_for_libvirt_offline_storage.patch
    Exit_on_Failure "git apply hlfs_driver_for_libvirt_offline_storage.patch"
     
    #patch_3
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch"
    git apply hlfs_driver_for_libvirt_add_classpath.patch
    Exit_on_Failure "git apply hlfs_driver_for_libvirt_add_classpath.patch" 
    
    ./autogen.sh
    Exit_on_Failure "libvirt autogen.sh"  
  fi
  

  ./configure
  Exit_on_Failure "libvirt configure"
  make -j $(cpu_cores_minus_one) 
  sudo  make install -j  $(cpu_cores_minus_one)
  Exit_on_Failure "libvirt make install" 
  
  sudo ldconfig
  Exit_on_Failure "sudo ldconfig"
}

libvirt_setup $TOP_DIR $DEST_DIR $LIBVIRT_SRC
Exit_on_Failure "libvirt_setup $TOP_DIR $DEST_DIR $LIBVIRT_SRC"
