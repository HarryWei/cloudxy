#!/usr/bin/env bash

set -x

source hlfs_funcs

function assert_hadoop_installed_or_Exit()
{
    if asseert_packages_installed_or_Exit hadoop-0.20-conf-pseudo
    then    
        echo hadoop has been installed
    else    
      Wget http://archive.cloudera.com/cdh4/one-click-install/precise/amd64/cdh4-repository_1.0_all.deb
      Exit_on_Failure "Wget cdh4-repository_1.0_all.deb failure"
	    
	    sudo dpkg -i cdh4-repository_1.0_all.deb
	    Exit_on_Failure "dpkg -i cdh4-repository_1.0_all.deb failure"
       
      assert_packages_installed_or_Exit curl
	    curl -s http://archive.cloudera.com/cdh4/ubuntu/precise/amd64/cdh/archive.key | sudo apt-key add -
	    Exit_on_Failure "apt-key add ...... failure"
      
      assert_apt_get_update_or_Exit
        
	    install_package_or_Exit  hadoop-0.20-conf-pseudo
        
	    sudo sed -i "s*# Attempt to set JAVA_HOME if it is not set*# Attempt to set JAVA_HOME if it is not set\nJAVA_HOME=${JAVA_HOME:-/usr/lib/jvm/java-7-oracle}*g" /usr/lib/hadoop/libexec/hadoop-config.sh
	    Exit_on_Failure "set JAVA_HOME in /usr/lib/hadoop/libexec/hadoop-config.sh failure"
    fi
}

function assert_hadoop_start_4_2developing_or_Exit()
{
    # start hdfs 


    local SERVICE
    for SERVICE in /etc/init.d/hadoop-hdfs-*
    do 
	sudo $SERVICE start
    done
    
    if ! $?
    then
    	# defend has fs existed and with some error
    	sudo killall -9 java;
    	sudo rm /var/lib/hadoop/cache/* -rf1
    	echo "Y"|sudo -u hdfs hdfs namenode -format
	for SERVICE in /etc/init.d/hadoop-hdfs-*
   	do 
	    sudo $SERVICE start
	    Exit_on_Failure "$SERVICE start failure"
    	done
    fi

    # conffig CLASSPATH of hdfs for compiling hlfs 
    hadoop_classpath
    Exit_on_Failure "set hadoop_classpath failure"
    # if you wangt see execute process and not set +x, you will :(
    
    set +x
    echo "source /etc/profile BEGIN"
    source /etc/profile
    echo "source /etc/profile END"
    set -x

    # create workenv
    sudo -u hdfs hadoop fs -mkdir /tmp
    sudo -u hdfs hadoop fs -chmod -R 1777 /tmp
    #sudo -u hdfs hadoop fs -mkdir /tmp/testenv
    #sudo -u hdfs hadoop fs -ls /tmp/testenv
    hadoop fs -mkdir /tmp/testenv
    hadoop fs -ls /tmp/testenv
    Exit_on_Failure "create workenv failure"
	
    assert_packages_installed_or_Exit libhdfs0-dev
}


function hadoop_classpath()
{
    local FILE=/etc/profile
    local NAME=CLASSPATH

    local HADOOP_CLASSPATH=$(hadoop classpath)
    local SAVED_IFS=$IFS
    IFS=:
    local TMP_VALUE
    local VALUE
    for VALUE in $HADOOP_CLASSPATH
    do
        if  TMP_VALUE=$(echo $VALUE | grep "/\.//")
        then
            VALUE=$( echo $TMP_VALUE | grep "/\.//.*\.jar")
        fi

        if  [[ -n $VALUE ]]
        then
            update_env $FILE $NAME $VALUE
        fi
    done
    IFS=$SAVED_IFS
}






    


TOP_DIR=$(cd $(dirname "$0") && pwd)

N-Cores=$(get_cpu_core_num)
((N-Cores--))
echo $N-Cores
##############################
# prepare develop enviroment #
##############################

# the package needed by worker
assert_packages_installed_or_Exit aptitude

# these package needed by developer
assert_packages_installed_or_Exit build-essential git subversion cmake libglib2.0-dev libsnappy-dev liblog4c-dev tightvnc-java

# these package needed by qemu
assert_packages_installed_or_Exit libpixman-1-dev zlib1g-dev

# these package needed by libvirt
assert_packages_installed_or_Exit libtool autoconf automake autopoint xsltproc libpciaccess-dev libnl-dev w3c-dtd-xhtml libxml2-utils libxml2-dev gettext libgcrypt11-dev python-dev libgnutls28-dev libdevmapper-dev libyajl-dev

# the package needed by hadoop
assert_java_installed_or_Exit

##################
# install hadoop #
##################
assert_hadoop_installed_or_Exit

# start hdfs and install dev for second developing
assert_hadoop_start_4_2developing_or_Exit


################
# install hlfs #
################

dealwith_source svn checkout https://cloudxy.googlecode.com/svn/trunk/hlfs/ hlfs

cd hlfs/build

cmake -DCMAKE_INSTALL_PREFIX=/usr/local/lib ../src
Exit_on_Failure "hlfs cmake failure"

make -j$(cpu_cores)
Exit_on_Failure "hlfs make failure"

sudo make install
Exit_on_Failure "hlfs install failure"

update_env /etc/profile HLFS_INSTALL_PATH /usr/local/lib/hlfs
Exit_on_Failure "update_env HLFS_INSTALL_PATH for hlfs failure"
update_env /etc/profile LD_LIBRARY_PATH /usr/local/lib/hlfs/lib
Exit_on_Failure "update_env LD_LIBRARY_PATH for hlfs failure"

echo "source /etc/profile BEGIN"
source /etc/profile
echo "source /etc/profile END"

sudo sh -c 'echo "/usr/local/lib/hlfs/lib/" >> /etc/ld.so.conf'
sudo sh -c "echo $JAVA_HOME/jre/lib/$(recognize_32_64 i386 amd64)/server >> /etc/ld.so.conf"
sudo ldconfig

cd $TOP_DIR

################
# install qemu #
################
dealwith_source git clone git://git.qemu.org/qemu.git qemu

cd qemu

# patch has been applied
if  ! assert_files_exist block/hlfs.c
then
    git reset --hard v1.3.0
    Exit_on_Failure "git reset --hard v1.3.0 failure"

    Wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch failure"
    
    git apply hlfs_driver_for_qemu_1.3.0.patch
    Exit_on_Failure "git apply hlfs_driver_for_qemu_1.3.0.patch failure" 
fi


./configure --enable-hlfs --with-coroutine=gthread
Exit_on_Failure "qemu configure failure"

make -j$(cpu_cores)
Exit_on_Failure "qemu make failure"

sudo make install
Exit_on_Failure "qemu install failure"

cd $TOP_DIR

###########
# libvirt #
###########
dealwith_source git clone git://libvirt.org/libvirt.git libvirt

cd libvirt

if ! assert_files_exist tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.args tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.xml assert_files_exist src/storage/storage_backend_hlfs.c src/storage/storage_backend_hlfs.h tests/storagebackendhlfstest.c tests/storagepoolxml2xmlin/pool-hlfs.xml tests/storagepoolxml2xmlout/pool-hlfs.xml tests/storagevolxml2xmlin/vol-hlfs.xml tests/storagevolxml2xmlout/vol-hlfs.xml
then
    
    git reset --hard v1.0.1
    Exit_on_Failure "git reset --hard v1.0.1 failure"
    rm tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.args
    rm tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.xml
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch failure"
    git apply hlfs_driver_for_libvirt_network_disk.patch
    Exit_on_Failure "git apply hlfs_driver_for_libvirt_network_disk.patch failure"

    rm src/storage/storage_backend_hlfs.c
    rm src/storage/storage_backend_hlfs.h
    rm tests/storagebackendhlfstest.c
    rm tests/storagepoolxml2xmlin/pool-hlfs.xml
    rm tests/storagepoolxml2xmlout/pool-hlfs.xml
    rm tests/storagevolxml2xmlin/vol-hlfs.xml
    rm tests/storagevolxml2xmlout/vol-hlfs.xml   
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch failure"
    git apply hlfs_driver_for_libvirt_offline_storage.patch
    Exit_on_Failure "git apply hlfs_driver_for_libvirt_offline_storage.patch failure"
    
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch
    Exit_on_Failure "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch failure"
    git apply hlfs_driver_for_libvirt_add_classpath.patch
    Exit_on_Failure "git apply hlfs_driver_for_libvirt_add_classpath.patch failure" 
fi

./autogen.sh
Exit_on_Failure "libvirt autogen.sh failure"

./configure 
Exit_on_Failure "libvirt configure failure"


make -j$(cpu_cores) 
Exit_on_Failure "libvirt make failue"

sudo make install
Exit_on_Failure "libvirt install failure"

cd $TOP_DIR

mkfs.hlfs -u hdfs:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024
Exit_on_Failure "create hlfs fs failure"
echo "********************************"
echo "create hlfs fs sucessful"
rmfs.hlfs -u hdfs:///tmp/testenv/testfs
Exit_on_Failure "rm hlfs fs failure"
echo "********************************"
echo "rm hlfs fs successful"

set +x
