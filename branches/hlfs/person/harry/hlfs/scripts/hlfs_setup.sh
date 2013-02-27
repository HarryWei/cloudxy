#!/usr/bin/env bash

set -x

source hlfs_funcs

function assert_files_exist()
{
    local FILE
    for FILE in "$@"
    do
	if [[ ! -e $FILE ]]
	then
	    echo "$FILE does not exist"
	    return 1
	fi 
    done
    return 0 
}

function exit_on_false()
{
    local RETURN_VALUE=$?	
    if [[ 0 -ne $RETURN_VALUE ]]; then
        echo
	echo "[ :( :( :( !!! -----HLFS ACCOUNT FAILURE----- !!! ]"
	echo "$@"
	echo "[ !!! -----HLFS ACCOUNT FAILURE----- !!! :) :) :) ]"
	echo
	cd $TOP_DIR
	set +x
	exit $RETURN_VALUE
    fi
}

function is_package_installed()
{
    if [[ -z "$1" ]]
    then
        echo "$1" is blank
	return 0
    fi

    if  ! dpkg -l "$1"
    then
        echo "$1 has NOT been installed"
	return 1
    elif dpkg -l "$1" |grep "^un"
    then
        echo "$1 has NOT been installed"
	return 1
    else
        echo "$1 HAS been installed!!!"
	return 0
    fi
}

function install_package()
{
    sudo apt-get install "$1" -y || (echo "n";sleep 3;echo "y";sleep 2;echo "y")|sudo aptitude install "$1"
    exit_on_false "install $1 failure"
}

function is_ppa_installed()
{
    if [[ -z "$@" ]]; then
        return 1
    fi
    local STR="$@"
    local CODENAME=$(lsb_release -cs)
    local SUB_STR_1=${STR#"ppa:"}
    local SUB_STR_2=${SUB_STR_1/"/"/"-"}
    if [[ -e /etc/apt/sources.list.d/${SUB_STR_2}-${CODENAME}.list ]]
    then
        return 0
    else
        return 1
    fi
}

function assert_ppa_installed()
{
    is_ppa_installed "$1" || echo "\n" | sudo add-apt-repository "$1"
    exit_on_false
}

#function recognize_32_64()
#{
#    local VALUE_32=$1
#    local VALUE_64=$2
#
#    if [[ $(getconf LONG_BIT) -eq 32 ]]; then
#        echo $VALUE_32
#    elif [[ $(getconf LONG_BIT) -eq 32 ]]; then
#		echo $VALUE_64
#	else
#		echo "Error, now, we just support 32 or 64 bits OS!\n"
#    fi
#}

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

function Wget()
{   
    local URL="$1"
    local FILENAME=$(basename $URL)    
    if [ -e basename ]
    then  
        echo $FILENAME has been existed
        return 0
    else
        wget -c $URL
    fi
}

function assert_hadoop_installed()
{
    if asseert_packages_installed_or_Exit hadoop-0.20-conf-pseudo
    then    
        echo hadoop has been installed
    else    
	Wget http://archive.cloudera.com/cdh4/one-click-install/precise/amd64/cdh4-repository_1.0_all.deb
        Exit_on_Failure "Wget cdh4-repository_1.0_all.deb failure"
	
	sudo dpkg -i cdh4-repository_1.0_all.deb
	Exit_on_Failure "dpkg -i cdh4-repository_1.0_all.deb failure"
       
	assert_package_installed curl
	curl -s http://archive.cloudera.com/cdh4/ubuntu/precise/amd64/cdh/archive.key | sudo apt-key add -
	Exit_on_Failure "apt-key add ...... failure"
        sudo apt-get update
        
	install_package  hadoop-0.20-conf-pseudo
	Exit_on_Failure "install hadoop-0.20-conf-pseudo failure"
        
	sudo sed -i "s*# Attempt to set JAVA_HOME if it is not set*# Attempt to set JAVA_HOME if it is not set\nJAVA_HOME=${JAVA_HOME:-/usr/lib/jvm/java-7-oracle}*g" /usr/lib/hadoop/libexec/hadoop-config.sh
	Exit_on_Failure "set JAVA_HOME in /usr/lib/hadoop/libexec/hadoop-config.sh failure"
    fi
}

function assert_hadoop_start_4_2developing()
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
	    exit_on_false "$SERVICE start failure"
    	done
    fi

    # conffig CLASSPATH of hdfs for compiling hlfs 
    hadoop_classpath
    exit_on_false "set hadoop_classpath failure"
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
    exit_on_false "create workenv failure"
	
    assert_package_installed libhdfs0-dev
}
    
function dealwith_source()
{
    local TOOL="$1"
    local COMMAND="$2"
    local ADDRESS="$3"
    local DEST_DIR="$4"

    if [[ (-e "$DEST_DIR" && -d "$DEST_DIR") ]]
    then
        cd "$DEST_DIR"
        case "$TOOL" in
            "svn")    svn info;;
            "git")    git status;;
        esac
	      cd -
	      #exit_on_false "update or pull  $DEST_DIR source failure"  
    else
	      $TOOL $COMMAND $ADDRESS $DEST_DIR
    fi
}

TOP_DIR=$(cd $(dirname "$0") && pwd)
echo "999 TOP_DIR is $TOP_DIR"

##Replace source list, update && upgrade


##############################
# prepare develop enviroment #
##############################

# the package needed by worker
assert_package_installed aptitude

# these package needed by developer
assert_package_installed build-essential git subversion cmake libglib2.0-dev libsnappy-dev liblog4c-dev tightvnc-java

# these package needed by qemu
assert_package_installed libpixman-1-dev zlib1g-dev

# these package needed by libvirt
assert_package_installed libtool autoconf automake autopoint xsltproc libpciaccess-dev libnl-dev w3c-dtd-xhtml libxml2-utils libxml2-dev gettext libgcrypt11-dev python-dev libgnutls28-dev libdevmapper-dev libyajl-dev

# the package needed by hadoop
assert_java_installed

##################
# install hadoop #
##################
assert_hadoop_installed

# start hdfs and install dev for second developing
assert_hadoop_start_4_2developing


################
# install hlfs #
################

dealwith_source svn checkout https://cloudxy.googlecode.com/svn/trunk/hlfs/ hlfs

cd hlfs/build

cmake -DCMAKE_INSTALL_PREFIX=/usr/local/lib ../src
exit_on_false "hlfs cmake failure"

make
exit_on_false "hlfs make failure"

sudo make install
exit_on_false "hlfs install failure"

update_env /etc/profile HLFS_INSTALL_PATH /usr/local/lib/hlfs
exit_on_false "update_env HLFS_INSTALL_PATH for hlfs failure"
update_env /etc/profile LD_LIBRARY_PATH /usr/local/lib/hlfs/lib
exit_on_false "update_env LD_LIBRARY_PATH for hlfs failure"

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
    exit_on_false "git reset --hard v1.3.0 failure"

    Wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch
    exit_on_false "Wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch failure"
    
    git apply hlfs_driver_for_qemu_1.3.0.patch
    exit_on_false "git apply hlfs_driver_for_qemu_1.3.0.patch failure" 
fi


./configure --enable-hlfs --with-coroutine=gthread
exit_on_false "qemu configure failure"

make
exit_on_false "qemu make failure"

sudo make install
exit_on_false "qemu install failure"

cd $TOP_DIR

###########
# libvirt #
###########
dealwith_source git clone git://libvirt.org/libvirt.git libvirt

cd libvirt

if ! assert_files_exist tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.args tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.xml assert_files_exist src/storage/storage_backend_hlfs.c src/storage/storage_backend_hlfs.h tests/storagebackendhlfstest.c tests/storagepoolxml2xmlin/pool-hlfs.xml tests/storagepoolxml2xmlout/pool-hlfs.xml tests/storagevolxml2xmlin/vol-hlfs.xml tests/storagevolxml2xmlout/vol-hlfs.xml
then
    
    git reset --hard v1.0.1
    exit_on_false "git reset --hard v1.0.1 failure"
    rm tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.args
    rm tests/qemuxml2argvdata/qemuxml2argv-disk-drive-network-hlfs.xml
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch
    exit_on_false "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch failure"
    git apply hlfs_driver_for_libvirt_network_disk.patch
    exit_on_false "git apply hlfs_driver_for_libvirt_network_disk.patch failure"

    rm src/storage/storage_backend_hlfs.c
    rm src/storage/storage_backend_hlfs.h
    rm tests/storagebackendhlfstest.c
    rm tests/storagepoolxml2xmlin/pool-hlfs.xml
    rm tests/storagepoolxml2xmlout/pool-hlfs.xml
    rm tests/storagevolxml2xmlin/vol-hlfs.xml
    rm tests/storagevolxml2xmlout/vol-hlfs.xml   
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch
    exit_on_false "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch failure"
    git apply hlfs_driver_for_libvirt_offline_storage.patch
    exit_on_false "git apply hlfs_driver_for_libvirt_offline_storage.patch failure"
    
    Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch
    exit_on_false "Wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch failure"
    git apply hlfs_driver_for_libvirt_add_classpath.patch
    exit_on_false "git apply hlfs_driver_for_libvirt_add_classpath.patch failure" 
fi

./autogen.sh
exit_on_false "libvirt autogen.sh failure"

./configure 
exit_on_false "libvirt configure failure"

make 
exit_on_false "libvirt make failue"

sudo make install
exit_on_false "libvirt install failure"

cd $TOP_DIR

mkfs.hlfs -u hdfs:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024
exit_on_false "create hlfs fs failure"
echo "********************************"
echo "create hlfs fs sucessful"
rmfs.hlfs -u hdfs:///tmp/testenv/testfs
exit_on_false "rm hlfs fs failure"
echo "********************************"
echo "rm hlfs fs successful"

set +x
