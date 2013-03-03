#!/usr/bin/env bash
#set -x

TOP_DIR=${1:-$(cd $(dirname $0)/.. && pwd)}
DEST_DIR=${2:-$(cd $TOP_DIR/.. && pwd)}
HLFS_SRC=${3:-"https://cloudxy.googlecode.com/svn/trunk/hlfs"}

source $TOP_DIR/functions

#Input $1:$TOP_DIR $2:$DEST_DIR$ 3:$HLFS_SRC
hlfs_setup()
{ 
  typeset TOP_DIR="$1"
  typeset DEST_DIR="$2"
  typeset HLFS_SRC="$3"
  
  ##################
  # install hadoop #
  ##################
  assert_hadoop_installed_or_Exit

  ################
  # install hlfs #
  ################

  dealwith_source_or_Exit svn checkout $HLFS_SRC hlfs
  Exit_on_Failure "dealwith_source_or_Exit svn checkout $HLFS_SRC hlfs"
 
  cd $DEST_DIR
  cd hlfs/build
  
  cmake -DCMAKE_INSTALL_PREFIX=/usr/local/lib ../src
  Exit_on_Failure "hlfs cmake"
  
  source /etc/profile
 
  make -j  $(cpu_cores_minus_one)
  Exit_on_Failure "hlfs make failure"
  
  sudo make install
  Exit_on_Failure "hlfs install"
  
  update_profile HLFS_INSTALL_PATH /usr/local/lib/hlfs
  Exit_on_Failure "update_profile HLFS_INSTALL_PATH for hlfs"
  
  update_profile LD_LIBRARY_PATH /usr/local/lib/hlfs/lib
  Exit_on_Failure "update_ldconf LD_LIBRARY_PATH for hlfs"
  
  update_path /usr/local/lib/hlfs/bin
  Exit_on_Failure  "update_path /etc/profile PATH for hlfs"
    
  sudo chmod +x /usr/local/lib/hlfs/bin/*
  Exit_on_Failure "chmod +x /usr/local/lib/hlfs/bin/*"

  update_ldconf /usr/local/lib/hlfs/lib
  Exit_on_Failure "update_ldconf /etc/ld.so.conf /usr/local/lib/hlfs/lib"

}

function assert_hadoop_installed_or_Exit()
{
  if is_package_installed hadoop-0.20-conf-pseudo
  then    
    assert_hdfs_running_or_Exit
  else    
    Wget http://archive.cloudera.com/cdh4/one-click-install/precise/amd64/cdh4-repository_1.0_all.deb
    Exit_on_Failure "Wget cdh4-repository_1.0_all.deb"
      
    sudo dpkg -i cdh4-repository_1.0_all.deb
    Exit_on_Failure "dpkg -i cdh4-repository_1.0_all.deb failure"
       
    assert_packages_installed_or_Exit curl
	  curl -s http://archive.cloudera.com/cdh4/ubuntu/precise/amd64/cdh/archive.key | sudo apt-key add -
	  Exit_on_Failure "apt-key add ...... "
      
    #sudo apt-get update
    assert_apt_get_update
        
	  install_package_or_Exit hadoop-0.20-conf-pseudo
    Exit_on_Failure "install_package_or_Exit hadoop-0.20-conf-pseudo"
         
	  sudo sed -i "s*# Attempt to set JAVA_HOME if it is not set*# Attempt to set JAVA_HOME if it is not set\nJAVA_HOME=${JAVA_HOME:-/usr/lib/jvm/java-7-oracle}*g" /usr/lib/hadoop/libexec/hadoop-config.sh
	  Exit_on_Failure "set JAVA_HOME in /usr/lib/hadoop/libexec/hadoop-config.sh"
	  
    # conffig CLASSPATH of hdfs for compiling hlfs 
    hadoop_classpath
    Exit_on_Failure "set hadoop_classpath"  #We want second develop, it is needed
 
    assert_packages_installed_or_Exit libhdfs0-dev
    
    assert_hdfs_running_or_Exit
  fi
}

function assert_hdfs_running_or_Exit()
{
  # start hdfs 
  local STATUS=true
  local SERVICE
  
  for SERVICE in /etc/init.d/hadoop-hdfs-*
  do
    $SERVICE status ||sudo SERVICE start || STATUS=false
  done 
  
  if ! $STATUS
  then
    # defend has fs existed and with some error
    sudo killall -9 java;
    sudo rm /var/lib/hadoop/cache/* -rf1
    echo "Y"|sudo -u hdfs hdfs namenode -format
	  for SERVICE in /etc/init.d/hadoop-hdfs-*
   	do 
	    sudo $SERVICE start
	    Exit_on_Failure "$SERVICE start"
    done
  fi

  # create workenv
  sudo -u hdfs hadoop fs -mkdir /tmp
  sudo -u hdfs hadoop fs -chmod -R 1777 /tmp
  #sudo -u hdfs hadoop fs -mkdir /tmp/testenv
  #sudo -u hdfs hadoop fs -ls /tmp/testenv
  hadoop fs -mkdir /tmp/testenv # || Exit_on_Failure "create workenv failure"
  hadoop fs -ls /tmp/testenv
  Exit_on_Failure "test hdfs"
  

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
      update_profile $NAME $VALUE
    fi
  done
  IFS=$SAVED_IFS
}

hlfs_setup $TOP_DIR $DEST_DIR $HLFS_SRC
Exit_on_Failure "hlfs_setup $TOP_DIR $DEST_DIR $HLFS_SRC"





