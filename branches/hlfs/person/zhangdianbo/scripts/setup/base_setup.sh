#!/usr/bin/env bash

TOP_DIR=${1:-$(cd $(dirname $0)/.. && pwd)}
DEST_DIR=${2:-$(cd $TOP_DIR/.. && pwd)}
PROBE=${3:-false}

source $TOP_DIR/functions
 
function base_setup()
{
  TOP_DIR="$1"
  DEST_DIR="$2"
  PROBE="$3"
 
  sudo_v_damen
  Exit_on_Failure "sudo_v_damen"

  apt_sources_setup $TOP_DIR $PROBE

  Exit_on_Failure "apt_sources_setup $TOP_DIR $PROBE"
  
  develop_tools_setup
  Exit_on_Failure "develop_tools_setup"

}

#Input: $1:$TOP_DIR $2:$PROBE
#Output: $SRC_FILE
function apt_sources_setup()
{
  typeset TOP_DIR=$1
  typeset PROBE=$2
  typeset ORIGIN_SRC="/etc/apt/sources.list"
  typeset APT_DIR=$TOP_DIR/apt
  typeset DOMAIN_DIR=$APT_DIR/domain
  typeset DOMAIN_FILE=$DOMAIN_DIR/domain.list
  typeset SORTED_FILE=$DOMAIN_DIR/sorted.list
  typeset SELECTED_FILE=$DOMAIN_DIR/selected.sources.file

  ! $PROBE && [[ -e $SELECTED_FILE ]] && return
 
  # Input:  $1: ORIGIN_SRC $2:$APT_DIR
  # Input:  $SRC_LST
  assert_sources_list_added_or_Exit $ORIGIN_SRC $APT_DIR 
  
  
  # Input:  $1:$APT_DIR $2:$DOMAIN_DIR $3:$DOMAIN_FILE
  # Output: $DOMAIN_FILE
  assert_domain_borned_from_sources_list_or_Exit $APT_DIR $DOMAIN_DIR $DOMAIN_FILE
  
  #Input:   $1:DOMAIN_FILE  $2:SORTED_FILE  $3:TEST_TIMES
  #Output:  SORTED_FILE
  probe_link_speed_input_domains_and_output_sorted $DOMAIN_FILE $SORTED_FILE 10
  
  # Inputput: $1:$SORTED_FILE $2:$ORIGIN_SRC  $3:$SELECTED_FILE
  # Output:   $SRC_FILE
  assert_the_fast_sources_list_or_Exit $SORTED_FILE $ORIGIN_SRC $SELECTED_FILE

}

# prepare develop enviroment
develop_tools_setup()
{
  # the package needed by worker
  assert_packages_installed_or_Exit aptitude
  Exit_on_Failure "assert_packages_installed_or_Exit aptitude"
  
  # these package needed by developer
  assert_packages_installed_or_Exit build-essential git subversion cmake libglib2.0-dev libsnappy-dev liblog4c-dev #tightvnc-java
  Exit_on_Failure "assert_packages_installed_or_Exit build-essential git subversion cmake libglib2.0-dev libsnappy-dev liblog4c-dev tightvnc-java"
  
  # these package needed by qemu
  assert_packages_installed_or_Exit libpixman-1-dev zlib1g-dev
  Exit_on_Failure "assert_packages_installed_or_Exit libpixman-1-dev zlib1g-dev" 
 
  # these package needed by libvirt
  assert_packages_installed_or_Exit libtool autoconf automake autopoint xsltproc libpciaccess-dev libnl-dev w3c-dtd-xhtml libxml2-utils libxml2-dev gettext libgcrypt11-dev python-dev libgnutls28-dev libdevmapper-dev libyajl-dev
  Exit_on_Failure "assert_packages_installed_or_Exit libtool autoconf automake autopoint xsltproc libpciaccess-dev libnl-dev w3c-dtd-xhtml libxml2-utils libxml2-dev gettext libgcrypt11-dev python-dev libgnutls28-dev libdevmapper-dev libyajl-dev"
  
  # the package needed by hadoop
  assert_java_installed_or_Exit
  Exit_on_Failure "assert_java_installed_or_Exit"
}

base_setup $TOP_DIR $DEST_DIR $PROBE
Exit_on_Failure "base_setup $TOP_DIR $DEST_DIR $PROBE" 