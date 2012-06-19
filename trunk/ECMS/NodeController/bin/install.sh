#!/bin/sh
ROOT=`pwd`
DIR=./erts-5.8.4/bin
sed s:%FINAL_ROOTDIR%:$ROOT: $DIR/erl.src > $DIR/erl

mkdir -p ~/script
mkdir -p ~/.VMSOURCE

cp ./lib/node_controller-0.1.0/priv/script/* ~/script  -rf
cp ./lib/node_controller-0.1.0/priv/* ~/.VMSOURCE -rf

export PATH=$PATH:~/script
echo "export PATH=$PATH:~/script" >> ~/.bashrc
