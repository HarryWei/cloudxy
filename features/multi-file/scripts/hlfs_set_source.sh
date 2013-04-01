#!/bin/bash

set -x

sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise main restricted" > /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise main restricted" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-updates main restricted" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-updates main restricted" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise universe" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise universe" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-updates universe" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-updates universe" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-updates multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-updates multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-backports main restricted universe multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-backports main restricted universe multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-security main restricted" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-security main restricted" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-security universe" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-security universe" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://mirrors.163.com/ubuntu/ precise-security multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb http://us.archive.ubuntu.com/ubuntu/ hardy multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://us.archive.ubuntu.com/ubuntu/ hardy multiverse" >> /etc/apt/sources.list'
sudo sh -c 'echo "deb-src http://mirrors.163.com/ubuntu/ precise-security multiverse" >> /etc/apt/sources.list'

sudo apt-get update
