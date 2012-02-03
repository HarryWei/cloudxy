#!/bin/bash
#This shell script runs all test cases one by one. Before you run it, you must
#have built all test cases.
#By kelvin <kelvin.xupt@gmail.com>
#2012.1.2

for log in log.*
do
if test -f $log
then
	rm $log
	echo "rm $log finished"
	sleep 1
fi
done

if test -f result.txt
then
	rm result.txt
	echo "rm result.txt finished"
	sleep 1
fi

var=./test_*
for i in $var
do
	$i >> result.txt
done
