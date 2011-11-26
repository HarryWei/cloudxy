1 create /tmp/testenv directory
2 build/mkfs.lhfs -l local:///tmp/testenv -f testfs  -s 67108864  -b 8196
3 ./test.lhfs
