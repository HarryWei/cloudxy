# cloudxy
Automatically exported from https://code.google.com/p/cloudxy/

#Cloudxy

Cloudxy is a generic and open source platform which can provide adjustable compute capacity in the cloud. It means you can scale capacity as your computing requirements change. Also, you can recover your virtual machine at any snapshot point when failure occurs.

Cloudxy is constructed of HLFS(HDFS-based Log-structured file system) and ECMS(Elastic Cloud Management System).

###HLFS

The subsystem HLFS(actually, Block level Storage System seems more proper than File System) is a distributed VM image storage system for ECMS,which provides highly available block level storage volumes that can be attached to XEN virtual machines by its tapdisk driver.Similar project related to KVM is sheepdog，but they are in different architectures.

Compared with sheepdog, HLFS has greater scalability and reliability than sheepdog by now,as we are on the shoulder of hadoop distribute file system (HDFS).Meanwhile,HLFS also supports advanced volume management features such as snapshot(HLFS can also support snapshot tree)、cloning、thin provisioning and cache.

The main idea of HLFS is:

    Take advantage of Log-structured File System's ideology to build an on-line image storage system on HDFS which can guarantee the reliability and scalability for our storage system
    The ideology of LFS makes our storage system support random access to online images.
    The ideology of LFS also makes our storage system more efficient and easily take snapshot. 

###ECMS

The subsystem ECMS is a virtual machine management system used in HLFS storage environment. The current work for ECMS is to smart schedule and life-cycle manage virtual machine

The further goal for ECMS is to build Virtual IDC, which will include develop virtual resource define language and virtual resource visualization management. we wish user can define their own virtual resource (for example,a single instance mysql service,or a master-slave mysql service,or else anything,they are all belong to virtual resource),and can instantiate、reuse、deploy virtual resource.


Authors: Hua Kang <kanghua151@gmail.com> and Weiwei Jia <harryxiyou@gmail.com>
