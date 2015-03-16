## Elastic Cloud Manage System（ECMS） 详细设计 ##


---

### 数据表的设计 ###
#### 1.网络资源表（NETWORK\_RESOURCE） ####
| **字段名称** | **约束** | **类型** | **默认值** | **描述** |
|:-----------------|:-----------|:-----------|:--------------|:-----------|
| **ip\_addr** | 主键 | 字符串 |  | ip地址 |
| **mac\_addr** |  | 字符串 |  | mac地址 |
| **is\_free** |  | 布尔 | true | 表示当前ip和mac地址是否空闲 |

#### 2.宿主机节点信息表（NODE\_RESOURCE） ####
| **字段名称** | **约束** | **类型** | **默认值** | **描述** |
|:-----------------|:-----------|:-----------|:--------------|:-----------|
| **host\_name** | 主键 | 字符串 |  | 唯一标识宿主机的名字 |
| **ip\_addr** |  | 字符串 |  | 宿主机ip地址 |
| **total\_ssize** |  | uint(32) |  | 总磁盘大小 |
| **total\_msize** |  | uint(32) |  | 总内存大小 |
| **free\_ssize** |  | uint(32) |  | 空闲磁盘大小|
| **status** |  | running|down|exception|unknown |  | node状态|
| **create\_time** |  | timestamp |  | 创建时间戳 |
| **update\_time** |  | timestamp |  | 更新时间戳 |

#### 3.虚拟机资源配置表（VM\_RESOURCE\_CONFIG） ####
| **字段名称** | **约束** | **类型** | **默认值** | **描述** |
|:-----------------|:-----------|:-----------|:--------------|:-----------|
| **vm\_id** | 主键 | 字符串 |  | 唯一标识虚拟机的名字 |
| **vcpu\_count** |  | uint(32) |  | 虚拟cpu个数 |
| **mem\_size** |  | uint(32) |  | 虚拟内存大小 |
| **disk\_size** |  | uint(32) |  | 系统盘大小（当前实现为固定大小） |
| **user\_password** |  | 字符串 |  | 虚拟机的密码 |
| **user\_hostname** |  | 字符串 |  | 虚拟机的主机名 |
| **ip\_address** |  | 字符串 |  | 虚拟机的ip地址 |
| **mac\_address** |  | 字符串 |  | 虚拟机的mac地址|
| **os\_type** |  | ubuntu|centos |  | 操作系统类型（暂只支持ubuntu和centos） |
| **update\_time** |  | timestamp |  | 更新时间戳 |


#### 4.虚拟机运行环境信息表（VM\_RUNENV\_INFO） ####
| **字段名称** | **约束** | **类型** | **默认值** | **描述** |
|:-----------------|:-----------|:-----------|:--------------|:-----------|
| **vm\_id** | 主键 | 字符串 |  | 唯一标识虚拟机的名字 |
| **host\_name** |  | 字符串 |  | 宿主机名称|
| **vnc\_port** |  | uint(32) |  | vnc端口 |
| **vnc\_password** |  | 字符串 |  | vnc密码 |
| **vm\_status** |  | running|stopped|suspend|unknown |  | 虚拟机状态（该字段以后移到虚拟机状态表中） |
| **vm\_vnc\_port** |  | uint |  | vnc软件的端口号 |


#### 5.虚拟磁盘信息表（VDISK\_INFO） ####
| **字段名称** | **约束** | **类型** | **默认值** | **描述** |
|:-----------------|:-----------|:-----------|:--------------|:-----------|
| **disk\_id** | 主键 | 字符串 |  | 唯一标识磁盘的名字 |
| **disk\_uri** |  | 字符串 |  | 虚拟磁盘的uri |
| **disk\_size** |  | 字符串 |  | 虚拟磁盘的大小 |
| **attached\_vm\_id** |  | 字符串 |  | 被挂载到的虚拟机名字 |
| **update\_time** |  | timestamp |  | 更新时间戳 |





---

### 脚本的设计 ###
涉及到的脚本有：

#### 1.get\_node\_resource.sh ####
| **脚本名称** | get\_node\_info.sh |
|:-----------------|:-------------------|
| **作用** | 获取宿主机状态信息 |
| **接受参数** | {node\_name} |
| **返回值** | {ok,Hostname,CpuCount,IP,TotalMem,TotalDise,FreeMem,FreeDisk|{error,Reason} |
| **结果影响** |  |
| **借助工具** | xm  |
| **实现描述** |```
```|


#### 2.vm\_start.sh ####
| **脚本名称** | vm\_start.sh |
|:-----------------|:-------------|
| **作用** | 创建并启动虚拟机 |
| **接受参数** |```
 {vm_id,vcpu_count,mem_size,vm_password,vm_hostname,ip_addr,mac_addr,vnc_port,vnc_password,os_type} ```|
| **返回值** | ok|{error,Reason} |
| **结果影响** |  |
| **借助工具** |  |
| **实现描述** | ```
1.检查是否有对应的工作目录，如果有则删除，并重新创建工作目录并切入；``````
2.判断是否为第一次启动(可根据系统增量盘是否存在判断是否第一次创建)，不是首次启动跳至第7步骤；如果是首次启动继续第3步骤；``````
3.制作场景化IOS—将vm_password和vm_hostname参数，写入一个配置文件initialize，mkisofs将此文件制作成一个iso镜像，镜像名字使用vm_id+'.iso'；``````
4.执行mkfs.hlfs -u hdfs://sysdisk_uri -b 8192 -s 67108864 -m 1024 (大小固定死)创建系统磁盘，然后根据sysdisk_uri，使用tap-ctl create + sysdisk_uri来连接hlfs和本地虚拟机块设备，截取返回值；``````
5.格式化上述虚拟块设备，挂载虚拟块设备到当前工作目录下sysdisk目录下；``````
6.在sysdisk目录下，根据os_type,选择合适的母盘镜像，使用vhd-util snapshot在系统目录创建增量盘（增量盘name使用vm_id+“.sys.img”）``````
7.根据vm_id,vcpu_cpunt,mem_size,ip_addr,mac_addr,vnc_password，增量盘等信息创建虚拟机的启动配置文件，存入工作目录；``````
8.xm create +配置文件 启动虚拟机，获取虚拟机的动态id；``````
9.（如果是第一次启动执行之，否则跳过该步骤）使用xenstore-write /local/domain/<id>/key 1,初始化变量key的值为1,dom0中循环检测等待这个值的变化，（不能是无限循环，需要等待足够长的时间，如果值没有变化返回  错误信息），如果值发生变化，卸载并销毁iso文件，返回初始化正常；``````
10.如果以上操作成功，则返回启动成功信息，否则返回失败信息。``` |

#### 3.vm\_stop.sh ####
| **脚本名称** | vm\_stop.sh |
|:-----------------|:------------|
| **作用** | 关闭虚拟机 |
| **接受参数** | {vm\_id} |
| **返回值** | ok|{error,Reason} |
| **结果影响** |  |
| **借助工具** | xm |
| **实现描述** | ```
1.检查参数合法性；``````
2.使用xm工具进行虚拟机关闭；``````
3.等待一定时间，查看虚拟机状态，查看是否关闭，如果是，继续第4步骤，否则继续第3步骤；``````
4.如果以上操作成功，返回成功信息，否则返回失败信息。``` |

#### 4.vm\_destroy.sh ####
| **脚本名称** | vm\_destroy.sh |
|:-----------------|:---------------|
| **作用** | 销毁虚拟机 |
| **接受参数** | {vm\_id} |
| **返回值** | ok|{error,Reason} |
| **结果影响** |  |
| **借助工具** |  |
| **实现描述** | ```
``` |

#### 5.check\_vm\_statu.sh ####
| **脚本名称** | check\_vm\_statu.sh |
|:-----------------|:--------------------|
| **作用** | 检查vm状态 |
| **接受参数** | {vm\_id} |
| **返回值** | "RUNING"|"SUSPEND"|“NO\_FIND"|
| **结果影响** |  |
| **借助工具** |  |
| **实现描述** | ```
``` |



---

### 虚拟机管理命令的设计 ###
#### Master Service命令设计 ####

##### 1.add\_node\_2m\_req #####
| **命令号** | add\_node\_2m\_req |
|:--------------|:-------------------|
| **描述** | 增加node controller节点，也就是增加宿主机|
| **参数** | node\_name |
| **备注** | 客户端发向master service |
| **请求包** | 见add\_node\_2m\_req in cloudMaster/include/cmd\_schema.hrl|
| **应答包** | ok|{error,Reason}|
| **应用场景** | 添加新节点的时候 |
| **步骤** |```
1.master远程启动node上的node_controller服务``````
2.master获取node的物理资源信息—通过get_node_info_2s_req请求``````
3.将node资源信息记录到原数据表node_resource中```|
| **异常处理** |  |

##### 2.vm\_create\_2m\_req #####
| **命令号** | vm\_create\_2\_m |
|:--------------|:-----------------|
| **描述** | 根据给定参数，在master中记录vm 配置信息 |
| **参数** | {vcpu\_count,mem\_size,vm\_password,vm\_hostname,os\_type} |
| **备注** | 客户端发向master service |
| **请求包** | 见vm\_create\_2m\_req in cloudMaster/include/cmd\_schema.hrl|
| **应答包** | {ok,Vm\_Id}|{error,Reason}|
| **应用场景** | 客户请求创建虚拟机时 |
| **步骤** | ```
将vm的用户给定配置信息记录到vm_res_config表中``` |
| **异常处理** |  |

##### 3.vm\_start\_2m\_req #####
| **命令号** | vm\_start\_2\_m |
|:--------------|:----------------|
| **描述** | 根据给定的虚拟机名字，启动指定的虚拟机 |
| **参数** | {vm\_id} |
| **备注** | 客户端发向master service  |
| **请求包** | 见vm\_start\_2m\_req in cloudMaster/include/cmd\_schema.hrl|
| **应答包** | {ok,Vm\_Id}|{error,Reason} |
| **应用场景** | 客户请求启动自己创建的虚拟机时 |
| **步骤** | ```
1.申请网络地址资源``````
2.获取给定vm的用户配置信息``````
3.获取符合要求的宿主机节点``````
4.构造请求下发给node—vm_start_2s_req``````
5.成功后，则更新vm_res_config表的网络地址；更新vm_runenv_info表中的宿主节点、vnc等信息```   |
| **异常处理** | 创建过程中碰到错误，需要将所申请的网络资源和节点物理资源归还 |


#### Node Controller命令设计 ####
##### 1.get\_node\_info\_2s\_req #####
| **命令号** | get\_node\_info\_2s\_req |
|:--------------|:-------------------------|
| **描述** | 获取node节点的状态信息，返回给master |
| **参数** |  |
| **备注** | master service发向node controller |
| **请求包** |见get\_node\_info\_2s\_req in cloudMaster/include/cmd\_schema.hrl|
| **应答包** |{ok,get\_node\_info\_2s\_res\_content\_t}|{error|Reason}|
| **引用场景** | 当master请求获取node资源信息时 |
| **步骤** | ```
调用get_node_resource.sh脚本``` |
| **异常处理** |  |

##### 2.vm\_start\_2s\_req #####
| **命令号** | vm\_start\_2\_s |
|:--------------|:----------------|
| **描述** | 创建虚拟机，并且启动创建的虚拟机 |
| **参数** |  |
| **备注** | master service发向node controller |
| **请求包** |见vm\_start\_2s\_req in cloudMaster/include/cmd\_schema.hrl |
| **应用场景** | 当master请求node启动虚拟机时 |
| **步骤** | ```
调用vm_start.sh脚本``` |
| **异常处理** |  ||\|


---

wroten by 孙建刚