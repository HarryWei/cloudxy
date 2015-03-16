# Elastic Cloud Manage System（ECMS） 概要设计 #



## 背景介绍 ##
ECMS是为用户提供按需分配物理资源的基础服务平台 ——Infrastructure As a Service。ECMS 主要借助虚拟机技术实现物理计算机资源（计算资源、存储资源、网络资源等）的再划分、隔离、以及现场分配和回收。用户以完全独占方式获得虚拟机实例（其上已运行标准操作系统），具体使用方式也由用户自裁。

## ECMS设计目标 ##
  * ECMS的初期设计目标是:管理虚拟机和管理虚拟磁盘。其与现行的诸多管理系统最大不同就在于——由于虚拟磁盘的存在，我们虚拟机调度已经与具体宿主机无关，可以在整个集群中自由迁移（离线和在线）。
  * ECMS的后期设计目标:在无需修改任何网络基础架构的前提下，自由实现虚拟的二层网络，从而能为大客户提供和其实际独立机房类似的网络环境（其上的网络管理软件、如dhcp等都可继续使用)。

## ECP设计特性 ##
### 基本设计特性 ###
  * 虚拟机特性方面
    1. 支持按用户定义的需求动态创建、销毁虚拟磁盘.
    1. 支持为给定虚拟机动态挂载、卸载虚拟磁盘.
    1. 支持按用户定义资源配置，创建和销毁虚拟机.
    1. 支持随时关闭、启动、暂停、恢复虚拟机.
  * 虚拟机集群控制系统特性方面
    1. 支持物理机集群在线扩容.
    1. 支持虚拟机和宿主机状态实时监控.
    1. 支持系统管理的元数据安全.
### 有待支持的特性 ###
  * 虚拟机特性方面
    1. 支持运行时动态改变虚拟机所使用的资源（包括内存、CPU、磁盘、网卡等）.
    1. 支持用户自定义image（目前只支持系统预置镜像）.
    1. 支持Kvm（目前支持Xen）.
  * 虚拟机集群控制系统特性方面
    1. 支持虚拟子网 —— 支持用户子组建三层／二层子网.
    1. 支持集群范围的动态负载均衡——VM需要又控制系统按负载情况动态迁移，已均匀压力.
    1. 用户权限认证体系.
    1. WEB UI系统.
## 基本架构 ##
ECMS 的系统架构采用传统的MASTER/SLAVE模式，分别对应——总控服务 Master Service  和 宿主机控制服务Node Controller.
  * Master Service负责
    1. 系统原数据管理:维护系统各种资源数据，以及控制和状态数据
    1. 系统命令处理:负责处理虚拟机控制、监控等各种管理命令处理
    1. 系统负载监控
    1. 虚拟机运行状态监控
  * Node Controller负责
    1. 按照总控服务指示执行具体虚拟机类命令
    1. 监控虚拟机运行状态
    1. 监控宿主机运行状态
## 技术选型 ##
    * 虚拟机平台我们选用业界已成熟的机器虚拟机（Machine Virtualization）平台 —— Xen.Xen平台为我们提供hypervisor、device model、管理工具等丰富的资源，而且文档和使用人群也最为广泛.
    * ECMS 实现采用面向分布系统的ERLANG语言实现,erlang 丰富的OTP库和消息处理潜质是我们实现分布式调度的不二选择
    * 虚拟机控制采用shell脚本，继承管理类命令实现（暂不采用api接口编程实现），虽然不很专业，但可满足我们当前需求
## Master Service 和 Node Controller 设计概要 ##
一些关键技术点阐述
### 1.Master Service 消息处理 ###
Master Serivce 消息处理采用：异步消息接收进程  +  多进程（erlang中是协程，不过我们文中仍称其进程吧）同步消息处理模型（类似于半同步半异步模型，只不过erlang协程很轻，所以现场创建、使用完销毁即可，不需要像传统方式那样以线程池实现之）。
### 实现提示 ###
消息处理采用Erlang 中 gen\_server 框架实现,具体实现方案可参考如下：
  * handle\_call 中spawn新进程负责具体的消息处理；
  * 回应句柄(From)需要告诉消息处理例程，以便命令执行完毕后，进行回应。
  * handle\_call中不负责消息回应，而是采用noreply模式，以防止堵塞后续命令；
伪代码片如下 ：
```
handle_call({foo,X}, From, State) -> 
    	spawn_link(fun() -> do_cmd_process(State, X, From) end),
    	{noreply, State}.  
do_cmd_process(State, X, From) ->
        % 获得node controller的reference
        {Node,Nc_name}  =  get_node_contrllor(X,State) 
        % 构造给node controller的命令消息
        {Msg_to_nc} = build_nc_msg (X,State)
        % 发送消息给node controller
        {Node,Ncname} ! Msg,
        receive
           {{Node,Ncname} , Res} ->  
               %处理从node controller的响应消息
               do_replay_process(); 
      	   {'DOWN', {Node,Nc_name}, _, _, _} ->
               %处理node 宕机事件
               do_node_down_process(); 
           after 5000 - > 
 	       %通讯超时处理 
               do_timeout_process()
         end.
         gen_server:reply(From, Reply).  %最后回应给客户端
```
注意：
  * Receive 处理属于异步消息处理，我们还可以简化成同步消息处理——采用gen\_server:call({Node,Ncname},Msg,Timeout}代替更为简便。
  * do\_cmd\_process 在单独进程中执行，且会存在修改元数据等动作，所以对元数据修改需要加锁。erlang中是采用transaction对数据库表加锁。

### 2.Node Controller 消息处理 ###
Node Controller 消息处理场模型和Master Controller一样，都属于异步消息接收进程  +  多进程（erlang中是协程）同步消息处理模型（类似于半同步半异步模型）。
其中同步消息处理用于执行虚拟机启动、关闭等耗时任务。这些任务需另起单独进程同步执行。

#### 实现提示 ####
Node Controller 和Master Serivce不同，它的消息处理主要是执行虚拟机控制脚本。伪代码片如下 ：
```
  handle_call({foo,X}, From, State) -> 
     spawn_link(fun() -> do_cmd_process(State, X, From) end),
     {noreply, State}.  
  do_cmd_process(State, X, From) ->
     %执行虚拟机管理脚本等
     os:cmd(do_vm_control.sh)
     %最后回应给master 
     gen_server:reply(From, Reply). 
```

### 3.Master Service 跨机器故障恢复(failover) ###
为了能快速异地跨机器failover(如果master service所在物理机器发生故障则需要快速地选择其他机器启动master service)，Master Service 采取“状态远程化”策略，将所有上下文数据都存储在分布冗余数据库中。虽然我们并非一个无状态服务器，但服务器状态持续化到远端数据库中能有效防止系统崩溃丢失状态数据或者硬盘损坏丢失元数据的问题。
#### 实现提示 ####
我们在erlang系统中使用其提供的mnesia分布、多副本数据库作数据存储，它的特性完全满足我们需求。我们目前配置3个数据副本即可确保数据安全。

### Master Service/Node Controller 本地故障恢复 ###
为了在本地快速地恢复服务（Master service/Node Controller服务本身异常退出后首先应该在本地重新启动）,本地机器上需要有一个看门狗监控Master service/Node Controller运行状态，一旦发现其终止则立刻重新启动恢复服务。但是如果发现连续多次启动仍无法成功则不再尝试。
#### 实现提示 ####
erlang系统中的supervisor 行为就是看门狗行为，我们的master service/Node Controller 将作为supervisor的worker 运行，一旦异常终止，立刻被重新启动。
注：supervisor我们只用于监控本地服务，而不从master节点远程监控slave节点—— 虽然erlang的supervisor 也可远程监控（跨节点），但不提倡跨节点使用（原因去google吧）。

### 4.Node Controller故障发现(心跳) ###
Master service 需要知道node controller是否活着（分布系统的普遍需求）。如果node controller已经死掉，则master service就无法下发命令，这时需要从资源池中屏蔽该节点,防止在选择该点上的资源，同时也需要报告用户等。
要实现这种快速发现对方是否活着的需求，最常用的方式是双方建立单项或双向的心跳消息（每隔一定间隔单项活着双向发送消息确认对方活着），如果失去心跳消息则认为对方以死。
### 实现提示 ###
  * erlang 中的link和moniter就是用于实现双向和单项心跳。我们只需要在master server和node controller 之间建立上述关系就能实现双方感知对方是否终止。（心跳的tick间隔erlang中也是可配置的）。对我们的场景中，master启动node上的node controller 服务时采用spawn link，即可建立连接
  * 对于我们来说，监控节点故障（而非进程）似乎更有效，因此建议由master向node方向使用montiro\_node（node,true）即可——Node 可立刻发现（进程link似乎不是立刻发现的）
  * 集群中的节点发生网络故障后（并非宕机），有可能重新上线（网络恢复）。这时就需要相互发现对方上线，从而再次建立关系。 再次感谢erlang，monitor\_node不但能发现node down,而且也能发现node up（node up目前erlang 实现中若发现，必须要去何其通讯，比如ping，所以可考虑master后台定期运行类似ping的探活机制）
  * 当master 收到node up事件后， 需要通过命令检测node是否是物理重启（查看其上的node controller是否运行——实现check alive 消息，或者pid = rpc:call(OtherNode, erlang,whereis,[control](node.md)) && is\_process\_alive(pid)），如果是物理重启则需要再次远程启动node controller服务。
  * master 如果重启，需要先从数据库中读出node信息，重新进行monitor。然后检查node controller是否活着，如果没活着，则需要远程启动。
注意：
  * Node Controller在我们服务中不需要监控Master Controller是否活着，至于状态上报时，master如果关闭，也只需重试即可（Master Controller具有唯一逻辑名，即便换机器启动了也无妨）。
  * 除了心跳发现外，master给node下发管理命令时，如果node宕机也会主动发现故障。这也可以作为一种故障发现方式。

### 5.Master Service 位置定位 ###
对于master service的位置定位我们使用逻辑名，而不是直接使用ip地址和port，这样才能做到位置透明。客户端（node controller和client api）和 master service通讯都使用逻辑地址，这样即便master service的物理位置发生变化，也不影响服务被正确寻址。 当然系统必须维护逻辑名到实际物理地址之间的映射关系，当master service发生变化时，也要改变映射关系。
#### 实现提示 ####
Erlang  系统有一个globe模块，它的主要功能之一就是提供一个服务（进程）全局注册服务。一个服务会注册一个全局唯一的逻辑名，以供访问。当服务终止时系统自动注销该逻辑名。我们的master service 一经启动则注册一个全局逻辑名称，node controller等访问它都利用这个公所周知的名字访问。如果master service 所在机器发生故障发生故障,master service 也随之终止，系统则自动取消该注册名，服务不再可用；当master service在新机器上重新启动后会再次注册逻辑名称，服务重新可用。

### 6.Node Controller位置定位 ###
Master service 和node controller 通讯需要能定位Node Controller。不过和Master Controller不同，Node Controller这里不再使用注册全局逻辑名方式。原因是Node controller在系统中不是唯一性角色，它会有数目不定的多个，且随时都有可能动态新增。
因此我们要么采用Node Controller启动后向Master Service注册自己位置（pid)的方式解决master service 到node controller的寻址问题。要么就要给Node controller找一个不变的命名，比如节点名+node controller应用名——这里的节点名应该是一个机器一个，不重复的固定名字。
#### 实现提示 ####
在erlang中消息发送大约四种形式：ServerRef = Name | {Name,Node} | {global,GlobalName} | pid 。
我们Node Controller使用 {Name,Node}形式，其中Node名称如下所示，Name为Node Controller的本地注册名。Node 的名字可在node 启动时按需要给定，不可重复。我们可以使用宿主机的hostname(必须唯一)，这里建议我们初期先使用唯一不变的主机名做node名字—— 主机名进行人工配置，不可重复。当机器节点加入时，管理员向master service  发出add node 命令——告诉master node 的位置信息——在erlang中你甚至还可以由master 远程启动node controller。

### 7.物理资源的自动发现 ###
资源集群支持随时补充的新物理机自动加入资源池，因此master service 要能自动发现新加入物理机，并且自动探测新加入物理机资源，并且更新系统资源池。
#### 实现提示 ####
  * 每当master收到节点注册消息后都应该发起get node info命令，获取node宿主机资源信息。
  * 将资源信息纪录到数据库资源表中。——  这里有个原则：为了安全和管理目的，node controller 不能直接访问数据库，只能由master 访问。所以这些信息要发给master，由其入库。
  * 资源信息中主机名应该是表名宿主机的唯一标示（ip mac地址也可以做该表示，总之不变唯一），因此资源信息里的主机名和node controller的位置标示（pid）对应关系应该由master service 记录。这样就能再资源池中找到资源目标机后，找到其上的node controller，发起命令了。

### 8.VM和NC宿主机状态变更监控 ###
VM 和NC 的状态变化需要及时上报，尤其是对于崩溃这种状态变化。NC宿主机如果突然崩溃我们由心跳机制负责监控；而对于其上的VM的突然终止（有可能是崩溃，也有可能是vm用户自己从内部终止虚拟机）则必须由Node controller监控，并上报给master service。Master service接收到VM崩溃消息后就要立刻更新虚拟机的状态（由runinng到stopped）。

#### 实现提示 ####
  * 原则上只要有状态变化就上报（node上初始状态是vm全空。有新运行vm出现，有运行vm终止都属于状态变化）。
  * node 要自己周期性监测vm状态（如用xm list查看）或利用vm启动关闭触发的事件（如热插拔设备事件）获知node上的vm的状态变化。上报的消息简单期间可以是全部当前node vm信息，让master判断对比自己数据库判断那些是变更信息。建议先用xm list监测变化－node需记录前次查询状态，已对比当前状态，发现变更。
  * node controller  用单独进程（spawn\_link）完成监测即上报工作。该进程周期运行，不退出。
注意：
  * 如果不用node本地监控，上报变化，也可使用master定期轮询node状态方式。后者轮询给网络和master本身带来更高负载，所以我们让node作这个检查工作和有事说事（发变更事件消息给master），没事休息。

### 9.虚拟机资源分配的实现 ###
虚拟机创建时首先要选择一个宿主机作为虚拟机部署位置。选择的大原则是找到满足用户虚拟机资源要求的宿主机为目标部署位置（说简单了就是从资源表中用 select条件查询找到一个合适的宿主机）。进一步思考我们又可打算采取均匀方式分配虚拟机或者用满一个宿主机再利用下一台宿主机——我们初期可先实现选择均匀分布策略（具体实现你们思考，不要想得复杂了）；下一阶段将根据node的负载（包括可复用内存）等动态信息进行更智能的调度。


### 虚拟机创建启动过程中的长事务处理问题 ###
虚拟机创建启动、关闭等操作类似于事务操作（如启动命令：需要查询资源库，标记vm状态starting，扣除资源，下发命令给node conteoller，如何成功则标记vm状态running。如果失败则退还资源，删除对应vm状态项。－ 这里关键在于资源预扣，和失败时资源回退。之所以要预先扣除是因为master serivce会并发的接受上述命令，所以如果不预先扣除资源则，会造成错误。

### 10.资源池内容 ###
vm启动所需资源都描述在master service 的资源池中。资源类别具体资源有：
  1. cpu(处理器的资源都是分时复用，复用率比较高，所以目前处理器资源可假设无限制﹣目前这样处理）
  1. 内存
  1. ip
  1. mac地址
  1. vm uuid全局唯一标识（可用于命名vm）。
  * 内存、cpu等是node congroller上报的宿主机资源
  * 空闲ip和mac和vm uuid等是master自维护资源，其中空闲ip资源应预先人工导
  * mac地址和vm uuid等都master可现场计算随机值获得。
  * IP地址资源我们初期采用由管理员预先导入的办法，而不是自动产生。
注意：其中我们没有记录磁盘资源是因为，因为整个集群的磁盘都被统一池化，而且hlfs支持thing provision，所以你几乎可以不考虑磁盘容量问题了。

### 11.存储优化策略 ###
为了减少虚拟磁盘使用中网络交互量，我们采用了只读母盘本地化、可变增量盘网络化的策略，从而极大减少系统运行时的网络交互。
具体而言有如下要点：
  1. 系统镜像文件目前我们只支持有限几种（尚不支持用户自定义image）。
  1. 有限几种镜像文件预部署到所有宿主机上。
  1. 用户申请的虚拟机系统盘将采用vhd格式，并且已本地预部署镜像作为母盘使用，而将vhd格式的增量盘作为文件放在我们虚拟磁盘上——tapdisk2方式挂载的tapdev设备。
  1. 目前实现中，为简单期间我们系统盘大小给定（非可变），只存储一些个性化配置等；用户数据请申请独立数据盘存储更为理想。
  1. 数据盘我们直接使用blktap2中我们提供的block-hlfs驱动来实现
注意：
上述策略用于系统盘，而非数据盘。因为数据盘存储用户数据，则不存在磨盘问题我们直接采用hlfs实现的虚拟盘供用户使用。

具体命令、原数据表、虚拟机操作脚本见详细设计

---

wroten by 康华