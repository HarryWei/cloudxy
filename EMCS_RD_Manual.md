## 获取EMCS代码 ##
svn co http://cloudxy.googlecode.com/svn/trunk/ECMS

## 编译 ##
  1. 首先需要下载erlang —— 建议使用最新OTP R15B的版本
  1. 进入CloudMaster目录;执行erl -make — 编译cloudMaster Application
  1. 进入NodeController目录;执行erl -make — 编译NodeController Application

## 启动node ##
  * 启动node 运行cloud master
    1. 进入CloudMaster目录
    1. erl -name node\_master@127.0.0.1 -setcookie abc -boot start\_sasl -pa ebin
  * 启动node 运行node controller
    1. 进入nodeController目录
    1. erl -name node\_controller1@127.0.0.1 -setcookie abc -boot start\_sasl -pa ebin
```
注意:
1. node 的cookie需要设置为一样,否则无法交流.
2. 我们假设的部署方式是:node controller一个宿主机只部署一个.
```

## 初始化CloudMaster ##
  * 启动cloud\_master — 在node shell 上启动cloud\_master application :(node1@127.0.0.1)1>application:start(cloud\_master) .

```
注：
1.系统启动自动初始化运行环境——包含各种元数据表等.如果启动后看到屏幕有输出
   =PROGRESS REPORT==== 23-Jan-2012::14:51:45 ===
         application: cloud_master
          started_at: 'node1@127.0.0.1'
   则表示启动成功。
2.为保证数据安全，数据表将配置在多个node机器上，这些副本所在node将在sys.config配置（meta_nodes），测试期间未有sys.config文件情况下，将默认在本机node上建立数据表。
3.tv:start().可观察到系统建立了我们建立了6张元数据表——其中sequence表用于产生序列号，其余表用于元数据存储。
4.appmon:start(). 可观察系统各组件运行状况
```
  * 导入网络资源 — 在node shell上执行meta\_data\_ops:add\_netaddr\_res（IPaddr） 如 meta\_data\_ops:add\_netaddr\_res("192.168.0.99") .
```
注
网络ip地址资源用于分配给各虚拟机,为了简单和部署灵活起见,我们目前采用手动导入的方式初始化IP地址资源——mac等则自动产生.注意该动作只用做一次
```

## 加入node到集群中 ##
  * 为了从master所在shell执行命令，需要加入对应的record(因为我们命令使用了record描述)—shell中执行 rr("include/**.hrl") .   （这里有一个星号，没有显示出来）
  * 动态增增加节点—  gen\_server:call(meta\_server,#add\_node\_2m\_req{node\_name="node\_controller1@127.0.0.1"},2000) .
```
注：
1.注意是从master所在shell执行。
2.如果你手头没有合适虚拟机环境，也可以进行调试。只需要将node controller/src/command的各命令中增加-define(MOCK_TEST,1)，则可进行模拟调试调度框架.
```**

## 创建虚拟机 ##
  * 执行gen\_server:call(meta\_server,  #vm\_create\_2m\_req{vm\_resource\_info=#machine\_resoure\_info\_t{mem\_size=256,cpu\_count=1,disk\_size=0},vm\_custom\_info=#vm\_custom\_info\_t{user\_password="xxx",user\_hostname="yyy"},os\_type=ubuntu},2000) .
```
注.
1.上述命令返回一个VM_ID序列号表示创建的VM，以后对该VM操作都将传入该ID.
2.tv:start()检测你可发现vm_resource_config表中已经创建了一个上述VM_ID为主键的记录，不过记录中网络地址并给定——因为只有到实际启动后才真正分配固定的IP给你.
3.目前我们实现中系统盘的disk_size并未使用，系统盘创建大小目前固定给出.
```

## 启动虚拟机 ##
  * 执行 gen\_server:call(meta\_server, #vm\_start\_2m\_req{vm\_id=1},2000) .

注意：以上所有在shell中执行的命令，最后都有一个 '.'号， 不要忘记了 ;-)


---

wroten by 康华