# HLFS snapshot design #

## 主要概念说明 ##
  * 为什么需要快照
> 对于host服务的虚拟机，快照和回滚是防止网站被入侵或者误操作所造成数据污染，损坏的最有效手段。
  * 快照的类型—我们将快照分为两种类型：
  1. checkpoint：检查点，检查点可以简单的认为是自动快照，也就是说无需外部触发，周期性，或者按某种条件自动触发进行的.在我们hlfs目前写透方式下，其实每写入log就等于进一个checkpoint.这种类型的快照无需重新设计，每个log都是一个天然的检查点.
  1. snapshot  ：快照,即用户使用接口运行时创建检查点.这种类型的快照是由用户触发,希望能永久性保留.

  * snapshot的内容
> 对应inode存储地址、时间戳、以及用户定义的快照名.
  * snapshot的存储方式
> 存储在文件checkpoints.txt中,主要追加方式写入.

## 关于snapshot场景下的段回收机制设计 ##
  * 方案1 - 保留最新时期所有日志.
> 该方案我们不再区分checkpoint或者snapshot——snapshot的意义相比checkpoint仅仅多了一个用户给定的标记。对段回收任务来说，只关心旧日志距当前多久。超过给定期限则进行回收（回收方法使用后面描述的“区间回收”），否则保留它。
  * 方案2 - 快照间分区回收。
> 假如用户打上了N次永久快照（分别是SS1..SSm-1,SSm,SSm+1..SSn)。那么我们的段回收工作将分成n个子任务，每个子任务负责一段区域内的段回收。
  * Task1负责SS1快照之前的所有段回收；
  * Taskm-1 负责SSm-2 到SSm-1 区间内的段回收；
  * Taskm   负责SSm-1 到SSm   区间内的段回收；
  * Taskm+1 负责SSm   到SSm+1 区间内的段回收；
  * Taskn   负责SSn   到当前段的段回收；
> Taskn 的实现按照传统的回收方式进行；其余回收子任务则有所不同（我们称为“区间回收”）
> 区间回收实现设计!
  * 段统计：和传统段统计相比:需要变化的是——选择次新inode，变为选择快照时刻对应inode.即Taskx使用SSx对应的inode作为统计时刻的inode.
  * 段清理：区间内实现段清理，对于活跃db统计计数已经为0的段，可以删除.但对于尚有少量活跃数据的段却不容易采用传统回收所使用的move + remove的方式实现.其中一个主要原因是若move一个区间内的一个log，就需要更改其后续的所有SS对应的inode（传统的段清理动作是将move的log写入最新段，从而也伴随更新最新inode，相对简单许多).这显然是一个很重的操作.如何在区间回收中找到一个很好的算法实现move + remove语意，我目前也没想到好办法.
> 从上述分析来看，区间回收不能完美实现，退而求其次，只回收那些活跃db为0的那些段文件。 鉴于不能彻底回收的原因，我们为了要避免可能的太多无法回收空间，同时也简化涉及的目的，比较现实的做法是采用：<br>
<blockquote><i>退化的区间回收算法（不move + remove)+时间超期限制(过期则必然回收，即便有快照)</i></blockquote>

<h2>关于树形快照的设计</h2>
<img src='http://cloudxy.googlecode.com/svn/wiki/images/tree-snapshot.png' />

上图是一个示意性树形快照,无论是横或竖链都代表了一次回滚后的新更新序列。<br>
就上图情况而言，用户操作过程如下：<br>
<ol><li>创建hlfs后，在线更新情况下，在后续时刻又创建了12个快照（分别在T1-T12),然后卸载hlfs.<br>
</li><li>回滚到T5时刻的快照，继续更新，在后续时刻又创建了5个快照（分别在T13-T17),然后卸载hlfs.<br>
</li><li>回滚到T16时刻的快照，继续更新，在后续时刻又创建了3个快照（分别在T18-T20),然后卸载hlfs.<br>
</li><li>回滚到T19时刻的快照，继续更新，在后续时刻又创建了2个快照（分别在T21-T23),然后卸载hlfs.<br>
</li><li>回滚到T23时刻的快照，继续更新，在后续时刻又创建了2个快照（分别在T24-T25),然后卸载hlfs.<br>
</li><li>回滚到T25时刻的快照，继续更新，在后续时刻又创建了1个快照（分别在T26),然后卸载hlfs.<br>
</li><li>回滚到T14时刻的快照，继续更新，在后续时刻又创建了2个快照（分别在T27-T28),然后卸载hlfs.<br>
</li><li>回滚到T8时刻的快照，继续更新，在后续时刻又创建了2个快照（分别在T29-T30),然后卸载hlfs.<br>
</li><li>回滚到T3时刻的快照，继续更新，在后续时刻又创建了4个快照（分别在T31-T34)。<br>
</li></ol><blockquote>最终用户选择了T1,T2,T3,T31,T32,T33,T34所在快照代表的更新序列。<br>
总结一下树形快照的特性：<br>
</blockquote><ul><li>可以回滚到任何时刻的快照。<br>
</li><li>可以从回滚点继续更新数据，并也可产生新快照。<br>
</li><li>回滚不破坏任何已经存在的快照，也不破坏任何已经在的数据。</li></ul>

为了描述这种树形快照，需要每个快照都记录其上游快照，因为只要有了上游快照，则可描述出如图所示的树形结构<br>
<pre><code>typdef struct snapshot{<br>
       char up_ss_name[128];<br>
       char ss_name[128];<br>
}SNAPSHOT_T<br>
</code></pre>

对于回滚到给定checkpoint的情况，由于checkpoint没有人为给定的命名，所以系统可自动其一个可记标记。从目前实现而言，使用checkpointer对应的inode存储地址（64位数字转成16进制地址）给其命名为直关。<br>
<br>
<h3>树形快照实现要点</h3>
树形快照的关键在于如何维护快照之间前驱关系，也就是说如何获得ss_name和up_ss_name。ss_name我们进行快照时由用户给定，而up_ss_name的获得则要有所是设计。我们倾向于系统启动时获取该信息，并且维护在自身上下文中——如此在进行快照动作时可很方便创建snapshot_t对象。<br>
<br>
我们看看大概的的几个启动场景：<br>
<ol><li>如果系统启动时未拉新分支(hlfs_open)，即基于最后一次log日志继续更新（如上图中沿着T34后某个log继续写），则只需要想办法获取前驱快照名称（T34)，即可维护起树状分支关系。<br>
</li><li>如果系统启动时按照给定快照作为前驱(hlfs_open_by_snapshot)，则意味着拉了一条新的分支进行更新（如上图从T8开始引出的新分支等），则前驱关系由给定快照提供。<br>
</li><li>如果系统启动时从一个非快照的log日志启动(hlfs_open_by_inode)，则意味着也要拉一个新分支进行更新(如T8 T29快照中某个log日志的inode做基点启动)，这时也要想办法获得前驱快照名称(T8)，同时创建一个新匿名快照（非用户给定名称，用inode地址做名称的快照)，从而维护树状分支关系。<br>
</li></ol><ul><li>1和3两个场景都存在如何获取前驱快照的问题。那么前驱快照如何维护呢？<br>
</li></ul><blockquote>我们知道前驱快照存在期对应上图中每一两个快照之间的运行期（期间会对应很多log日志），所以在运行上下文中称为alive snapshot更合适，只有再创建新快照或者回滚到某个新快照继续更新时，才会有新的alive snapshot出现。因此最朴素的做法是每次创建新alive snapshot都如快照一样记录到alive snapshot文件中（按时间需有序追加）。如此一来再1/3两个场景中寻找alive snapshot的操作就很自然的通过读取alive snapshot文件完成了。</blockquote>

<hr />

<h2>API设计</h2>
<blockquote>我们先实现最精简的快照接口<br>
<pre><code> int take_snapshot(HLFS_CTRL *ctrl, const char *ss_name)<br>
  给用户提供的在线方式快照接口，用户可以指定快照名<br>
  * ctrl: 全局控制结构体<br>
  * ss_name: 用户指定的快照名<br>
  * return value: 成功返回0，失败返回&lt;0<br>
 int rm_snapshot(const char *uri, const char *ss_name)<br>
  用户可以通过这个接口来删除指定名字的快照<br>
  * uri: 文件系统的位置 <br>
  * ss_name: 用户指定的快照名<br>
  * return value: 成功返回0，失败返回&lt;0<br>
 int find_inode_before_time(const char *uri, uint64_t time,uint64_t* inode_addr)<br>
  用户可以通过这个接口来获取timestamp之前的inode存储地址<br>
  * uri: 文件系统的位置 <br>
  * time: 用户指定的时间<br>
  * inode_addr 返回inode存储地址。<br>
  * return value: 成功返回0,失败返回&lt;0<br>
 int find_inode_by_name(const char *uri, const char *ss_name,uint64_t* inode_addr)<br>
  用户可以通过这个接口来获取指定名字的快照记录的inode存储地址<br>
  * uri: 文件系统的位置 <br>
  * ss_name: 用户指定的快照名<br>
  * inode_addr 返回inode存储地址<br>
  * return value: 成功返回0,失败返回&lt;0<br>
 int get_inode_info(const char *uri,uint64_t inode_addr, uint64_t *creat_time, uint64_t *length)<br>
  获取指定inode的创建时间和文件长度<br>
  * uri: 文件系统的位置 <br>
  * inode_addr: 指定的inode存储地址<br>
  * creat_time: 创建时间将存放在这里<br>
  * length: 文件长度将存放在这里<br>
  * return value: 成功返回0，失败返回&lt;0<br>
 int hlfs_open_by_inode(HLFS_CTRL *ctrl,uint64_t inode_addr,int flag)<br>
  加载指定的inode，以flag标识的模式启动回滚<br>
  * ctrl : 全局控制结构<br>
  * inode: 指定的inode<br>
  * flag: 0 只读模式， 1 可写模式<br>
  * return value: 成功返回0，失败返回&lt;0<br>
  int hlfs_open_by_snapshot(HLFS_CTRL *ctrl,const char* snapshot,int flag)<br>
  加载指定的inode，以flag标识的模式启动回滚<br>
  * ctrl : 全局控制结构<br>
  * snapshot: 快照名称<br>
  * flag: 0 只读模式， 1 可写模式<br>
  * return value: 成功返回0，失败返回&lt;0 <br>
  struct snapshot * hlfs_get_all_snapshots(const char *uri，char *num_enteris)<br>
  给用户列出所有已存在的快照名及时间信息<br>
  * uri: 文件系统的位置 <br>
  * num_enteris: 返回的snapshot数量。<br>
  * return snapshot:返回snapshot对象数组。<br>
</code></pre></blockquote>


<h2>API使用指南</h2>
<ul><li>快照： 在线模式调用take_snapshot()函数<br>
</li><li>获取快照清单：离线模式调用list_all_snapshot()<br>
</li><li>删除指定名字的快照：离线模式调用rm_snapshot()<br>
</li><li>回滚到检查点(只知道模糊时间点)：调用find_inode_before_time获得inode结构体，然后调用get_inode_info()获取inode信息，根据该信息判断是否满足用户要求，则启动回滚——调用init_hlfs()获得全局控制结构，并以只读或者是可写模式调用hlfs_open_by_inode()<br>
</li><li>回滚到快照：调用find_inode_by_snapshot.<br>
<hr />
write by 康华</li></ul>
