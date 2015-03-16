# HLFS Cache设计 #
目的：减少读写操作的IO次数，提高hlfs吞吐量。

## 背景分析 ##
  * Cache分为写Cache和读Cache。
  * 就虚拟机应用而言，vm的page cache其实已经做了类似客户端cache了，所以hlfs层面不大需要再进行读cache；另外，由于hdfs已经有预读，因此hlfs层面的预读也可暂时省了。所以hlfs的读cache我们暂缓实现。
  * 我们当前的重点是实现写Cache——聚集分散的写IO请求，类似于kernel中的flush那样刷新page cache到磁盘。和写透相比与page cache的问题一样，系统宕机会丢失最后未写下去的脏页，对于多数应用场合而言这是可以接受的——如果不接受，那只能牺牲性能，采用写透方式执行。

## 机制描述（HLFS cache采用类似kernel中page cache机制） ##
  * 如果cache未达脏块刷新阀值，直接写入cache中，然后返回
  * 如果cache到达脏块刷新阀值，则唤醒flush后台线程，让其异步执行刷新操作。然后检测cache中是否有容纳本次写入的空间，如果有则写入，没有则继续等待到condition变量上 —— 等待再次唤醒。
  * flush后台将cache中的脏块写入后，释放cache空间，并通知在condition上等待的写入线程。
  * flush线程执行有两个条件——一个是周期行时间到达，一个是被写入线程唤醒。
  * flush线程每次写动作写入的脏块不能超过给定数量（每次写入对应一个log项，且每次写动作都要唤醒可能堵塞的写线程）。但会在循环中连续执行写入，直到脏块数低于阀值才停止写入，再次进入休眠状态。
  * 由于目前log的实现限制，所以每次flush写入的都只能是连续脏块。
  * 为了简化操作，目前flush写下去的脏块就直接归还到cache中。
  * 读请求来了后首先从cache中查询，如果没有再去从后台查询——但是这里要注意，由于读请求必然连续，但是如果有部分block在cache中，则可能要将请求分成多个小请求下发给后台存储。但后台hdfs没有（网络文件系统可能也不要）类似于readv的scatter io操作，所以多次小调用带来的开销远大于1次大的读请求。所以我们读到cache中的数据先临时缓存，然后请求原样发到后端，待返回结果后将临时缓存的脏块覆盖对应的块，然后返回该结果。这里还要说明一下，我们将全部命中的叫full hit,将只命中部分的叫part hit
```
    因为hdfs每次有预读功能，所以目前简单起见我们并未积攒请求，而是每块都做一次读操作——预读的效果是：连续读块都被读到了本地，所以分散IO对性能也影响不大。
```

## 数据结构描述 ##
![http://cloudxy.googlecode.com/svn/wiki/images/block-cache.png](http://cloudxy.googlecode.com/svn/wiki/images/block-cache.png)
  * block cache —— block buffer
  * hash table  —— 用于维护存储地址到block buffer中block的内存地址；后期将使用radix以优化。
  * dirty\_block list —— 用于按时间序列排列脏块，以便flush时根据按时间进行。
```
struct block_t{
uint64_t block_no;
char*    block;
}
```

## 接口描述 ##
```
cache 接口
struct cache_ctrl{
GMutex         *cache_lock；
GTrashStack    *block_cache；//块cache结构，目前使用glib的GTrashStack容器
GQueue     *dirty_block;     //脏块有序队列
Ghash     *block_map; //块号到块区内存地址映射表，目前使用glib的hash容器实现。
Gthread   *flush_worker; //刷新线程句柄
Gcond     *flush_waken_cond;   //刷新线程被写入线程唤醒的条件变量。
Gcond     *writer_waken_cond;  //写入线程被flush线程唤醒的条件变量。
void      *write_callback_func;   //用户传入的实际写cb函数.flush线程最终调用该cb写入数据。cb函数固定接受void*(存储句柄）,buff,len 三个参数，第一个由用户给定，后两个由flush线程提供。
void      *write_callback_user_param; //void*(存储句柄）由用户传入。
uint64_t  cache_size;    //缓存大小，单位块。
uint64_t  block_size;    //块大小，单位字节。
uint64_t  flush_interval; //刷新线程执行间隔
uint64_t  flush_trigger_level;//唤醒刷新线程执行的脏块比重 （0-100）
uint64_t  flush_once_size;//刷新动作单次写入的脏块数量。
uint64_t  total_write_count; //总写入请求数 
uint64_t  total_read_count;  //总读取请求数
uint64_t  cache_full_hit;    //读请求cache全部命中数
uint64_t  cache_part_hit;    //读请求cache部分命中数
}
```
```
struct cache_ctrl *cache_new()；
该函数内部创建cache_ctrl结构体，并返回。 

int cache_init(struct cache_ctrl* ctrl,
               uint64_t block_size,
               uint64_t cache_size,
               uint64_t flush_interval,
               uint64_t flush_trigger_level
	       uint64_t flush_once_size);
该函数将初始化、锁、内部容器、设定配置项值、启动刷新后台线程。

int cache_set_write_cb(cache_ctrl* ctrl,void* cb_func,void* cb_param);
该函数设置flush函数回调函数和用户参数

int cache_destroy（struct cache_ctrl* ctrl）;
该函数负责终止后台线程、销毁容器、销毁ctrl结构。

int cache_insert（struct cache_ctrl* ctrl,uint64_t block_no,char* block）;
该函数负责将数据写入cache中
。
int cache_query（struct cache_ctrl* ctrl,uint64_t block_no,char**block）;
该函数负责将从cache中读取数据

暂不实现....
int cache_change_size（）
int cache_set_trigger_level()
int cache_set_flush_interver()
int cache_get_status（）
.....
```

## 关键部分伪代码片描述 ##
关键逻辑描述，理解为主，不必拘泥
### cache\_insert 伪代码 ###
```
if(cur_write_size + get_cache_free_space(ctrl) <       ctrl->flush_trigger_level * ctrl->cache_size){
   g_cond_singal(ctrl->flush_waken_cond);
   g_mutex_lock(ctrl->cache_lock)
   while (get_cache_free_space(ctrl) < cur_write_size)
          g_cond_wait(ctrl->writer_waken_cond,ctrl->cache_lock);
   g_mutex_unlock(ctrl->cache_lock)
}else{
   //write cache...
   write_cache(ctrl,strat_block_no,cur_write_buff,cur_write_size);
}
```

### write\_cache 伪代码 ###
```
g_mutex_lock(ctrl->cache_lock)
for(int i=0 ; i< cur_write_size/ctrl->block_size;i++)
  struct block_t *block = g_trash_stack_pop (ctrl->block_cache);
  block->block_no = start_block_no + i;
   memcpy(cur_write_buff+i*ctrl->block_size,block->block,ctrl->block_size);
  g_hash_table_insert(ctrl->block_map,start_block_no + i,block);
  g_queue_push_head(ctrl->dirty_block,block);
}
g_mutex_unlock(ctrl->cache_lock)
```

### flush\_worker 伪代码 ###
```
while(!should_exit){
  gboolean ret = g_cond_timed_wait(ctrl->flush_waken_cond,ctrl->flush_waken_mutex,current_time + flush_interval);
  do(   
        GSList *contines_blocks = NULL;
  	get_continues_blocks(ctrl,continue_blocks);
        uint32_t size = g_slist_length(contines_blocks)
  	if(ctrl->cb_func!=NULL){
           char* tmp_buf = malloc(size * ctrl->block_size);
           for(int i=0;i<size;i++){
               block_t *block =  g_slist_nth_data(continue_blocks,i);
               memcpy(tmp_buf + i*ctrl->block_size,block->block,ctrl->block_size);
           }
     	   ret = ctrl->cb_func(ctrl->cb_param,tmp_buf,size * ctrl->block_size);
  	}else{
           print it...
        }
        if (ret == 0){
          g_mutex_lock(ctrl->writer_waken_mutex);
          free_cache(ctrl,g_slist_length(contines_blocks));
  	  g_cond_singal(ctrl->writer_waken_cond);
          g_mutex_unlock(ctrl->writer_waken_mutex);
          g_slist_free(continues_blocks)
        }
  }while( get_cache_free_space(ctrl) < ctrl->flush_trigger_level * ctrl->cache_size)
}
```

### cache\_destroy 伪代码 ###
```
g_mutex_lock(ctrl->cache_lock)
for(int i=0;i<size;i++){
  g_hash_table_remove(ctrl->block_map,block_no);
  block = g_queue_pop_tail(ctrl->dirty_block);
  g_trash_stack_push(ctrl->block_cache,block);
}
g_mutex_unlock(ctrl->cache_lock)
```

### get\_continues\_blocks 伪代码 ###int size = g_queue_get_length(ctrl->dirty_block);
block_t *block = g_queue_peek_nth(size);
int max_blockno = block->block_no - 1;
int min_blockno = block->block_no + 1;
for(i=size-1;i>0;i++){
    block_t *block = g_queue_peek_nth(i);
    if(max_blockno+1!=block->block_no || min_block_no-1!=block->block_no){
        break;
    }
    if(block->no > max_blockno){
       max_blockno+1;
    }
    if(block->no < min_blockno){
       min_blockno-1;
    }
}

-------------------
待续 ...
wroten by kh```