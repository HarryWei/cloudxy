# 段清理说明 #
目的：回收冗余数据，释放磁盘空间
支持：
  * 两步实现段清理——段统计和段回收
  * 快照条件下的段清理
  * 在线和离线条件下段清理
  * 拉模式和推模式段统计计数
# 回收步骤 #
  1. 首先计算给定段的段统计计数 —— 用于段回收阶段
```
拉模式执行段统计：output/bin/segcalc.hlfs -u local:///tmp/testenv/testfs -s 0 -e 100  ; 具体命令参数请 -h 查看

推模式执行段统计（只能在hdfs之上）：hadoop pipes -D mapred.reduce.tasks=1 -input /tmp/testenv/testfs  -output /output -program /exe/mr_segcalc.hlfs；
* mr_segcalc.hlfs 通过/src/clean/Mapreducer/下编译而得
* 我们只需要一个reduce 不要搞错喽。
```
  1. 其次进行段回收 —— 段统计之后才可回收
```
离线模式（当hlfs close后才能是使用): ../../../output/bin/segclean.hlfs -u local:///tmp/testenv/testfs -s 0 -e 100 -w 1024  ; 具体命令参数请 -h 查看

在线模式（runtime) : 不需要外部触发，由seg_clean_task后台任务周期执行。
```

# 执行策略 #
  1. 段统计可在任何时刻运行（在线或离线都无妨）；实践中可选择在系统IO比较轻时运行，比如每天夜里运行；触发动作由外部系统给出，如cron。
  1. 段回收或可实时运行（seg\_clean\_task会避开IO忙时，偷偷逮空运行）；或再离线时按需触发segclean.hlfs执行之；


-- 由于测试不很充分，因此难免有一些bug,欢迎指正