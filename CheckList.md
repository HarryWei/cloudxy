# Checklist #

## 第一阶段 ##

| **责任人** | **任务** | **完成情况** | **分享与讨论日志** | **备注** |
|:--------------|:-----------|:-----------------|:--------------------------|:-----------|
| 张典博   | openstack 安装工作   | 进行中    | 未讨论 | 要注意devstack的安装 |
| 康华     | 测试HLFS iSCSI驱动   | 进行中      |  未讨论 |  测试要全面    |
| 王森     | 修复目前使用QEMU的bug，讲解iSCSI中HLFS驱动的实现 | 已讨论  | 已讨论 |  修复要细致，准确  |
| 贾威威   | 1,调查Openstack支持Sheepdog都需要对QEMU Libvirt等做了哪些改动;<br>2,调查Openstack支持iSCSI都需要做哪些改动;   <table><thead><th> 调查完成   </th><th> 讨论完成 </th><th> 理解其支持的原理和机制，方便移植  </th></thead><tbody></tbody></table>

<h2>第二阶段</h2>
<table><thead><th> <b>责任人</b> </th><th> <b>任务</b> </th><th> <b>完成情况</b> </th><th> <b>分享与讨论日志</b> </th><th> <b>备注</b> </th></thead><tbody>
<tr><td> 王森     </td><td> 修复目前使用QEMU的bug，完善iscsi驱动的功能（快照、获取容量） </td><td> 进行中  </td><td> 未讨论 </td><td>  修复要细致，准确  </td></tr>
<tr><td> 康华 <br> 贾威威   </td><td> 1, 测试(可能需要修改)HLFS patch for QEMU; <br> 2, 开发HLFS driver for Libvirt并测试;   </td><td> 任务1完成<br>任务2调整到下阶段完成   </td><td> 讨论完成 </td><td> 1, 理解HLFS driver for QEMU&&Libvirt;<br>2, 测试工作需仔细认真对待(关乎HLFS集成Openstack的成功与否);  </td></tr></tbody></table>

<h2>第三阶段</h2>
<table><thead><th> <b>责任人</b> </th><th> <b>任务</b> </th><th> <b>完成情况</b> </th><th> <b>分享与讨论日志</b> </th><th> <b>备注</b> </th></thead><tbody>
<tr><td> 康华 <br> 贾威威   </td><td> 开发HLFS driver for Libvirt并测试;   </td><td> 完成  </td><td> 讨论完成 </td><td> 测试工作需仔细认真对待(关乎HLFS集成Openstack的成功与否);<br>这个阶段分为了两部分完成了：<br> 1, HLFS Online storage driver <br> 2, HLFS Offline storage driver <br> 讨论也分为对应的两部分完成了 </td></tr></tbody></table>

<h2>第四阶段</h2>
<table><thead><th> <b>责任人</b> </th><th> <b>任务</b> </th><th> <b>完成情况</b> </th><th> <b>分享与讨论日志</b> </th><th> <b>备注</b> </th></thead><tbody>
<tr><td> 康华 <br> 张典博 <br> 贾威威   </td><td> 开发Openstack driver for Libvirt并测试;   </td><td> 完成  </td><td> 讨论完成 </td><td> 开发Openstack Nova和Openstack Cinder等工作，并测试 </td></tr></tbody></table>

<h2>第五阶段</h2>
<table><thead><th> <b>责任人</b> </th><th> <b>任务</b> </th><th> <b>完成情况</b> </th><th> <b>分享与讨论日志</b> </th><th> <b>备注</b> </th></thead><tbody>
<tr><td> 康华 <br> 张典博 <br> 贾威威   </td><td> 1, 测试完善一键安装脚本; <br> 2, 完成多文件扩展概要设计和详细设计等前期工作;  <br> 3, 实现多文件扩展并测试;   </td><td> 完成  </td><td> 未讨论 </td><td> 1, 一键安装脚本测试要完善; <br>  2, 多文件前期设计文档很重要，必须设计合理，思路清晰，这样后期会事半功倍; </td></tr></tbody></table>

<h2>第六阶段</h2>
<table><thead><th> <b>责任人</b> </th><th> <b>任务</b> </th><th> <b>完成情况</b> </th><th> <b>分享与讨论日志</b> </th><th> <b>备注</b> </th></thead><tbody>
<tr><td> 康华 <br> 贾威威   </td><td> 1, 制作HLFS debian和fedora distro. <br> 2, 合并HLFS drivers for QEMU/Libvirt/Openstack到对应社区; <br> 3, HLFS性能优化;   </td><td> 进行中  </td><td> 未讨论 </td><td> 1, 此阶段核心任务是HLFS性能优化，此任务要做细，这是HLFS推广的重要环节<br> 2, 补丁合并要有耐心，最后都要合并到社区，这样补丁才有生存力;  </td></tr></tbody></table>

<h2>注意事项</h2>
<ol><li>各个成员积极完成自己的任务，完成后组织大家开会，讲解自己的解决方案和结论。<br>
</li><li>各成员只有完成本阶段任务，才能进入下阶段任务，不可跳跃。<br>
</li><li>完成任务过程中遇到问题，及时沟通，可通过群邮件或者其他方式。<br>
</li><li>这种开发方式可有效避免重复劳动，推动项目前进。</li></ol>

<hr />
Checklist Maintainer:贾威威 (harryxiyou@gmail.com)