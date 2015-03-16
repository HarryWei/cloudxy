# Introduction #
**压缩的目的是什么？
毫无疑问：
  1. 节约存储空间.
  1. 节约网络带宽.**

要达到第一点，我们可借助hdfs本身的压缩功能——实现于服务端（http://www.devx.com/Java/Article/47913/1954）；要达到第二点则需要自己做点工作，也就是在client端实现压缩。<br>

<b>用什么压缩算法？<br>
毫无疑问，我们要：<br>
<ol><li>速度最快的（因为是online服务），而不是压缩比最高的.<br>
</li><li>能支持块压缩（我们的压缩单位是block).</li></ol></b>

满足上述要求的最佳选择是lzo算法（我们应使用最简单的minilzo即可）<br>
<br>
<h1>Details</h1>

<img src='http://cloudxy.googlecode.com/svn/wiki/images/lzo.png' />