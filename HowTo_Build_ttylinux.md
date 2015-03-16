# 基于ttylinux项目制作最小文件系统和linux启动镜像 #

### 目的 ###
制作一个最小文件系统和linux启动镜像

---

  1. 感谢我们的源码提供者——ttylinux (http://ttylinux.net), 这是一个旨在给各种平台提供小的linux镜像的开源项目。
  1. 衷心的感谢 Douglas Jerome（douglas@ttylinux.org）和康华师哥（kanghua151@gmail.com）的帮助，如果没有他们的帮助，我可能无法顺利的探索出正确的制作方法。

---

### 制作方法 ###
  * 下载ttylinux源码包
    1. 登陆ttylinux官网，http://ttylinux.net/
    1. 手动下载或者在命令行下(Unix-like)输入 wget http://ttylinux.net/Download/ttylinux-src-mp9.tar.bz2
  * 制作交叉编译工具链
    1. tar xvf ttylinux-src-mp9.tar.bz2
    1. cd xbuildtool-0.2
    1. make setup
    1. make dload
    1. make x86\_64-2.13-gnu (通过make help 可以查看你想要的平台名称，我这里是64位平台的交叉工具名称)
  * 制作最小文件系统和最小linux镜像
    1. make dload
    1. 根据你的需要修改ttylinux主目录下的ttylinux-config.sh文件，可参考我的(想要生成x86\_64平台下的最小文件系统和linux启动镜像可以在http://code.google.com/p/cloudxy/downloads/list 中找到并下载)
    1. make dist (通过这一步可以生成ttylinux的所有目标，当然你也可以只生成部分目标，具体可以查看命令'make help'或者How\_To\_Build\_ttylinux.txt文件)
    1. 最后你就可以在ttylinux主目录下的img目录找到最小文件系统和linux启动镜像。
  * 我把自己制作的64位的最小文件系统和linux启动镜像都共享到了download列表，可以在 http://code.google.com/p/cloudxy/downloads/list 找到，并下载

### 注意事项 ###
  1. 制作交叉编译工具链时可以参考主目录下的x86\_64-2.13-gnu文件，制作最小文件系统和最小linux镜像时也可以参照主目录下的How\_To\_Build\_ttylinux.txt文件。
  1. 制作最小文件系统和最小linux镜像的过程中，如果发现问题，可以主目录下的var/log下的相关日志文件，这样可能快速找到问题并解决。
  1. 编译之前请确保你的/bin/sh 指向 bash 而非其他(dash等等)。
  1. 如果制作过程中遇到任何问题，都欢迎发邮件到ttylinux@googlegroups.com或者cloudxy@googlegroups.com邮件列表。
  1. ttylinux 的制作途径可能不仅局限于上述的方法，当然由于本人能力有限，以上的步骤也可能还存在漏洞，如果实践过程中发现问题，请及时指正，谢谢。


---

wroten by 贾威威 2012 2 12