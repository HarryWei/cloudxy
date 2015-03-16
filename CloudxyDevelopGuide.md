# Cloudxy 代码开发规则 #

## 代码开发具体规则说明 ##
  1. 编码风格这里不做硬性规定，但是建议遵守linux内核的编码风格，代码尽量简洁，原则上是清晰明了,可读性高。
  1. 如果你想和我们一起开发cloudxy，那么你应该创建自己的分支，然后在自己的分支上实现自己想要完成的工作，然后写一些合适的单元测试，以及集成测试。
  1. 目前项目的开发借助了一些第三方库，详见项目主目录下面的3part目录，不到万不得已，我们不想再加入第三方库，当然也不是绝对的，如果你有好的建议，可以发邮件到邮件列表进行讨论。
  1. 目前我们借助的是google code的 svn 管理clouxy项目，如果你想用其他工具，例如git, cvs, hg,等等。这都可以，那么当你想把你实现的功能合并到mainline，你需要制作一个补丁发到邮件列表。
  1. clouxy项目的每个feature都将有一个专门维护的开发人员，你在自己的分支上开发完成之后，可以提交给对应feature的维护人员，通过后由这个feature的维护人合并到此feature中，最后由康华审核后合并到mainline。


## 如何创建自己的分支(针对google code svn) ##

  1. 首先需要加入我们的邮件列表，具体加入方式请参考http://code.google.com/p/cloudxy/wiki/HlfsJoinUs
  1. 发邮件到邮件列表(cloudxy@googlegroups.com), 说明你想参与一起开发，我们就会把你加到开发列表，接着你就可以创建自己的分支，进而开发。
  1. 登陆自己的google账号， 然后再打开网页http://code.google.com/p/cloudxy/source/checkout接着在终端输入 svn checkout https://cloudxy.googlecode.com/svn/ cloudxy --username yourname@gmail.com
  1. 如果顺利，你的当前目录就会出现clouxy目录，然后执行以下命令cd cloudxy/branches/hlfs/person  (或者 cd cloudxy/branches/ecms/person)  svn cp ../trunk/hlfs your\_branch\_name  (或者 svn cp ../trunk/ECMS your\_branch\_name)
  1. 到这里，你的分支就创建好了，注意上面的'yourname'需要换成你自己的gmail账号，'your\_branch\_name'需要换成你自己的分支名字，你的分支和当前主线一样了，你可以在自己的分支上实现自己的想法了

## 如何进行测试 ##
严格来讲，当你在自己的分支上实现一个功能，必须进行单元测试和集成测试，只有进行了
这两种测试才有可能被合并到主线。我们的测试框架采用glib测试框架。具体实现可以参见
glib手册或者http://cloudxy.googlecode.com/svn/trunk/hlfs/src/snapshot/unittest/


-- write by 贾威威  2012 2 3