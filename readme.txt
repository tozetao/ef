cd("d:/server/framework").
code:add_paths("d:/server/framework/ebin").


cd("d:/server/app").
code:add_paths("d:/server/app/ebin").
make:all([load]).


当前任务
-----------------------
搭建一套长连接框架，要求有：
	连接处理
	协议解析：做成插件式，可替换协议的实现。
	路由模块

	玩家进程
	登陆验证机制

	模型层

	翻译application的章节

设计想法
	在游戏世界中，所有的一切都是玩家触发的，玩家进程是核心，它负责调用模块/函数来处理客户端法来的玩家请求。








问题：
连续帧怎么办?
包头长度判断似乎有问题

sys_conn响应消息处理。



实践
进程抛出异常将会终止执行。在督程下，子进程会被重启。如果重启次数超过督程设置的强度，督程会终止所有子进程。



