一个简单的Erlang GameServer, 正在持续架构中....

使用:

	# 获取deps依赖
	make deps
	# 编译
	make compile
	# 生成发布
	make gen
	# 运行
	make run
	# attach到server上
	make attach
	# 停止运行
	make stop

测试:
	
	目前只有一个login_case测试用例 用于测试重登机制

	make erl
	erl -pa apps/*/ebin/ apps/*/deps/*/ebin
	Erlang/OTP 17 [erts-6.3.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

	Eshell V6.3.1  (abort with ^G)
	1> login_case:run().
