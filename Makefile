REBAR 	= ./bin/rebar --config config/rebar.config
ENVPATH = ebin/ deps/*/ebin
OPTS	= -pa $(ENVPATH)

all : compile


deps :
	$(REBAR) get-deps

compile:
	$(REBAR) compile

run:
	#epmd -daemon # 让epmd后台运行
	erl $(OPTS) -run erlserver start

erl:
	erl $(OPTS)

# simple run
srun:
	erl $(OPTS) 

clean:
	$(REBAR) clean

kill:
	ps aux | grep bream | grep erlserver_app | awk '{print $$2}' | xargs kill -9


