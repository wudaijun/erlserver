REBAR 	= ./bin/rebar --config config/rebar.config
ENVPATH = ebin/ deps/*/ebin
CONFIG  = ./app.config
OPTS	= -pa $(ENVPATH) -config $(CONFIG)

all : compile


deps : config/rebar.config
	$(REBAR) get-deps

compile:
	$(REBAR) compile

run:
	#epmd -daemon # 让epmd后台运行
	erl $(OPTS) -run erlserver_app start

erl:
	erl $(OPTS)

# simple run
srun:
	erl $(OPTS) 

clean:
	$(REBAR) clean

kill:
	ps aux | grep bream | grep erlserver_app | awk '{print $$2}' | xargs kill -9


