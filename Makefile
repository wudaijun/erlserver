REBAR 	= rebar --config rebar.config
ENVPATH = apps/*/ebin/ apps/*/deps/*/ebin
OPTS	= -pa $(ENVPATH)
SERVER  = ./rel/erlserver/bin/erlserver

all : 
	make compile
	make gen

deps : rebar.config
	$(REBAR) get-deps

compile:
	$(REBAR) compile

gen:
	-mkdir rel
	rm -rf rel/*
	cp -r apps/erlserver/rel/* rel
	$(REBAR) generate

run:
	$(SERVER) start

attach:
	$(SERVER) attach

stop:
	$(SERVER) stop	

erl:
	erl $(OPTS)

rellog:
	cat ./rel/erlserver/log/erlang.log.1

clean:
	$(REBAR) clean
	rm -rf logs/*

cleang:
	rm -rf logs/*
