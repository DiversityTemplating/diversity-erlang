PROJECT = diversity

DEPS = cowboy jiffy
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_jiffy = git https://github.com/davisp/jiffy.git master
# dep_jsonrpc2-erlang = git https://github.com/Textalk/jsonrpc2-erlang.git

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

include erlang.mk
