PROJECT = diversity


DEPS = mustache.erl jiffy
dep_jiffy = git https://github.com/davisp/jiffy.git master
dep_mustache.erl = git https://github.com/Textalk/mustache.erl diversity-extensions

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

include erlang.mk
