PROJECT = diversity

DEPS = mustache.erl jiffy lager
dep_jiffy = git https://github.com/davisp/jiffy master
dep_mustache.erl = git https://github.com/Textalk/mustache.erl 0.1.0
dep_lager = git https://github.com/basho/lager master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master
  
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'
ERLC_OPTS = $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS = $(ERLC_COMPILE_OPTS)

include erlang.mk
