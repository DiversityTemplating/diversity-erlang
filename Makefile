PROJECT = diversity

DEPS = mustache.erl jiffy lager
dep_jiffy = git https://github.com/davisp/jiffy master
dep_mustache.erl = git https://github.com/Textalk/mustache.erl master
dep_lager = git https://github.com/basho/lager master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master
  
ERLC_COMPILE_OPTS = +'{parse_transform, lager_transform}'

include erlang.mk
