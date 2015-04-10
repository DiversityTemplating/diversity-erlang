PROJECT = diversity

DEPS = cowboy jiffy mustache.erl lager
dep_cowboy = git https://github.com/ninenines/cowboy 1.0.1
dep_jiffy = git https://github.com/davisp/jiffy.git master
dep_mustache.erl = git https://github.com/Textalk/mustache.erl diversity-extensions
dep_lager = git https://github.com/basho/lager.git master

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck.git master

include erlang.mk
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
