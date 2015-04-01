-module(diversity_api_client_tests).

-include_lib("eunit/include/eunit.hrl").

diversty_api_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     [fun get_diversity_json/0,
      fun get_component_settings_schema/0,
      fun get_file/0]}.


start() ->
    application:ensure_all_started(diversity).

stop(_) ->
    application:stop(diversity).

get_diversity_json() ->
    ok.

get_component_settings_schema() ->
    ok.

get_file() ->
    ok.
