-module(diversity_api_client_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FILE_BODY, <<"This is a file!">>).

diversty_api_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     [fun get_diversity_json_and_component_settings_schema/0,
      fun get_file/0,
      fun resource_not_found/0,
      fun server_error/0]}.


start() ->
    application:ensure_all_started(diversity),
    meck:new(httpc, [passthrough]),
    ok.

stop(_) ->
    meck:unload(httpc),
    application:stop(diversity).

%%%% TESTS %%%%%

get_diversity_json_and_component_settings_schema() ->
    meck:expect(
        httpc, request,
        fun(get, _Reuqest, [], _Opts) ->
            {ok, {{version, 200, []}, [], <<"{\"key\":\"foobar\", \"integer\": 1234}">>}}
        end
    ),
    DiversityJson = diversity_api_client:get_diversity_json(<<"FooBar">>, <<"1.0.0">>),
    %% Now check if we get the expected value
    ?assertEqual({ok, #{<<"key">> => <<"foobar">>, <<"integer">> => 1234}}, DiversityJson),
    %% When fetching diversity_json, we have optimized to fetch the settings as well.
    %% So count should be 2.
    ?assertEqual(2, meck:num_calls(httpc, request, [get, {'_',[]}, [], [{body_format,binary}]])),
    %% Now fetching settings we should get the resutl without making a httpc:request.
    ComponentSettings = diversity_api_client:get_component_settings_schema(<<"FooBar">>, <<"1.0.0">>),
    ?assertEqual({ok, #{<<"key">> => <<"foobar">>, <<"integer">> => 1234}}, ComponentSettings),
    %% Should still be 2.
    ?assertEqual(2, meck:num_calls(httpc, request, [get, {'_',[]}, [], [{body_format,binary}]])),
    %% This means stuff is cached.
    ok.

get_file() ->
    meck:reset(httpc),
    meck:expect(
        httpc, request,
        fun(get, _Reuqest, [], _Opts) ->
            {ok, {{version, 200, []}, [], ?FILE_BODY}}
        end
    ),
    FileBody = diversity_api_client:get_file(<<"Foobar">>, <<"1.0.0">>, <<"/foobar.txt">>),
    ?assertEqual({ok, ?FILE_BODY}, FileBody),
    ?assertEqual(1, meck:num_calls(httpc, request, [get, {'_',[]}, [], [{body_format,binary}]])),
    FileBody1 = diversity_api_client:get_file(<<"Foobar">>, <<"1.0.0">>, <<"/foobar.txt">>),
    ?assertEqual(1, meck:num_calls(httpc, request, [get, {'_',[]}, [], [{body_format,binary}]])),
    ?assertEqual({ok, ?FILE_BODY}, FileBody1).

resource_not_found() ->
    meck:expect(
        httpc, request,
        fun(get, _Reuqest, [], _Opts) ->
            {ok, {{version, 404, []}, [], ""}}
        end
    ),
    ?assertEqual(undefined, diversity_api_client:get_file(<<"fail">>, <<"*">>, <<"fail">>)).

server_error() ->
    meck:expect(
        httpc, request,
        fun(get, _Reuqest, [], _Opts) ->
            {ok, {{version, 500, []}, [], ""}}
        end
    ),
    ?assertThrow(server_error, diversity_api_client:get_file(<<"fail">>, <<"*">>, <<"fail">>)).
