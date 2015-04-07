-module(diversity_api_client).

-export([get_diversity_json/2, get_component_settings_schema/2, get_file/3, call_diversity_api/3]).

-define(RESOURCE_CACHE_TIME, 1000 * 60 * 60 * 1). %% An hour

%% @doc Fetches the diversity json for a specific Component and Tag
-spec get_diversity_json(binary(), binary()) -> map().
get_diversity_json(Component, Tag) ->
    {DiversityJson, _} = get_diversity_json_and_settings(Component, Tag),
    DiversityJson.

%% @doc Fetches settings for specific Component and Tag
-spec get_component_settings_schema(binary(), binary()) -> map().
get_component_settings_schema(Component, Tag) ->
    {_, Settings} = get_diversity_json_and_settings(Component, Tag),
    Settings.

%% @doc Fetches file content for a specific component and tag.
-spec get_file(binary(), binary(), binary()) -> binary().
get_file(Component, Tag, File) ->
    Tag1 = case Tag of
        <<"*">> ->
            get_latest_tag(Component);
        _ -> Tag
    end,
    diversity_cache:get(
        {Component, Tag1, File},
        fun() ->
              call_diversity_api(Component, Tag, {file, File})
        end,
        ?RESOURCE_CACHE_TIME
    ).

%%%%%%%%%%%%%%%%%%%%
% Internal methods %
%%%%%%%%%%%%%%%%%%%%

-spec get_diversity_json_and_settings(binary(), binary()) -> {map(), map()}.
get_diversity_json_and_settings(Component, Tag) ->
    Tag1 = case Tag of
        <<"*">> ->
            get_latest_tag(Component);
        _ -> Tag
    end,
    diversity_cache:get(
        {Component, Tag1, diversity_json_and_settings},
        fun () ->
            {call_diversity_api(Component, Tag, diversity_json),
             call_diversity_api(Component, Tag, settings)}
        end,
        ?RESOURCE_CACHE_TIME
    ).

%% @doc Get latest tag from *.
-spec get_latest_tag(binary()) -> binary().
get_latest_tag(Component) ->
    Tags = call_diversity_api(Component, undefined, tags),
    diversity_semver:expand_tag(<<"*">>, Tags).

%% Will make a http get call to the diversiyt-api server provided in the sys.config.
-spec call_diversity_api(binary(), binary(), atom()) -> binary() | term() | map().
call_diversity_api(Component, Tag, Action) ->
    {ok, Url} = application:get_env(diversity, diversity_api_url),
    Opts = [{body_format, binary}],
    Path = build_path(Component, Tag, Action),
    Request = {lists:flatten([binary_to_list(Url) | Path]), []},
    case httpc:request(get, Request, [], Opts) of
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} ->
            case Status of
                404 -> throw(resource_not_found);
                500 -> throw(server_error);
                _   ->
                    case Action of
                        {file, _} ->
                            Body;
                        tags ->
                            jiffy:decode(Body);
                        _ ->
                            %% Settings and diversity json
                            jiffy:decode(Body, [return_maps])
                    end
            end
    end.

build_path(Component, Tag, {file, File}) ->
    ["/components/", binary_to_list(Component), "/", binary_to_list(Tag), "/files/",
     binary_to_list(File)];
build_path(Component, Tag, diversity_json) ->
    ["/components/", binary_to_list(Component), "/", binary_to_list(Tag), "/"];
build_path(Component, Tag, settings) ->
    ["/components/", binary_to_list(Component), "/", binary_to_list(Tag), "/settings/"];
build_path(Component, undefined, tags) ->
    ["/components/", binary_to_list(Component), "/"].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_path_test() ->
    %% Files
    ?assertEqual(
        ["/components/", "foobar", "/", "1.0.0", "/files/", "test.html"],
        build_path(<<"foobar">>, <<"1.0.0">>, {file, <<"test.html">>})
    ),
    %% diversity json
    ?assertEqual(
        ["/components/", "foobar", "/", "1.0.0", "/"],
        build_path(<<"foobar">>, <<"1.0.0">>, diversity_json)
    ),
    %% Settings
    ?assertEqual(
        ["/components/", "foobar", "/", "1.0.0", "/settings/"],
        build_path(<<"foobar">>, <<"1.0.0">>, settings)
    ),
    %% Tags
    ?assertEqual(["/components/", "foobar", "/"], build_path(<<"foobar">>, undefined, tags)).

-endif.
