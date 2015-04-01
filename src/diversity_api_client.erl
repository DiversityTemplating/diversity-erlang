-module(diversity_api_client).

-export([get_diversity_json/2, get_component_settings_schema/2, get_file/3]).

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
        1000 * 60 * 60 * 1 %% An hour
    ).


%% @doc Fetches file content for a specific component and tag.
-spec get_file(binary(), binary(), binary()) -> binary().
get_file(Component, Tag, File) ->
    call_diversity_api(Component, Tag, {file, File}).

%%%%%%%%%%%%%%%%%%%%
% Internal methods %
%%%%%%%%%%%%%%%%%%%%

%% @doc Get latest tag from *.
-spec get_latest_tag(binary()) -> binary().
get_latest_tag(Component) ->
    Tags = call_diversity_api(Component, undefined, tags),
    diversity_semver:expand_tag(<<"*">>, Tags).

%% Will make a http get call to the diversiyt-api server provided in the sys.config.
-spec call_diversity_api(binary(), binary(), atom()) -> binary() | term() | map().
call_diversity_api(Component, Tag, Action) ->
    Component, Tag, Action,
    {ok, Url} = application:get_env(diversity, diversity_api_url),
    Opts = [{body_format, binary}],
    Path = build_path(Component, Tag, Action),
    Request = {lists:flatten([binary_to_list(Url) | Path]), []},
    case httpc:request(get, Request, [], Opts) of
        {ok, {{_Version, _Status, _ReasonPhrase}, _Headers, Body}} ->
            case Action of
                {file, _} ->
                    Body;
                tags ->
                    jiffy:decode(Body);
                _ ->
                    %% Settings and diversity json
                    jiffy:decode(Body, [return_maps])
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
