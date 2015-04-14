-module(diversity_api_client).

-export([get_diversity_json/2, get_file/3, get_tags/1, get_versions/1]).

-define(RESOURCE_CACHE_TIME, 1000 * 60 * 60 * 1). %% An hour

%% @doc Fetches the diversity json for a specific Component and Tag
-spec get_diversity_json(binary(), diversity_semver:semver()) -> {ok, map()} | undefined.
get_diversity_json(Component, Version) ->
    Tag = diversity_semver:semver_to_binary(Version),
    Path = ["/components/", unicode:characters_to_list(Component), "/", unicode:characters_to_list(Tag), "/"],
    diversity_cache:get(
        {Component, Version, diversity_json},
        fun () ->
            {ok, DiversityJSON} = call_api(Path),
            Diversity = jiffy:decode(DiversityJSON, [return_maps]),
            not is_map(Diversity) andalso throw({invalid_component, Component, Version, diversity_json}),
            Diversity
        end,
        ?RESOURCE_CACHE_TIME
    ).

%% @doc Fetches file content for a specific component and tag.
-spec get_file(binary(), diversity_semver:semver(), binary()) -> {ok, binary()} | undefined.
get_file(Component0, Version0, FilePath0) ->
    Component1 = unicode:characters_to_list(Component0),
    Version1 = unicode:characters_to_list(diversity_semver:semver_to_binary(Version0)),
    FilePath1 = unicode:characters_to_list(FilePath0),
    Path = ["/components/", Component1, "/", Version1, "/files/", FilePath1],
    diversity_cache:get(
        {Component0, Version0, FilePath0},
        fun () -> call_api(Path) end,
        ?RESOURCE_CACHE_TIME
    ).

get_versions(Component) ->
    diversity_cache:get(
        {Component, tags},
        fun () ->
            lists:filtermap(
              fun (Tag) ->
                  try diversity_semver:binary_to_semver(Tag) of
                      Version -> {true, Version}
                  catch
                      error:badarg -> false
                  end
              end,
              get_tags(Component)
             )
        end,
        ?RESOURCE_CACHE_TIME
    ).

%%%%%%%%%%%%%%%%%%%%
% Internal methods %
%%%%%%%%%%%%%%%%%%%%

get_tags(Component) ->
    Path = ["/components/", unicode:characters_to_list(Component), "/"],
    {ok, Tags0} = call_api(Path),
    Tags1 = jiffy:decode(Tags0),
    not is_list(Tags1) andalso throw({invalid_component, Component, tags}),
    Tags1.

call_api(Path) ->
    {ok, Url} = application:get_env(diversity, diversity_api_url),
    Opts = [{body_format, binary}],
    Request = {lists:flatten([unicode:characters_to_list(Url), Path]), []},
    case httpc:request(get, Request, [], Opts) of
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} ->
            case Status of
                404 -> undefined;
                500 -> throw(server_error);
                200 -> {ok, Body}
            end
    end.
