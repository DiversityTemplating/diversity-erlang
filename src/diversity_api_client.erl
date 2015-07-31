-module(diversity_api_client).

-export([get_diversity_json/3, get_file/4, get_tags/2, get_versions/2]).

-define(RESOURCE_CACHE_TIME(),
        (application:get_env(diversity, resource_cache_time, 1000 * 60 * 60 * 1))).

%% @doc Fetches the diversity json for a specific Component and Tag
-spec get_diversity_json(binary(), diversity_semver:semver(), binary()) -> {ok, map()} | undefined.
get_diversity_json(Component, Version, DiversityURL) ->
    Tag = diversity_semver:semver_to_binary(Version),
    Path = ["components/", unicode:characters_to_list(Component), "/", unicode:characters_to_list(Tag), "/"],
    diversity_cache:get(
        {Component, Version, diversity_json},
        fun () ->
            {ok, DiversityJSON} = call_api(DiversityURL, Path),
            Diversity = jiffy:decode(DiversityJSON, [return_maps]),
            not is_map(Diversity) andalso throw({invalid_component, Component, Version, diversity_json}),
            Diversity
        end,
        ?RESOURCE_CACHE_TIME()
    ).

%% @doc Fetches file content for a specific component and tag.
-spec get_file(binary(), diversity_semver:semver(), binary(), binary()) -> {ok, binary()} | undefined.
get_file(Component0, Version0, FilePath0, DiversityURL) ->
    Component1 = unicode:characters_to_list(Component0),
    Version1 = unicode:characters_to_list(diversity_semver:semver_to_binary(Version0)),
    FilePath1 = unicode:characters_to_list(FilePath0),
    Path = ["components/", Component1, "/", Version1, "/files/", FilePath1],
    diversity_cache:get(
        {Component0, Version0, FilePath0},
        fun () -> call_api(DiversityURL, Path) end,
        ?RESOURCE_CACHE_TIME()
    ).

get_versions(Component, DiversityURL) ->
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
              get_tags(Component, DiversityURL)
             )
        end,
        ?RESOURCE_CACHE_TIME()
    ).

%%%%%%%%%%%%%%%%%%%%
% Internal methods %
%%%%%%%%%%%%%%%%%%%%

get_tags(Component, DiversityURL) ->
    Path = ["components/", unicode:characters_to_list(Component), "/"],
    {ok, Tags0} = call_api(DiversityURL, Path),
    Tags1 = jiffy:decode(Tags0),
    not is_list(Tags1) andalso throw({invalid_component, Component, tags}),
    Tags1.

call_api(URL, Path) ->
    Opts = [{body_format, binary}],
    Request = {lists:flatten([unicode:characters_to_list(URL), Path]), []},
    case httpc:request(get, Request, [], Opts) of
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} ->
            case Status of
                404 -> undefined;
                500 -> throw(server_error);
                200 -> {ok, Body}
            end
    end.
