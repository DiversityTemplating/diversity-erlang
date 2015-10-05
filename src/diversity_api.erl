-module(diversity_api).

-export([get_diversity_json/3, get_file/4, get_tags/2, get_versions/2]).

%% @doc Fetches the diversity json for a specific Component and Tag
-spec get_diversity_json(binary(), diversity_semver:semver(), binary()) -> {ok, map()} | undefined.
get_diversity_json(Component, Version, DiversityURL) ->
    Tag = diversity_semver:semver_to_binary(Version),
    diversity_cache:get(
      {Component, Version, diversity_json},
      fun () ->
              Path = filename:join([<<"components">>, Component, Tag]),
              {ok, DiversityJSON} = call_api(DiversityURL, Path),
              jiffy:decode(DiversityJSON, [return_maps])
      end,
      application:get_env(diversity, resource_cache_time, 3600000)
     ).

%% @doc Fetches file content for a specific component and tag.
-spec get_file(binary(), diversity_semver:semver(), binary(), binary()) ->
    {ok, binary()} | undefined.
get_file(Component, Version, FilePath, DiversityURL) ->
    Tag = diversity_semver:semver_to_binary(Version),
    diversity_cache:get(
      {Component, Version, FilePath},
      fun () ->
              Path = filename:join([<<"components">>, Component, Tag, <<"files">>, FilePath]),
              call_api(DiversityURL, Path)
      end,
      application:get_env(diversity, resource_cache_time, 3600000)
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
      application:get_env(diversity, resource_cache_time, 5000)
     ).

%%%%%%%%%%%%%%%%%%%%
% Internal methods %
%%%%%%%%%%%%%%%%%%%%

get_tags(Component, DiversityURL) ->
    {ok, Tags0} = call_api(DiversityURL, filename:join(<<"components">>, Component)),
    jiffy:decode(Tags0).

call_api(URL, Path) ->
    Opts = [{body_format, binary}],
    Request = {unicode:characters_to_list(<<URL/binary, Path/binary>>), []},
    case httpc:request(get, Request, [], Opts) of
        {ok, {{_Version, Status, _ReasonPhrase}, _Headers, Body}} ->
            case Status of
                404 -> undefined;
                500 -> {error, server_error};
                200 -> {ok, Body}
            end
    end.
