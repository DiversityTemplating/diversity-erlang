-module(diversity_semver).

-export([expand_tag/2]).

%% @doc Expand  an incoming tag into the latest matching tag
%% Supported formats are:
%% *     - Latest version
%% X     - Latest minor in given major
%% X.Y   - Latest patch in given major and minor
%% X.Y.Z - Given version if it is in the given Tags
%%
%% all cases will throw an error if no version can be found
expand_tag(Tag0, Tags) ->
    case binary:split(Tag0, <<$.>>, [global]) of
        %% Exact version specified, check if it exists
        [_Major, _Minor, _Patch] = Version ->
            _Version = to_version(Version),
            true = lists:member(Tag0, Tags),
            Tag0;
        %% Expand to the latest version
        [<<"*">>] ->
            find_latest_version(fun compare_all/2, Tags);
        %% Expand to the latest minor and patch in a given major
        [Major0] ->
            Major1 = binary_to_integer(Major0),
            find_latest_version(compare_major(Major1), Tags);
        %% Expand to the latest tag in a given major and minor
        [Major0, Minor0] ->
            {Major1, Minor1} = {binary_to_integer(Major0), binary_to_integer(Minor0)},
            find_latest_version(compare_minor(Major1, Minor1), Tags)
    end.

%% @doc Compare all version
compare_all(Version0, Version1) ->
    case Version1 > Version0 of
        true  -> Version1;
        false -> Version0
    end.

%% @doc Compare only a given major version
compare_major(Major) ->
    %% Same major version, compare them
    fun ({_Major0, _Minor0, _Patch0} = Version0, {Major1, _Minor1, _Patch1} = Version1)
          when Major1 == Major ->
        case Version1 > Version0 of
            true  -> Version1;
            false -> Version0
        end;
    %% Different major, keep the acc
        (Version0, _Version1) ->
            Version0
    end.

%% @doc Compare a given major and minor version
compare_minor(Major, Minor) ->
    %% Same major version, compare them
    fun ({_Major0, _Minor0, _Patch0} = Version0, {Major1, Minor1, _Patch1} = Version1)
          when Major1 == Major, Minor1 == Minor ->
        case Version1 > Version0 of
            true  -> Version1;
            false -> Version0
        end;
    %% Different major, keep the acc
        (Version0, _Version1) ->
            Version0
    end.

%% @doc Retrive the latest version according to the given comparsion function
find_latest_version(MaybeSwap, Tags) ->
    find_latest_version({-1, -1, -1}, MaybeSwap, Tags).

find_latest_version(Version, _MaybeSwap, []) ->
    version_to_binary(Version);
find_latest_version(VersionAcc, MaybeSwap, [Tag | Tags]) ->
    NewVersion = try to_version(Tag) of
        %% Compare with the version we got
        Version -> MaybeSwap(VersionAcc, Version)
    catch
        %% Incorrect semver, ignore and move on
        error:_ -> VersionAcc
    end,
    find_latest_version(NewVersion, MaybeSwap, Tags).

%% @doc Version to binary
%% May throw error:badarg.
version_to_binary({Major, Minor, Patch}) when Major >= 0, Minor >= 0, Patch >= 0 ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>.

%% @doc Binary or list to version tuple
to_version(Version) when is_list(Version) ->
    case lists:map(fun binary_to_integer/1, Version) of
        [Major, Minor, Patch] when Major >= 0, Minor >= 0, Patch >= 0 ->
            {Major, Minor, Patch};
        _ ->
            error(badarg)
    end;
to_version(Tag) when is_binary(Tag) ->
    to_version(binary:split(Tag, <<$.>>, [global])).

resolve_constraints(Constraints) ->
    resolve_constraints(Constraints, {0, 0, 0}).

resolve_constraints([], Acc) ->
    Acc;
resolve_constraints([ConstraintBin | Constraints], Acc0) ->
    Acc1 = case binary_to_constraint(ConstraintBin) of
               any ->
                   Acc0;
               {atleast, Version} ->
                   case Version >= Acc0 of
                       true ->
                           Version;
                       false ->
                           %% Log error because versions are not compatible
                           Version
                   end
           end,
    resolve_constraints(Constraints, Acc1).

binary_to_constraint(<<"*">>) ->
    '*';
binary_to_constraint(<<"^", VersionBin/binary>>) ->
    Version = to_version(VersionBin),
    {atleast, Version}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

expand_tag_test_() ->
    L = lists:seq(0,3),
    Tags = [<<"invalid">> | [version_to_binary({X,Y,Z}) || X <- L, Y <- L, Z <- L]],
    [?_assertEqual(<<"3.3.3">>, expand_tag(<<"*">>, Tags)),
     ?_assertEqual(<<"1.3.3">>, expand_tag(<<"1">>, Tags)),
     ?_assertEqual(<<"2.3.3">>, expand_tag(<<"2.3">>, Tags)),
     ?_assertEqual(<<"1.2.3">>, expand_tag(<<"1.2.3">>, Tags)),
     ?_assertError(_,           expand_tag(<<"4">>, Tags)),
     ?_assertError(_,           expand_tag(<<"3.4">>, Tags)),
     ?_assertError(_,           expand_tag(<<"*">>, [<<"no_valid_tags">>]))].
-endif.
