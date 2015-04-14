-module(diversity_semver).

-export([check_constraint/2, resolve_version/2, binary_to_semver/1, semver_to_binary/1]).
-export_type([semver/0]).

-type semver() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()} | '*'.

check_constraint(Version, Constraint) ->
    Check = constraint_fun(Constraint),
    Check(Version).

resolve_version(Name, Constraints) ->
    Versions0 = diversity_api_client:get_versions(Name),
    Versions1 = filter_constraints(Constraints, Versions0),
    find_latest_version(Versions1).

filter_constraints([], Versions) ->
    Versions;
filter_constraints([<<"*">> | Constraints], Versions) ->
    filter_constraints(Constraints, Versions);
filter_constraints([Constraint | Constraints], Versions) ->
    ConstraintFilterFun = constraint_fun(Constraint),
    filter_constraints(Constraints, lists:filter(ConstraintFilterFun, Versions)).

constraint_fun(<<"^", VersionBin/binary>>) ->
    case binary_to_version(VersionBin) of
        {Major0} ->
            fun ({Major, _Minor, _Patch}) ->
                Major =:= Major0
            end;
        {Major0, Minor0} ->
            fun ({Major, Minor, _Patch}) ->
                Major =:= Major0 andalso Minor >= Minor0
            end;
        {Major0, Minor0, Patch0} ->
            fun ({Major, Minor, Patch}) ->
                Major =:= Major0 andalso {Minor, Patch} >= {Minor0, Patch0}
            end
    end.

%% @doc Retrive the latest version according to the given comparsion function
find_latest_version(Versions) ->
    find_latest_version({-1, -1, -1}, Versions).

find_latest_version({-1, -1, -1}, []) ->
    undefined;
find_latest_version(VersionAcc, []) ->
    VersionAcc;
find_latest_version(VersionAcc, [Version | Versions]) when Version > VersionAcc ->
    find_latest_version(Version, Versions);
find_latest_version(VersionAcc, [_ | Versions]) ->
    find_latest_version(VersionAcc, Versions).

%% @doc Version to binary
%% May throw error:badarg.
semver_to_binary({Major, Minor, Patch}) when Major >= 0, Minor >= 0, Patch >= 0 ->
    <<(integer_to_binary(Major))/binary, $.,
      (integer_to_binary(Minor))/binary, $.,
      (integer_to_binary(Patch))/binary>>;
semver_to_binary('*') ->
    <<"*">>.

binary_to_semver(BinaryTag) when is_binary(BinaryTag) ->
    case binary_to_version(BinaryTag) of
        {_Major, _Minor, _Patch} = Version -> Version;
        _                                  -> error(badarg)
    end.

%% @doc Binary or list to version tuple
binary_to_version(BinaryTag) when is_binary(BinaryTag) ->
    Parts = binary:split(BinaryTag, <<$.>>, [global]),
    VersionList = lists:map(
                      fun (Part) ->
                          case binary_to_integer(Part) of
                              Integer when Integer >= 0 -> Integer;
                              _                         -> error(badarg)
                          end
                      end,
                      Parts
                  ),
    list_to_tuple(VersionList).
