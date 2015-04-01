-module(diversity_util).

-export([fold/4, get_diversity_json/2, get_settings_schema/2, get_file/3]).

-include_lib("eunit/include/eunit.hrl").

fold(#{<<"component">> := Name, <<"settings">> := Settings} = Component,
     #{<<"type">> := <<"object">>, <<"format">> := <<"diversity">>},
     Fun, Acc0) ->
    Version = maps:get(<<"version">>, Component, <<"*">>),
    Acc1 = Fun(Component, Acc0),
    {ok, Schema} = get_settings_schema(Name, Version),
    fold(Settings, Schema, Fun, Acc1);
fold(Settings, #{<<"type">> := <<"object">>, <<"properties">> := Properties}, Fun, Acc0) when is_map(Settings) ->
    maps:fold(
      fun (Property, Value, Acc) ->
              case maps:find(Property, Properties) of
                  {ok, PropertySchema} -> fold(Value, PropertySchema, Fun, Acc);

                  _            -> Acc
              end
      end,
      Acc0,
      Settings
     );
fold(Settings, #{<<"type">> := <<"array">>, <<"items">> := Schema}, Fun, Acc0) when is_list(Settings) ->
    lists:foldl(fun (Item, Acc) -> fold(Item, Schema, Fun, Acc) end, Acc0, Settings);
fold(_Settings, _Schema, _Fun, Acc) ->
    Acc.

get_diversity_json(Component, Version) ->
    Path = unicode:characters_to_list(<<Component/binary, $/, Version/binary>>),
    URL = "http://api.diversity.io/components/" ++ Path,
    get_json(URL).

get_settings_schema(Component, Version) ->
    Path = unicode:characters_to_list(<<Component/binary, "/", Version/binary, "/settings">>),
    URL = "http://api.diversity.io/components/" ++ Path,
    get_json(URL).

get_json(URL) ->
    case get_file(URL) of
        {ok, Data} -> {ok, jiffy:decode(Data, [return_maps])};
        error      -> error
    end.

get_file(Component, Version, Path) ->
    FullPath = unicode:characters_to_list(<<Component/binary, "/", Version/binary, "/files/", Path/binary>>),
    URL = "http://api.diversity.io/components/" ++ FullPath,
    get_file(URL).

get_file(URL) ->
    case httpc:request(get, {URL, []}, [], [{body_format, binary}]) of
        {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}} ->
            {ok, Body};
        _ ->
            error
    end.
