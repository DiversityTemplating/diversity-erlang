-module(diversity_twapi_client).

-export([url_get/2, theme_get/3, theme_select/3]).

url_get(Url, PreviewKey) ->
    try
        Url1 = case application:get_env(diversity, debug) of
            {ok, true}  ->
                {ok, DebugUrl} = application:get_env(diversity, debug_url),
                DebugUrl;
            {ok, false} ->
                Url
        end,
        QsBin = build_qs_bin({auth, PreviewKey}),
        [{ok, UrlObj}] = post_to_twapi([{<<"Url.get">>, [Url1, true]}], QsBin),
        {Url2, UrlObj1} = case maps:get(<<"type">>, UrlObj) of
            <<"Moved">> ->
                MovedUrl = maps:get(<<"url">>, UrlObj),
                [{ok, MovedUrlProps}] = post_to_twapi([{<<"Url.get">>, [MovedUrl, true]}], QsBin),
                {MovedUrl, MovedUrlProps};
            _ ->
                {Url1, UrlObj}
        end,
        [{language, maps:get(<<"language">>, UrlObj1)},
         {webshop_uid, maps:get(<<"webshop">>, UrlObj1)},
         {webshop_url, Url2}]
    catch
        _ ->
            throw(no_webshop_found)
    end.

theme_get(ThemeId, WebshopId, PreviewKey) ->
    try
        QsBin = build_qs_bin([{auth, PreviewKey}, {webshop, WebshopId}]),
        [{ok, Theme}] =
            post_to_twapi(
                [{<<"Theme.get">>, [ThemeId, <<"params">>]}],
                QsBin
        ),
        Theme
    catch
        _ -> throw(no_theme_found)
    end.

theme_select(WebshopId, PreviewKey, Headers) ->
    try
        QsBin = build_qs_bin([{auth, PreviewKey}, {webshop, WebshopId}]),
        [{ok, Theme}] =
            post_to_twapi(
                [{<<"Theme.select">>, [<<"params">>]}],
                QsBin,
                Headers
            ),
        Theme
    catch
        _ ->
            throw(no_theme_found)
    end.

build_qs_bin(List) when is_list(List) ->
    lists:foldl(
        fun (KeyValue, QsBin) ->
            QsBin1 = case QsBin of
                <<>> -> build_qs_bin(KeyValue);
                _    -> <<QsBin/binary, (build_qs_bin(KeyValue))/binary>>
            end,
            QsBin1
        end,
        <<>>,
        List
    );
build_qs_bin({_Key, undefined}) ->
    <<>>;
build_qs_bin({Key, Value}) when is_binary(Value) ->
    <<(atom_to_binary(Key, utf8))/binary, "=", Value/binary>>;
build_qs_bin({Key, Value}) when is_integer(Value) ->
    <<(atom_to_binary(Key, utf8))/binary, "=", (integer_to_binary(Value))/binary>>.

post_to_twapi(MethodAndParamsPair, ContextQs) ->
    post_to_twapi(MethodAndParamsPair, ContextQs, []).

post_to_twapi(MethodAndParamsPair, ContextQs, Headers) ->
    {ok, Url} = application:get_env(diversity, twapi_url),
    jsonrpc2_client:batch_call(
        MethodAndParamsPair,
        fun (PostData) ->
            Opts = [{body_format, binary}],
            Url1 = case ContextQs of
                <<>> -> Url;
                _    -> <<Url/binary, "?", ContextQs/binary>>
            end,
            Request = {binary_to_list(Url1), Headers, "application/json", PostData},
            case httpc:request(post, Request, [], Opts) of
                {ok, {{_Version, _Status, _ReasonPhrase}, _Headers, Body}} ->
                    Body
            end
        end,
        % fun jiffy:decode/1,
        fun (EncodedJson) ->
            jiffy:decode(EncodedJson, [return_maps])
        end,
        fun jiffy:encode/1,
        1
    ).
