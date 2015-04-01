-module(diversity_http).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-record(state, {}).

init(_, Req, Opts) ->
    Req, Opts,
    {ok, Req, #state{}}.


%%% Context;
    % Webshopid
    % WebshopUrl
    % Theme.params

handle(Req, State) ->
    try
        Req2 = handle_diversity_request(Req),
        {ok, Req2, State}
    catch
        throw:no_theme_found ->
            cowboy_req:reply(
                500, [{<<"content-type">>, <<"text/plain">>}], <<"No theme found">>, Req
            );
        throw:no_webshop_found ->
            cowboy_req:reply(
                200, [{<<"content-type">>, <<"text/plain">>}], <<"No webshop found">>, Req
            )

    end.

handle_diversity_request(Req) ->
    Req1 = redirect_if_preview(Req),
    %% Check if we have a theme_id on cookie
    {ThemeIdCookie, Req2} = cowboy_req:cookie(<<"lang">>, Req1, <<>>),
    {ThemeId, PreviewKey} = case binary:split(ThemeIdCookie, <<":">>) of
        [<<>>]                  -> {undefined, undefined};
        [ThemeId0]              -> {ThemeId0, undefined};
        [ThemeId0, <<>>]        -> {ThemeId0, undefined};
        [ThemeId0, PreviewKey0] -> {ThemeId0, PreviewKey0}
    end,

    {Url, Req3} = cowboy_req:url(Req2),


    %% Get webshop id with Url.get with the url we got and create a context to work with
    UrlPropList = diversity_twapi_client:url_get(Url, PreviewKey),
    WebshopUid = proplists:get_value(webshop_uid, UrlPropList),
    {Headers, _} = cowboy_req:headers(Req3),
    Headers1 = header_key_value_to_list(Headers, []),
    Theme = case ThemeId of
        undefined ->
            %% If we have no theme_id on do theme select
            %% Now if we still have no theme id we should return failure
            diversity_twapi_client:theme_select(WebshopUid, PreviewKey, Headers1);
        _ ->
            try
                %% If we have a theme id on cookie do a Theme.get
                diversity_twapi_client:theme_get(ThemeId, PreviewKey, WebshopUid)
            catch
                _ ->
                    %% if object not found we do a Theme.select and try getting the correct theme id
                    %% (Corner case we should fix that!)
                    diversity_twapi_client:theme_select(WebshopUid, PreviewKey, Headers1)
            end
    end,

    {ok, APIUrl} = application:get_env(diversity, twapi_url),

    ContextMap = #{webshopUrl => proplists:get_value(webshop_url, UrlPropList),
                   webshop    => WebshopUid,
                   language   => proplists:get_value(language, UrlPropList),
                   apiUrl     => APIUrl},
    try
        %% All good? Send to renderer and let the magic happen in a nice try block.
        Output = diversity:render(maps:get(<<"params">>, Theme), ContextMap),
        {ok, _} = cowboy_req:reply(
            200, [{<<"content-type">>, <<"text/html">>}], Output, Req3
        )
    catch
        _ ->
            {ok, _} = cowboy_req:reply(
                500, [{<<"content-type">>, <<"text/plain">>}], "Internal server error", Req3
            )
    end,
    Req3.

redirect_if_preview(Req) ->
    case cowboy_req:qs_val(<<"preview_key">>, Req, undefined) of
        {undefined, _} ->
            %% do nothing
            Req;
        {PreviewKey, Req1} ->
            {ThemeId, Req2} = cowboy_req:qs_val(<<"theme_id">>, Req1, null),
            %% Set it on the cookie and respond 302 and redirect back to us but remove qs part.
            %% Rebuild QS
            {QsPropList, Req3} = cowboy_req:qs_vals(Req2),
            QueryString = lists:foldr(
                fun ({Key, Value}, Acc) ->
                    case Key of
                        <<"theme_id">> ->
                            Acc;
                        <<"preview_key">> ->
                            Acc;
                        _ ->
                            case Acc of
                                <<>> -> <<"?", Key/binary, "=", Value/binary>>;
                                _    -> <<Acc/binary, "&", Key/binary, "=", Value/binary>>
                            end
                    end
                end,
                <<>>,
                QsPropList
            ),
            {HostUrl, Req4} = cowboy_req:host_url(Req3),

            Req5 = cowboy_req:set_resp_cookie(
                <<"theme_id">>, <<ThemeId/binary, ":", PreviewKey/binary>>, [], Req4
            ),
            cowboy_req:reply(
                302,
                [{<<"Location">>, <<HostUrl/binary, "/", QueryString/binary>>}],
                <<"Redirecting with Header!">>,
                Req5
            )
    end.

header_key_value_to_list([{Key, Value} | Rest], Acc)->
    Acc1 = [{binary_to_list(Key), binary_to_list(Value)} | Acc],
    header_key_value_to_list(Rest, Acc1);
header_key_value_to_list([], Acc) ->
    lists:reverse(Acc).

terminate(_Reason, _Req, _State) ->
    ok.
