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
        throw:no_url_info ->
            cowboy_req:reply(
                200, [{<<"content-type">>, <<"text/plain">>}], <<"No webshop found">>, Req
            )

    end.

handle_diversity_request(Req) ->
    Req1 = redirect_if_preview(Req),
    %% Check if we have a theme_id on cookie
    {ThemeId, PreviewKey} = get_themeid_and_from_cookie(Req),
    {Url, Req3} = cowboy_req:url(Req1),

    %% Get webshop id with Url.get with the url we got and create a context to work with
    UrlInfo = case get_url_info(Url, PreviewKey, Req) of
        {error, no_url_info} -> throw(no_url_info);
        UrlInfo0 -> UrlInfo0
    end,

    WebshopUid = proplists:get_value(webshop_uid, UrlInfo),
    {Headers, _} = cowboy_req:headers(Req3),
    Headers1 = header_key_value_to_list(Headers, []),
    Theme = get_theme(ThemeId, WebshopUid, PreviewKey, Headers1),

    {ok, APIUrl} = application:get_env(diversity, twapi_url),
    {ok, DiversityURL} = application:get_env(diversity, diversity_api_url),

    Language = proplists:get_value(language, UrlInfo),
    Context = #{<<"webshopUrl">> => proplists:get_value(webshop_url, UrlInfo),
                <<"webshopUid">> => WebshopUid,
                <<"apiUrl">>     => APIUrl,
                <<"serveWithStyleUrl">> => <<DiversityURL/binary, $/>>},
    try
        lager:info("Building ~p ~s", [WebshopUid, proplists:get_value(webshop_url, UrlInfo)]),
        %% All good? Send to renderer and let the magic happen in a nice try block.
        Output = case application:get_env(diversity, verbose_log) of
            {ok, true} ->
                {Time, Result} = timer:tc(diversity,
                                          render, [maps:get(<<"params">>, Theme), Language, Context]),
                lager:info("Rendering page took ~p", [Time]),
                Result;
            _ ->
               diversity:render(maps:get(<<"params">>, Theme), Language, Context)
        end,
        {ok, _} = cowboy_req:reply(
            200, [{<<"content-type">>, <<"text/html">>}], Output, Req3
        )
    catch
        Class:Error ->
            io:format("~p ~p ~n", [Class, Error]),
            {ok, _} = cowboy_req:reply(
                500, [{<<"content-type">>, <<"text/plain">>}], "Internal server error", Req3
            )
    end,
    Req3.

get_themeid_and_from_cookie(Req) ->
    {ThemeIdCookie, _} = cowboy_req:cookie(<<"themeid">>, Req, <<>>),
    case binary:split(ThemeIdCookie, <<":">>) of
        [<<>>]                  -> {undefined, undefined};
        [ThemeId0]              -> {ThemeId0, undefined};
        [ThemeId0, <<>>]        -> {ThemeId0, undefined};
        [ThemeId0, PreviewKey0] -> {ThemeId0, PreviewKey0}
    end.

get_url_info(Url, PreviewKey, Req) ->
    %% Do we have debug mode on and a debug url? Use that instead.
    Url1 = case application:get_env(diversity, debug) of
        {ok, true}  ->
            {ok, DebugUrl} = application:get_env(diversity, debug_url),
            DebugUrl;
        {ok, false} ->
            Url
    end,
    %% Check if this is a stage first and then rewrite it!
    %% Are we in a staging enviroment? Then perhaps we need to rewrite the url so we get the right
    %% data from twapi. This is a specific textalk environment issue.
    %% Probably break this out in a separate module handling staging.
    Url2 = case application:get_env(diversity, staging) of
        {ok, true} ->
            {ok, StagingRegExp} = application:get_env(diversity, staging_regexp),
            StagingRegExp /= undefined orelse Url1,
            % [SplittedUrl, _] =
            RealUrl = case re:split(Url1, StagingRegExp) of
                SplittedUrl when SplittedUrl == 1 -> Url1;
                [SplittedUrl, _]                  -> SplittedUrl
            end,
            {PathPart, _} = cowboy_req:path(Req),
            {Qs, _} = cowboy_req:qs(Req),
            <<RealUrl/binary, PathPart/binary, Qs/binary>>;
        _ ->
          Url1
    end,

    case diversity_twapi_client:get_url_info(Url2, PreviewKey) of
        {error, no_url_info} ->
            %% We haven't got any webshop id with the provided url.
            %% Check against the hostname and rebuild the url if we get a new host url
            {HostUrl, _} = cowboy_req:host_url(Req),
            HostUrlInfo = diversity_twapi_client:url_get(HostUrl, PreviewKey),
            HostUrlInfo /= {error, no_url_info} orelse throw(no_url_info),
            {Path, _} = cowboy_req:host_url(Req),
            WebshopUrl = proplists:get_value(webshop_url, HostUrlInfo),
            NewUrl = <<WebshopUrl/binary, Path/binary>>,
            diversity_twapi_client:url_get(NewUrl, PreviewKey);
        UrlInfo0 when is_list(UrlInfo0) ->
            UrlInfo0
    end.

get_theme(undefined, WebshopUid, PreviewKey, Headers) ->
    %% If we have no theme_id on do theme select
    %% Now if we still have no theme id we should return failure
    diversity_twapi_client:theme_select(WebshopUid, PreviewKey, Headers);
get_theme(ThemeId, WebshopUid, PreviewKey, Headers) ->
    try
        %% If we have a theme id on cookie do a Theme.get
        diversity_twapi_client:theme_get(ThemeId, PreviewKey, WebshopUid)
    catch
        _ ->
            %% if object not found we do a Theme.select and try getting the correct theme id
            %% (Corner case we should fix that!)
            diversity_twapi_client:theme_select(WebshopUid, PreviewKey, Headers)
    end.

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
