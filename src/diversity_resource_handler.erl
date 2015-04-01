-module(diversity_resource_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-record(state, {resource}).

init(_, Req, Opts) ->
    {ok, Req, #state{resource = Opts}}.

%% @doc Handles different resource types like favicon and sitemap.xml
%% @todo Implement a favicon fetcher
%% @todo Implement sitemap.xml generator.
handle(Req, #state{resource = favicon} = State) ->
    Req2 = cowboy_req:reply(404, "Favicon not implemented", Req),
    {ok, Req2, State};
handle(Req, #state{resource = sitemap} = State) ->
    Req2 = cowboy_req:reply(404, "Sitemap not implemented", Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
