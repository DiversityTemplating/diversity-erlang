-module(diversity_status_check).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-record(state, {}).

init(_, Req, Opts) ->
    Req, Opts,
    {ok, Req, #state{}}.

handle(Req, State) ->
    %% Reply ok if everyting seems ok!
    {ok, cowboy_req:reply(200, "OK", Req), State}.

terminate(_Reason , _Req, _State) ->
    ok.
