-module(diversity_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->

    Routes = [
      {"/favicon.ico", diversity_resource_handler, favicon},
      {"/sitemap.xml", diversity_resource_handler, sitemap},
      {"/backend/ha/check.txt", diversity_status_check, []},
      {"/", diversity_http, []}
    ],
    Dispatch = cowboy_router:compile([
        {'_', Routes}
    ]),
    {ok, Port} = application:get_env('diversity', port),
    lager:info("Diversity-erlang started. Listening to port ~p", [Port]),
    case application:get_env(diversity, debug) of
        {ok, true} ->
            {ok, DebugUrl} = application:get_env(diversity, debug_url),
            lager:info("-------------------------------------------------------------------------"),
            lager:info("Debug mode active and will always use ~s", [DebugUrl]),
            lager:info("-------------------------------------------------------------------------");
        _ ->
            ok
    end,

    cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    diversity_sup:start_link().

stop(_State) ->
    ok.
