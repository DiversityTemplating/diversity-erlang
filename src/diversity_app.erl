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
    cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    diversity_sup:start_link().

stop(_State) ->
    ok.
