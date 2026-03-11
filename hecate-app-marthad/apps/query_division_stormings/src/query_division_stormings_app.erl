-module(query_division_stormings_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    query_division_stormings_sup:start_link().

stop(_State) ->
    ok.
