%%% @doc Martha application module.
%%%
%%% Starts the Martha supervision tree which manages all 9 domain
%%% app supervisors plus the SSE event bridge.
%%% @end
-module(martha_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    martha_sup:start_link().

stop(_State) ->
    ok.
