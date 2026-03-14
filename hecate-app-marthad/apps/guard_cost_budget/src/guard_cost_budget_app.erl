%%% @doc guard_cost_budget application behaviour
-module(guard_cost_budget_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    guard_cost_budget_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
