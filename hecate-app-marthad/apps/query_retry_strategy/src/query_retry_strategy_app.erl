%%% @doc query_retry_strategy OTP application.
-module(query_retry_strategy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("[query_retry_strategy] Starting retry strategy query service"),
    query_retry_strategy_sup:start_link().

stop(_State) ->
    ok.
