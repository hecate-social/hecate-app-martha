%%% @doc project_retry_strategy OTP application.
-module(project_retry_strategy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("[project_retry_strategy] Starting retry strategy projections"),
    project_retry_strategy_sup:start_link().

stop(_State) ->
    ok.
