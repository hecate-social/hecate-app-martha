%%% @doc guide_retry_strategy OTP application.
%%% Manages adaptive retry lifecycle for agent sessions.
-module(guide_retry_strategy_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:info("[guide_retry_strategy] Starting retry strategy service"),
    guide_retry_strategy_sup:start_link().

stop(_State) ->
    ok.
