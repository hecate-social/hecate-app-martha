%%% @doc Supervisor for guide_retry_strategy.
%%% Starts process managers that react to gate rejections and retry events.
-module(guide_retry_strategy_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        pm(on_vision_gate_rejected_initiate_retry, orchestration_store),
        pm(on_retry_attempted_reinitiate_agent, retry_strategy_store)
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.

pm(Mod, StoreId) ->
    #{id => Mod,
      start => {evoq_event_handler, start_link, [Mod, #{store_id => StoreId}]},
      restart => permanent,
      type => worker}.
