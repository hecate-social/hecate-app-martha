%%% @doc Supervisor for initiate_explorer desk.
-module(initiate_explorer_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(explorer_initiated_v1_to_pg),
        emitter(on_vision_gate_passed_initiate_explorer),
        emitter(on_explorer_initiated_run_explorer_llm)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
