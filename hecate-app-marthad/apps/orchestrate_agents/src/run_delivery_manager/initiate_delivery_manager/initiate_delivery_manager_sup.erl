%%% @doc Supervisor for initiate_delivery_manager desk.
-module(initiate_delivery_manager_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(delivery_manager_initiated_v1_to_pg),
        emitter(on_team_formed_initiate_delivery_manager),
        emitter(on_delivery_manager_initiated_run_delivery_manager_llm)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
