%%% @doc Supervisor for initiate_architect desk.
-module(initiate_architect_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(architect_initiated_v1_to_pg),
        emitter(on_team_formed_initiate_architect),
        emitter(on_architect_initiated_run_architect_llm)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
