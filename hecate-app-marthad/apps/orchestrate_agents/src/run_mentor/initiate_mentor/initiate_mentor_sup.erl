%%% @doc Supervisor for initiate_mentor desk.
-module(initiate_mentor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(mentor_initiated_v1_to_pg),
        emitter(on_venture_initiated_initiate_mentor),
        emitter(on_mentor_initiated_run_mentor_llm)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
