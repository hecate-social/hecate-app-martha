%%% @doc Supervisor for initiate_svelte_coder desk.
-module(initiate_svelte_coder_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(svelte_coder_initiated_v1_to_pg),
        emitter(on_svelte_coder_initiated_run_svelte_coder_llm)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
