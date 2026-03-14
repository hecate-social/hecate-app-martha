%%% @doc Supervisor for fail_explorer desk.
-module(fail_explorer_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(explorer_failed_v1_to_pg)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
