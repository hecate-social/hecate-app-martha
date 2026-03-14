%%% @doc Supervisor for escalate_design_gate desk (includes pass/reject sub-desks).
-module(escalate_design_gate_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(design_gate_escalated_v1_to_pg),
        #{id => pass_design_gate_sup,
          start => {pass_design_gate_sup, start_link, []},
          restart => permanent, type => supervisor},
        #{id => reject_design_gate_sup,
          start => {reject_design_gate_sup, start_link, []},
          restart => permanent, type => supervisor},
        emitter(on_stormer_completed_escalate_design_gate)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
