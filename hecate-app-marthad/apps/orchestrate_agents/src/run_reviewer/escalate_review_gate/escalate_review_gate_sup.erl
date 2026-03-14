%%% @doc Supervisor for escalate_review_gate desk (includes pass/reject sub-desks).
-module(escalate_review_gate_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        emitter(review_gate_escalated_v1_to_pg),
        #{id => pass_review_gate_sup,
          start => {pass_review_gate_sup, start_link, []},
          restart => permanent, type => supervisor},
        #{id => reject_review_gate_sup,
          start => {reject_review_gate_sup, start_link, []},
          restart => permanent, type => supervisor},
        emitter(on_reviewer_completed_escalate_review_gate)
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
