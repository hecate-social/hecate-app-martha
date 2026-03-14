%%% @doc guard_cost_budget top-level supervisor
%%%
%%% Supervises process managers for cost budget domain:
%%% - on_llm_call_tracked_record_spending: reacts to LLM usage, dispatches spending
%%% - on_budget_breached_pause_agents: reacts to budget breach, notifies via pg
%%% @end
-module(guard_cost_budget_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },

    Children = [
        %% ── Process Managers ─────────────────────────────────────────────────

        %% LLM call tracked -> record spending against budget
        emitter(on_llm_call_tracked_record_spending),

        %% Budget breached -> notify via pg for frontend
        emitter(on_budget_breached_pause_agents)
    ],

    {ok, {SupFlags, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
