%%% @doc guide_knowledge_graph top-level supervisor.
%%%
%%% Process managers subscribe to orchestration_store events and dispatch
%%% commands to knowledge_graph_store to capture cross-agent knowledge.
%%% @end
-module(guide_knowledge_graph_sup).
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
        pm(on_agent_completed_capture_insight, orchestration_store),
        pm(on_vision_gate_passed_capture_insight, orchestration_store),
        pm(on_vision_gate_rejected_capture_insight, orchestration_store),
        pm(on_agent_completed_audit_quality, orchestration_store),
        pm(on_entity_recognized_detect_conflicts, knowledge_graph_store)
    ],
    {ok, {SupFlags, Children}}.

pm(Mod, StoreId) ->
    #{id => Mod,
      start => {evoq_event_handler, start_link, [Mod, #{store_id => StoreId}]},
      restart => permanent,
      type => worker}.
