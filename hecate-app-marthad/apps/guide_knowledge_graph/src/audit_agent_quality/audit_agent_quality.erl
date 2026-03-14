%%% @doc Quality auditor — cross-checks agent outputs against accumulated knowledge.
%%% Stateless desk in guide_knowledge_graph. Runs after each agent completion,
%%% comparing new output against existing knowledge graph. Produces warnings
%%% as capture_insight commands with insight_type "quality_warning".
-module(audit_agent_quality).
-export([audit/3]).

%% @doc Audit an agent's output against the knowledge graph.
%% Returns list of warning binaries (empty = no issues found).
-spec audit(binary(), binary(), binary()) -> {ok, [binary()]}.
audit(VentureId, AgentRole, NotationOutput) ->
    case project_knowledge_graph_store:get_graph(VentureId) of
        {ok, Graph} ->
            Warnings = audit_agent_quality_rules:check(AgentRole, NotationOutput, Graph),
            {ok, Warnings};
        {error, not_found} ->
            {ok, []}
    end.
