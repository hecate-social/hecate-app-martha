%%% @doc Role-based relevance scoring for knowledge graph curation.
%%% Pure heuristics — no LLM needed. Scores insights and entities
%%% by how relevant they are to a given agent role.
-module(curate_context_relevance).
-export([score_insights/2, score_entities/2]).

%% @doc Score insights by relevance to the given agent role.
%% Returns [{Score, Insight}] sorted descending by score.
-spec score_insights(binary(), [map()]) -> [{non_neg_integer(), map()}].
score_insights(Role, Insights) ->
    Scored = [{score_insight(Role, I), I} || I <- Insights],
    lists:reverse(lists:keysort(1, Scored)).

%% @doc Score entities by relevance to the given agent role.
%% Returns [{Score, Entity}] sorted descending by score.
-spec score_entities(binary(), [map()]) -> [{non_neg_integer(), map()}].
score_entities(Role, Entities) ->
    Scored = [{score_entity(Role, E), E} || E <- Entities],
    lists:reverse(lists:keysort(1, Scored)).

%% --- Internal: Insight scoring ---

score_insight(Role, Insight) ->
    TypeScore = insight_type_score(Role, gv(<<"insight_type">>, Insight)),
    SourceScore = source_agent_score(Role, gv(<<"source_agent">>, Insight)),
    TypeScore + SourceScore.

%% Role-specific insight type relevance (3 pts max)
insight_type_score(<<"visionary">>, <<"gate_decision">>)     -> 3;
insight_type_score(<<"visionary">>, <<"constraint">>)        -> 3;
insight_type_score(<<"visionary">>, <<"user_segment">>)      -> 3;
insight_type_score(<<"visionary">>, <<"quality_warning">>)   -> 2;
insight_type_score(<<"explorer">>, <<"vision_decision">>)    -> 3;
insight_type_score(<<"explorer">>, <<"domain_boundary">>)    -> 3;
insight_type_score(<<"explorer">>, <<"constraint">>)         -> 2;
insight_type_score(<<"architect">>, <<"domain_boundary">>)   -> 3;
insight_type_score(<<"architect">>, <<"entity_type">>)       -> 3;
insight_type_score(<<"architect">>, <<"event_storming">>)    -> 3;
insight_type_score(<<"architect">>, <<"quality_warning">>)   -> 2;
insight_type_score(<<"stormer">>, <<"aggregate_design">>)    -> 3;
insight_type_score(<<"stormer">>, <<"domain_model">>)        -> 3;
insight_type_score(<<"stormer">>, <<"entity_type">>)         -> 2;
insight_type_score(<<"erlang_coder">>, <<"aggregate_design">>) -> 3;
insight_type_score(<<"erlang_coder">>, <<"event_storming">>)   -> 3;
insight_type_score(<<"erlang_coder">>, <<"quality_warning">>)  -> 2;
insight_type_score(<<"svelte_coder">>, <<"ui_design">>)      -> 3;
insight_type_score(<<"svelte_coder">>, <<"user_segment">>)   -> 2;
insight_type_score(<<"sql_coder">>, <<"aggregate_design">>)  -> 3;
insight_type_score(<<"sql_coder">>, <<"domain_model">>)      -> 3;
insight_type_score(<<"tester">>, <<"quality_warning">>)      -> 3;
insight_type_score(<<"tester">>, <<"aggregate_design">>)     -> 2;
insight_type_score(<<"reviewer">>, <<"gate_decision">>)      -> 3;
insight_type_score(<<"reviewer">>, <<"quality_warning">>)    -> 3;
insight_type_score(<<"reviewer">>, <<"constraint">>)         -> 2;
insight_type_score(<<"coordinator">>, <<"gate_decision">>)   -> 3;
insight_type_score(<<"coordinator">>, <<"quality_warning">>) -> 2;
insight_type_score(<<"mentor">>, <<"quality_warning">>)      -> 3;
insight_type_score(<<"mentor">>, <<"gate_decision">>)        -> 2;
insight_type_score(<<"delivery_manager">>, <<"constraint">>) -> 3;
insight_type_score(<<"delivery_manager">>, <<"gate_decision">>) -> 2;
insight_type_score(_Role, _Type) -> 1.

%% Source agent recency/relevance (2 pts max)
%% Agents that feed into the current role score higher
source_agent_score(<<"explorer">>, <<"visionary">>)         -> 2;
source_agent_score(<<"architect">>, <<"explorer">>)         -> 2;
source_agent_score(<<"architect">>, <<"stormer">>)          -> 2;
source_agent_score(<<"stormer">>, <<"architect">>)          -> 2;
source_agent_score(<<"erlang_coder">>, <<"architect">>)     -> 2;
source_agent_score(<<"erlang_coder">>, <<"stormer">>)       -> 2;
source_agent_score(<<"erlang_coder">>, <<"reviewer">>)      -> 2;
source_agent_score(<<"svelte_coder">>, <<"architect">>)     -> 2;
source_agent_score(<<"svelte_coder">>, <<"reviewer">>)      -> 2;
source_agent_score(<<"sql_coder">>, <<"architect">>)        -> 2;
source_agent_score(<<"sql_coder">>, <<"stormer">>)          -> 2;
source_agent_score(<<"tester">>, <<"erlang_coder">>)        -> 2;
source_agent_score(<<"tester">>, <<"svelte_coder">>)        -> 2;
source_agent_score(<<"reviewer">>, _AnySource)              -> 1;
source_agent_score(_Role, _Source) -> 0.

%% --- Internal: Entity scoring ---

score_entity(Role, Entity) ->
    entity_type_score(Role, gv(<<"type">>, Entity)).

%% Role-specific entity type relevance
entity_type_score(<<"architect">>, <<"aggregate">>)       -> 3;
entity_type_score(<<"architect">>, <<"bounded_context">>) -> 3;
entity_type_score(<<"architect">>, <<"domain_event">>)    -> 2;
entity_type_score(<<"stormer">>, <<"aggregate">>)         -> 3;
entity_type_score(<<"stormer">>, <<"domain_event">>)      -> 3;
entity_type_score(<<"stormer">>, <<"command">>)           -> 3;
entity_type_score(<<"erlang_coder">>, <<"aggregate">>)    -> 3;
entity_type_score(<<"erlang_coder">>, <<"domain_event">>) -> 3;
entity_type_score(<<"erlang_coder">>, <<"command">>)      -> 3;
entity_type_score(<<"sql_coder">>, <<"aggregate">>)       -> 3;
entity_type_score(<<"sql_coder">>, <<"read_model">>)      -> 3;
entity_type_score(<<"svelte_coder">>, <<"ui_component">>) -> 3;
entity_type_score(<<"svelte_coder">>, <<"read_model">>)   -> 2;
entity_type_score(<<"tester">>, <<"aggregate">>)          -> 2;
entity_type_score(<<"tester">>, <<"command">>)            -> 2;
entity_type_score(<<"reviewer">>, _AnyType)               -> 2;
entity_type_score(_Role, _Type) -> 1.

%% --- Internal: helpers ---

gv(Key, Map) when is_binary(Key) ->
    maps:get(Key, Map, undefined).
