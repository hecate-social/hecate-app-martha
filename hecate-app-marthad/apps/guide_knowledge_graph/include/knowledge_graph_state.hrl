-ifndef(KNOWLEDGE_GRAPH_STATE_HRL).
-define(KNOWLEDGE_GRAPH_STATE_HRL, true).

-record(knowledge_graph_state, {
    venture_id     :: binary() | undefined,
    status = 0     :: non_neg_integer(),
    entities = #{} :: map(),       %% #{entity_id => #{type, name, description, source_agent, captured_at}}
    relationships = #{} :: map(),  %% #{rel_id => #{from_entity, to_entity, rel_type, strength, drawn_at}}
    insights = [] :: [map()],      %% Bounded list of recent insights
    initiated_at   :: integer() | undefined
}).

-endif.
