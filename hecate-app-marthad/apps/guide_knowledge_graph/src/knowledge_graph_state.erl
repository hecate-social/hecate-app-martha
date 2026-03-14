%%% @doc Knowledge graph state module — implements evoq_state behaviour.
%%%
%%% Owns the knowledge_graph_state record, initial state creation, event folding,
%%% and serialization. Extracted from knowledge_graph_aggregate.
-module(knowledge_graph_state).

-behaviour(evoq_state).

-include("knowledge_graph_status.hrl").
-include("knowledge_graph_state.hrl").

-export([new/1, apply_event/2, to_map/1]).

-type state() :: #knowledge_graph_state{}.
-export_type([state/0]).

-define(MAX_INSIGHTS, 500).

%% --- evoq_state callbacks ---

-spec new(binary()) -> state().
new(_AggregateId) ->
    #knowledge_graph_state{}.

-spec apply_event(state(), map()) -> state().

%% Normalize atom event_type to binary (from typed evoq_event modules)
apply_event(S, #{event_type := Type} = E) when is_atom(Type) ->
    apply_event(S, E#{event_type := atom_to_binary(Type, utf8)});

apply_event(S, #{event_type := <<"knowledge_graph_initiated_v1">>} = E)  -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"insight_captured_v1">>} = E)           -> apply_insight_captured(E, S);
apply_event(S, #{event_type := <<"entity_recognized_v1">>} = E)          -> apply_entity_recognized(E, S);
apply_event(S, #{event_type := <<"relationship_drawn_v1">>} = E)         -> apply_relationship_drawn(E, S);
apply_event(S, #{event_type := <<"insight_superseded_v1">>} = E)         -> apply_insight_superseded(E, S);
apply_event(S, #{event_type := <<"knowledge_graph_archived_v1">>})       -> apply_archived(S);
%% Unknown — ignore
apply_event(S, _E) -> S.

%% --- to_map ---

-spec to_map(state()) -> map().
to_map(#knowledge_graph_state{} = S) ->
    #{
        venture_id    => S#knowledge_graph_state.venture_id,
        status        => S#knowledge_graph_state.status,
        entities      => S#knowledge_graph_state.entities,
        relationships => S#knowledge_graph_state.relationships,
        insights      => S#knowledge_graph_state.insights,
        initiated_at  => S#knowledge_graph_state.initiated_at
    }.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#knowledge_graph_state{
        venture_id = get_value(venture_id, E),
        status = evoq_bit_flags:set(?KG_INITIATED, ?KG_ACTIVE),
        initiated_at = get_value(initiated_at, E)
    }.

apply_insight_captured(E, #knowledge_graph_state{insights = Insights} = State) ->
    Insight = #{
        insight_id => get_value(insight_id, E),
        content => get_value(content, E),
        source_agent => get_value(source_agent, E),
        source_session => get_value(source_session, E),
        insight_type => get_value(insight_type, E),
        captured_at => get_value(captured_at, E),
        superseded => false
    },
    Bounded = lists:sublist([Insight | Insights], ?MAX_INSIGHTS),
    State#knowledge_graph_state{insights = Bounded}.

apply_entity_recognized(E, #knowledge_graph_state{entities = Entities} = State) ->
    EntityId = get_value(entity_id, E),
    Entity = #{
        entity_type => get_value(entity_type, E),
        name => get_value(name, E),
        description => get_value(description, E),
        source_agent => get_value(source_agent, E),
        captured_at => get_value(captured_at, E)
    },
    State#knowledge_graph_state{entities = Entities#{EntityId => Entity}}.

apply_relationship_drawn(E, #knowledge_graph_state{relationships = Rels} = State) ->
    RelId = get_value(rel_id, E),
    Rel = #{
        from_entity => get_value(from_entity, E),
        to_entity => get_value(to_entity, E),
        rel_type => get_value(rel_type, E),
        strength => get_value(strength, E, 1.0),
        drawn_at => get_value(drawn_at, E)
    },
    State#knowledge_graph_state{relationships = Rels#{RelId => Rel}}.

apply_insight_superseded(E, #knowledge_graph_state{insights = Insights} = State) ->
    InsightId = get_value(insight_id, E),
    Updated = lists:map(fun(I) ->
        case maps:get(insight_id, I, undefined) of
            InsightId ->
                I#{superseded => true,
                   superseded_by => get_value(superseded_by, E),
                   superseded_at => get_value(superseded_at, E)};
            _ -> I
        end
    end, Insights),
    State#knowledge_graph_state{insights = Updated}.

apply_archived(#knowledge_graph_state{status = Status} = State) ->
    State#knowledge_graph_state{
        status = evoq_bit_flags:set(Status, ?KG_ARCHIVED)
    }.

%% --- Internal ---

get_value(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.

get_value(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
