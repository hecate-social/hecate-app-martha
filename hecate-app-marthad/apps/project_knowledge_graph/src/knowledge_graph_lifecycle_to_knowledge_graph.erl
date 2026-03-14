%%% @doc Merged projection: all knowledge graph events to one ETS table.
%%%
%%% Single gen_server guarantees sequential event processing (no race).
%%% @end
-module(knowledge_graph_lifecycle_to_knowledge_graph).

-behaviour(evoq_projection).

-include_lib("guide_knowledge_graph/include/knowledge_graph_status.hrl").

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_knowledge_graph).

interested_in() ->
    [<<"knowledge_graph_initiated_v1">>,
     <<"insight_captured_v1">>,
     <<"entity_recognized_v1">>,
     <<"relationship_drawn_v1">>,
     <<"insight_superseded_v1">>,
     <<"knowledge_graph_archived_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

%% --- Birth event ---
do_project(<<"knowledge_graph_initiated_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    Status = evoq_bit_flags:set(?KG_INITIATED, ?KG_ACTIVE),
    Graph = #{
        venture_id => VentureId,
        status => Status,
        status_label => evoq_bit_flags:to_string(Status, ?KG_FLAG_MAP),
        entities => #{},
        relationships => #{},
        insights => [],
        entity_count => 0,
        relationship_count => 0,
        insight_count => 0,
        initiated_at => gf(initiated_at, Data)
    },
    {ok, RM2} = evoq_read_model:put(VentureId, Graph, RM),
    {ok, State, RM2};

%% --- Insight captured ---
do_project(<<"insight_captured_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    update_graph(VentureId, State, RM, fun(G) ->
        Insight = #{
            insight_id => gf(insight_id, Data),
            content => gf(content, Data),
            source_agent => gf(source_agent, Data),
            source_session => gf(source_session, Data),
            insight_type => gf(insight_type, Data),
            captured_at => gf(captured_at, Data),
            superseded => false
        },
        Insights = maps:get(insights, G, []),
        G#{insights => [Insight | Insights],
           insight_count => length(Insights) + 1}
    end);

%% --- Entity recognized ---
do_project(<<"entity_recognized_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    update_graph(VentureId, State, RM, fun(G) ->
        EntityId = gf(entity_id, Data),
        Entity = #{
            entity_id => EntityId,
            entity_type => gf(entity_type, Data),
            name => gf(name, Data),
            description => gf(description, Data),
            source_agent => gf(source_agent, Data),
            captured_at => gf(captured_at, Data)
        },
        Entities = maps:get(entities, G, #{}),
        G#{entities => Entities#{EntityId => Entity},
           entity_count => maps:size(Entities) + 1}
    end);

%% --- Relationship drawn ---
do_project(<<"relationship_drawn_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    update_graph(VentureId, State, RM, fun(G) ->
        RelId = gf(rel_id, Data),
        Rel = #{
            rel_id => RelId,
            from_entity => gf(from_entity, Data),
            to_entity => gf(to_entity, Data),
            rel_type => gf(rel_type, Data),
            strength => gf(strength, Data, 1.0),
            drawn_at => gf(drawn_at, Data)
        },
        Rels = maps:get(relationships, G, #{}),
        G#{relationships => Rels#{RelId => Rel},
           relationship_count => maps:size(Rels) + 1}
    end);

%% --- Insight superseded ---
do_project(<<"insight_superseded_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    InsightId = gf(insight_id, Data),
    update_graph(VentureId, State, RM, fun(G) ->
        Insights = maps:get(insights, G, []),
        Updated = lists:map(fun(I) ->
            case maps:get(insight_id, I, undefined) of
                InsightId ->
                    I#{superseded => true,
                       superseded_by => gf(superseded_by, Data),
                       superseded_at => gf(superseded_at, Data)};
                _ -> I
            end
        end, Insights),
        G#{insights => Updated}
    end);

%% --- Archived ---
do_project(<<"knowledge_graph_archived_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    update_graph(VentureId, State, RM, fun(G) ->
        OldStatus = maps:get(status, G, 0),
        NewStatus = evoq_bit_flags:set(OldStatus, ?KG_ARCHIVED),
        G#{status => NewStatus,
           status_label => evoq_bit_flags:to_string(NewStatus, ?KG_FLAG_MAP)}
    end);

do_project(_EventType, _Data, State, RM) ->
    {ok, State, RM}.

%%====================================================================
%% Helpers
%%====================================================================

update_graph(VentureId, State, RM, UpdateFun) ->
    case evoq_read_model:get(VentureId, RM) of
        {ok, G} ->
            Updated = UpdateFun(G),
            {ok, RM2} = evoq_read_model:put(VentureId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(#{event_type := T}) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.

gf(Key, Data, Default) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, Default)
    end.
