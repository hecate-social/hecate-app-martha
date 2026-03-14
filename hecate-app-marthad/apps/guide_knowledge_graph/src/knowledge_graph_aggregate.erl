%%% @doc Knowledge graph aggregate — persistent cross-agent memory.
%%%
%%% Stream: knowledge-{venture_id}
%%% Store: knowledge_graph_store
%%%
%%% Lifecycle:
%%%   1. initiate_knowledge_graph (birth event)
%%%   2. capture_insight / recognize_entity / draw_relationship / supersede_insight
%%%   3. archive_knowledge_graph (walking skeleton)
%%% @end
-module(knowledge_graph_aggregate).

-behaviour(evoq_aggregate).

-include("knowledge_graph_status.hrl").
-include("knowledge_graph_state.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([flag_map/0]).

-type state() :: #knowledge_graph_state{}.
-export_type([state/0]).

-define(MAX_INSIGHTS, 500).

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?KG_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(_AggregateId) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #knowledge_graph_state{}.

%% --- Execute ---

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh aggregate — only initiate allowed
execute(#knowledge_graph_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"initiate_knowledge_graph">> -> execute_initiate(Payload);
        _ -> {error, knowledge_graph_not_initiated}
    end;

%% Archived — nothing allowed
execute(#knowledge_graph_state{status = S}, _Payload) when S band ?KG_ARCHIVED =/= 0 ->
    {error, knowledge_graph_archived};

%% Active — route by command type
execute(#knowledge_graph_state{status = S} = State, Payload) when S band ?KG_INITIATED =/= 0 ->
    case get_command_type(Payload) of
        <<"capture_insight">>          -> execute_capture_insight(Payload, State);
        <<"recognize_entity">>         -> execute_recognize_entity(Payload, State);
        <<"draw_relationship">>        -> execute_draw_relationship(Payload, State);
        <<"supersede_insight">>        -> execute_supersede_insight(Payload, State);
        <<"archive_knowledge_graph">>  -> execute_archive(Payload, State);
        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Command handlers ---

execute_initiate(Payload) ->
    {ok, Cmd} = initiate_knowledge_graph_v1:from_map(Payload),
    convert_events(maybe_initiate_knowledge_graph:handle(Cmd),
                   fun knowledge_graph_initiated_v1:to_map/1).

execute_capture_insight(Payload, _State) ->
    {ok, Cmd} = capture_insight_v1:from_map(Payload),
    convert_events(maybe_capture_insight:handle(Cmd),
                   fun insight_captured_v1:to_map/1).

execute_recognize_entity(Payload, State) ->
    {ok, Cmd} = recognize_entity_v1:from_map(Payload),
    Context = #{entities => State#knowledge_graph_state.entities},
    convert_events(maybe_recognize_entity:handle(Cmd, Context),
                   fun entity_recognized_v1:to_map/1).

execute_draw_relationship(Payload, State) ->
    {ok, Cmd} = draw_relationship_v1:from_map(Payload),
    Context = #{entities => State#knowledge_graph_state.entities},
    convert_events(maybe_draw_relationship:handle(Cmd, Context),
                   fun relationship_drawn_v1:to_map/1).

execute_supersede_insight(Payload, State) ->
    {ok, Cmd} = supersede_insight_v1:from_map(Payload),
    Context = #{insights => State#knowledge_graph_state.insights},
    convert_events(maybe_supersede_insight:handle(Cmd, Context),
                   fun insight_superseded_v1:to_map/1).

execute_archive(Payload, _State) ->
    {ok, Cmd} = archive_knowledge_graph_v1:from_map(Payload),
    convert_events(maybe_archive_knowledge_graph:handle(Cmd),
                   fun knowledge_graph_archived_v1:to_map/1).

%% --- Apply ---

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    apply_event(Event, State).

-spec apply_event(map(), state()) -> state().

apply_event(#{event_type := <<"knowledge_graph_initiated_v1">>} = E, S)      -> apply_initiated(E, S);
apply_event(#{event_type := <<"insight_captured_v1">>} = E, S)               -> apply_insight_captured(E, S);
apply_event(#{event_type := <<"entity_recognized_v1">>} = E, S)              -> apply_entity_recognized(E, S);
apply_event(#{event_type := <<"relationship_drawn_v1">>} = E, S)             -> apply_relationship_drawn(E, S);
apply_event(#{event_type := <<"insight_superseded_v1">>} = E, S)             -> apply_insight_superseded(E, S);
apply_event(#{event_type := <<"knowledge_graph_archived_v1">>} = _E, S)       -> apply_archived(S);
apply_event(_E, S) -> S.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#knowledge_graph_state{
        venture_id = gv(venture_id, E),
        status = evoq_bit_flags:set(?KG_INITIATED, ?KG_ACTIVE),
        initiated_at = gv(initiated_at, E)
    }.

apply_insight_captured(E, #knowledge_graph_state{insights = Insights} = State) ->
    Insight = #{
        insight_id => gv(insight_id, E),
        content => gv(content, E),
        source_agent => gv(source_agent, E),
        source_session => gv(source_session, E),
        insight_type => gv(insight_type, E),
        captured_at => gv(captured_at, E),
        superseded => false
    },
    %% Bounded list — keep most recent
    Bounded = lists:sublist([Insight | Insights], ?MAX_INSIGHTS),
    State#knowledge_graph_state{insights = Bounded}.

apply_entity_recognized(E, #knowledge_graph_state{entities = Entities} = State) ->
    EntityId = gv(entity_id, E),
    Entity = #{
        entity_type => gv(entity_type, E),
        name => gv(name, E),
        description => gv(description, E),
        source_agent => gv(source_agent, E),
        captured_at => gv(captured_at, E)
    },
    State#knowledge_graph_state{entities = Entities#{EntityId => Entity}}.

apply_relationship_drawn(E, #knowledge_graph_state{relationships = Rels} = State) ->
    RelId = gv(rel_id, E),
    Rel = #{
        from_entity => gv(from_entity, E),
        to_entity => gv(to_entity, E),
        rel_type => gv(rel_type, E),
        strength => gv(strength, E, 1.0),
        drawn_at => gv(drawn_at, E)
    },
    State#knowledge_graph_state{relationships = Rels#{RelId => Rel}}.

apply_insight_superseded(E, #knowledge_graph_state{insights = Insights} = State) ->
    InsightId = gv(insight_id, E),
    Updated = lists:map(fun(I) ->
        case maps:get(insight_id, I, undefined) of
            InsightId ->
                I#{superseded => true,
                   superseded_by => gv(superseded_by, E),
                   superseded_at => gv(superseded_at, E)};
            _ -> I
        end
    end, Insights),
    State#knowledge_graph_state{insights = Updated}.

apply_archived(#knowledge_graph_state{status = Status} = State) ->
    State#knowledge_graph_state{
        status = evoq_bit_flags:set(Status, ?KG_ARCHIVED)
    }.

%% --- Internal ---

get_command_type(#{command_type := T}) when is_binary(T) -> T;
get_command_type(#{command_type := T}) when is_atom(T) -> atom_to_binary(T);
get_command_type(_) -> undefined.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.

gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.

convert_events({ok, Events}, ToMapFn) ->
    {ok, [ToMapFn(E) || E <- Events]};
convert_events({error, _} = Err, _) ->
    Err.
