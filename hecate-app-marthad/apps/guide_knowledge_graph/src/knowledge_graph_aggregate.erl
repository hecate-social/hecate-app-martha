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
-export([state_module/0, flag_map/0]).

-type state() :: #knowledge_graph_state{}.
-export_type([state/0]).

-spec state_module() -> module().
state_module() -> knowledge_graph_state.

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?KG_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(AggregateId) ->
    {ok, knowledge_graph_state:new(AggregateId)}.

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
    knowledge_graph_state:apply_event(State, Event).

%% --- Internal ---

get_command_type(#{command_type := T}) when is_binary(T) -> T;
get_command_type(#{command_type := T}) when is_atom(T) -> atom_to_binary(T);
get_command_type(_) -> undefined.

convert_events({ok, Events}, ToMapFn) ->
    {ok, [ToMapFn(E) || E <- Events]};
convert_events({error, _} = Err, _) ->
    Err.
