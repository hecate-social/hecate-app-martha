%%% @doc Division storming aggregate — EventStorming content for a division.
%%%
%%% Stream: division-storming-{division_id}
%%% Store: martha_store
%%%
%%% Lifecycle:
%%%   1. initiate_storming (birth event, auto-activates, triggered by PM)
%%%   2. design_aggregate / design_event / plan_desk / plan_dependency (requires ACTIVE)
%%%   3. archive_storming (walking skeleton — only exit state)
%%% @end
-module(division_storming_aggregate).

-behaviour(evoq_aggregate).

-include("storming_status.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([flag_map/0]).

-record(division_storming_state, {
    division_id    :: binary() | undefined,
    venture_id     :: binary() | undefined,
    context_name   :: binary() | undefined,
    status = 0     :: non_neg_integer(),
    initiated_at   :: non_neg_integer() | undefined,
    initiated_by   :: binary() | undefined,
    designed_aggregates = #{} :: map(),
    designed_events = #{} :: map(),
    planned_desks = #{} :: map(),
    planned_dependencies = #{} :: map()
}).

-type state() :: #division_storming_state{}.
-export_type([state/0]).

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?STORMING_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(_AggregateId) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #division_storming_state{}.

%% --- Execute ---
%% NOTE: evoq calls execute(State, Payload) - State FIRST!

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh aggregate — only initiate allowed
execute(#division_storming_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"initiate_storming">> -> execute_initiate_storming(Payload);
        _ -> {error, storming_not_initiated}
    end;

%% Archived — nothing allowed
execute(#division_storming_state{status = S}, _Payload) when S band ?STORMING_ARCHIVED =/= 0 ->
    {error, storming_archived};

%% Initiated and not archived — route by command type
execute(#division_storming_state{status = S} = State, Payload) when S band ?STORMING_INITIATED =/= 0 ->
    case get_command_type(Payload) of
        <<"archive_storming">>  -> execute_archive_storming(Payload);
        <<"design_aggregate">>  -> require_active(S, fun() -> execute_design_aggregate(Payload, State) end);
        <<"design_event">>      -> require_active(S, fun() -> execute_design_event(Payload, State) end);
        <<"plan_desk">>         -> require_active(S, fun() -> execute_plan_desk(Payload, State) end);
        <<"plan_dependency">>   -> require_active(S, fun() -> execute_plan_dependency(Payload, State) end);
        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Command handlers ---

execute_initiate_storming(Payload) ->
    {ok, Cmd} = initiate_storming_v1:from_map(Payload),
    convert_events(maybe_initiate_storming:handle(Cmd), fun storming_initiated_v1:to_map/1).

execute_archive_storming(Payload) ->
    {ok, Cmd} = archive_storming_v1:from_map(Payload),
    convert_events(maybe_archive_storming:handle(Cmd), fun storming_archived_v1:to_map/1).

execute_design_aggregate(Payload, #division_storming_state{designed_aggregates = Aggs}) ->
    {ok, Cmd} = design_aggregate_v1:from_map(Payload),
    Context = #{designed_aggregates => Aggs},
    convert_events(maybe_design_aggregate:handle(Cmd, Context), fun aggregate_designed_v1:to_map/1).

execute_design_event(Payload, #division_storming_state{designed_events = Evts}) ->
    {ok, Cmd} = design_event_v1:from_map(Payload),
    Context = #{designed_events => Evts},
    convert_events(maybe_design_event:handle(Cmd, Context), fun event_designed_v1:to_map/1).

execute_plan_desk(Payload, #division_storming_state{planned_desks = Desks}) ->
    {ok, Cmd} = plan_desk_v1:from_map(Payload),
    Context = #{planned_desks => Desks},
    convert_events(maybe_plan_desk:handle(Cmd, Context), fun desk_planned_v1:to_map/1).

execute_plan_dependency(Payload, #division_storming_state{planned_dependencies = Deps}) ->
    {ok, Cmd} = plan_dependency_v1:from_map(Payload),
    Context = #{planned_dependencies => Deps},
    convert_events(maybe_plan_dependency:handle(Cmd, Context), fun dependency_planned_v1:to_map/1).

require_active(S, Fun) ->
    case S band ?STORMING_ACTIVE of
        0 -> {error, storming_not_active};
        _ -> Fun()
    end.

%% --- Apply ---
%% NOTE: evoq calls apply(State, Event) - State FIRST!

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    apply_event(Event, State).

-spec apply_event(map(), state()) -> state().

%% Storming lifecycle events
apply_event(#{<<"event_type">> := <<"storming_initiated_v1">>} = E, S) -> apply_initiated(E, S);
apply_event(#{event_type := <<"storming_initiated_v1">>} = E, S)      -> apply_initiated(E, S);
apply_event(#{<<"event_type">> := <<"storming_archived_v1">>} = _E, S) -> apply_archived(S);
apply_event(#{event_type := <<"storming_archived_v1">>} = _E, S)       -> apply_archived(S);

%% Design + plan events
apply_event(#{<<"event_type">> := <<"aggregate_designed_v1">>} = E, S) -> apply_aggregate_designed(E, S);
apply_event(#{event_type := <<"aggregate_designed_v1">>} = E, S)       -> apply_aggregate_designed(E, S);
apply_event(#{<<"event_type">> := <<"event_designed_v1">>} = E, S)     -> apply_event_designed(E, S);
apply_event(#{event_type := <<"event_designed_v1">>} = E, S)           -> apply_event_designed(E, S);
apply_event(#{<<"event_type">> := <<"desk_planned_v1">>} = E, S)      -> apply_desk_planned(E, S);
apply_event(#{event_type := <<"desk_planned_v1">>} = E, S)            -> apply_desk_planned(E, S);
apply_event(#{<<"event_type">> := <<"dependency_planned_v1">>} = E, S) -> apply_dependency_planned(E, S);
apply_event(#{event_type := <<"dependency_planned_v1">>} = E, S)       -> apply_dependency_planned(E, S);

%% Unknown — ignore
apply_event(_E, S) -> S.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#division_storming_state{
        division_id = get_value(division_id, E),
        venture_id = get_value(venture_id, E),
        context_name = get_value(context_name, E),
        status = evoq_bit_flags:set(
            evoq_bit_flags:set(0, ?STORMING_INITIATED),
            ?STORMING_ACTIVE),
        initiated_at = get_value(initiated_at, E),
        initiated_by = get_value(initiated_by, E)
    }.

apply_archived(#division_storming_state{status = Status} = State) ->
    State#division_storming_state{status = evoq_bit_flags:set(Status, ?STORMING_ARCHIVED)}.

apply_aggregate_designed(E, #division_storming_state{designed_aggregates = Aggs} = State) ->
    AggName = get_value(aggregate_name, E),
    AggData = #{
        description => get_value(description, E),
        stream_prefix => get_value(stream_prefix, E),
        fields => get_value(fields, E, []),
        designed_at => get_value(designed_at, E)
    },
    State#division_storming_state{
        designed_aggregates = Aggs#{AggName => AggData}
    }.

apply_event_designed(E, #division_storming_state{designed_events = Evts} = State) ->
    EvtName = get_value(event_name, E),
    EvtData = #{
        description => get_value(description, E),
        aggregate_name => get_value(aggregate_name, E),
        fields => get_value(fields, E, []),
        designed_at => get_value(designed_at, E)
    },
    State#division_storming_state{
        designed_events = Evts#{EvtName => EvtData}
    }.

apply_desk_planned(E, #division_storming_state{planned_desks = Desks} = State) ->
    DeskName = get_value(desk_name, E),
    DeskData = #{
        department => get_value(department, E),
        description => get_value(description, E),
        commands => get_value(commands, E, []),
        planned_at => get_value(planned_at, E)
    },
    State#division_storming_state{
        planned_desks = Desks#{DeskName => DeskData}
    }.

apply_dependency_planned(E, #division_storming_state{planned_dependencies = Deps} = State) ->
    DepId = get_value(dependency_id, E),
    DepData = #{
        from_desk => get_value(from_desk, E),
        to_desk => get_value(to_desk, E),
        dep_type => get_value(dep_type, E),
        planned_at => get_value(planned_at, E)
    },
    State#division_storming_state{
        planned_dependencies = Deps#{DepId => DepData}
    }.

%% --- Internal ---

get_command_type(#{<<"command_type">> := T}) -> T;
get_command_type(#{command_type := T}) when is_binary(T) -> T;
get_command_type(#{command_type := T}) when is_atom(T) -> atom_to_binary(T);
get_command_type(_) -> undefined.

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

convert_events({ok, Events}, ToMapFn) ->
    {ok, [ToMapFn(E) || E <- Events]};
convert_events({error, _} = Err, _) ->
    Err.
