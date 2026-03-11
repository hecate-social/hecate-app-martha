%%% @doc Division planning aggregate — lifecycle for division planning dossier.
%%%
%%% Stream: division-planning-{division_id}
%%% Store: martha_store
%%%
%%% Lifecycle (circular, not linear):
%%%   1. initiate_planning (birth event, auto-opens, triggered by PM)
%%%   2. open_planning / shelve_planning / resume_planning (pause/resume cycle)
%%%   3. submit_planning (signals crafting, clears on next design/plan)
%%%   4. archive_planning (walking skeleton — only exit state)
%%%
%%% Content desks (design_aggregate, design_event, plan_desk, plan_dependency)
%%% have moved to guide_division_storming.
%%% @end
-module(division_planning_aggregate).

-behaviour(evoq_aggregate).

-include("planning_status.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([flag_map/0]).

-record(division_planning_state, {
    division_id    :: binary() | undefined,
    venture_id     :: binary() | undefined,
    context_name   :: binary() | undefined,
    status = 0     :: non_neg_integer(),
    initiated_at   :: non_neg_integer() | undefined,
    initiated_by   :: binary() | undefined,
    opened_at      :: non_neg_integer() | undefined,
    shelved_at     :: non_neg_integer() | undefined,
    shelve_reason  :: binary() | undefined
}).

-type state() :: #division_planning_state{}.
-export_type([state/0]).

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?PLANNING_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(_AggregateId) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #division_planning_state{}.

%% --- Execute ---
%% NOTE: evoq calls execute(State, Payload) - State FIRST!

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh aggregate — only initiate allowed
execute(#division_planning_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"initiate_planning">> -> execute_initiate_planning(Payload);
        _ -> {error, planning_not_initiated}
    end;

%% Archived — nothing allowed
execute(#division_planning_state{status = S}, _Payload) when S band ?PLANNING_ARCHIVED =/= 0 ->
    {error, planning_archived};

%% Initiated and not archived — route by command type
execute(#division_planning_state{status = S} = State, Payload) when S band ?PLANNING_INITIATED =/= 0 ->
    case get_command_type(Payload) of
        <<"archive_planning">>  -> execute_archive_planning(Payload);
        <<"open_planning">>     -> execute_open_planning(Payload, State);
        <<"shelve_planning">>   -> execute_shelve_planning(Payload, State);
        <<"resume_planning">>   -> execute_resume_planning(Payload, State);
        <<"submit_planning">>   -> execute_submit_planning(Payload, State);
        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Command handlers ---

execute_initiate_planning(Payload) ->
    {ok, Cmd} = initiate_planning_v1:from_map(Payload),
    convert_events(maybe_initiate_planning:handle(Cmd), fun planning_initiated_v1:to_map/1).

execute_archive_planning(Payload) ->
    {ok, Cmd} = archive_planning_v1:from_map(Payload),
    convert_events(maybe_archive_planning:handle(Cmd), fun planning_archived_v1:to_map/1).

execute_open_planning(Payload, #division_planning_state{status = S}) ->
    case S band ?PLANNING_OPEN of
        0 ->
            {ok, Cmd} = open_planning_v1:from_map(Payload),
            convert_events(maybe_open_planning:handle(Cmd), fun planning_opened_v1:to_map/1);
        _ ->
            {error, planning_already_open}
    end.

execute_shelve_planning(Payload, #division_planning_state{status = S}) ->
    case S band ?PLANNING_OPEN of
        0 -> {error, planning_not_open};
        _ ->
            {ok, Cmd} = shelve_planning_v1:from_map(Payload),
            convert_events(maybe_shelve_planning:handle(Cmd), fun planning_shelved_v1:to_map/1)
    end.

execute_resume_planning(Payload, #division_planning_state{status = S}) ->
    case S band ?PLANNING_SHELVED of
        0 -> {error, planning_not_shelved};
        _ ->
            {ok, Cmd} = resume_planning_v1:from_map(Payload),
            convert_events(maybe_resume_planning:handle(Cmd), fun planning_resumed_v1:to_map/1)
    end.

execute_submit_planning(Payload, #division_planning_state{status = S, venture_id = VId, context_name = CName}) ->
    case S band ?PLANNING_OPEN of
        0 -> {error, planning_not_open};
        _ ->
            {ok, Cmd} = submit_planning_v1:from_map(Payload),
            Context = #{venture_id => VId, context_name => CName},
            convert_events(maybe_submit_planning:handle(Cmd, Context), fun planning_submitted_v1:to_map/1)
    end.

%% --- Apply ---
%% NOTE: evoq calls apply(State, Event) - State FIRST!

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    apply_event(Event, State).

-spec apply_event(map(), state()) -> state().

%% Planning lifecycle events
apply_event(#{<<"event_type">> := <<"planning_initiated_v1">>} = E, S) -> apply_initiated(E, S);
apply_event(#{event_type := <<"planning_initiated_v1">>} = E, S)      -> apply_initiated(E, S);
apply_event(#{<<"event_type">> := <<"planning_archived_v1">>} = _E, S) -> apply_archived(S);
apply_event(#{event_type := <<"planning_archived_v1">>} = _E, S)       -> apply_archived(S);
apply_event(#{<<"event_type">> := <<"planning_opened_v1">>} = E, S)   -> apply_opened(E, S);
apply_event(#{event_type := <<"planning_opened_v1">>} = E, S)         -> apply_opened(E, S);
apply_event(#{<<"event_type">> := <<"planning_shelved_v1">>} = E, S)  -> apply_shelved(E, S);
apply_event(#{event_type := <<"planning_shelved_v1">>} = E, S)        -> apply_shelved(E, S);
apply_event(#{<<"event_type">> := <<"planning_resumed_v1">>} = _E, S) -> apply_resumed(S);
apply_event(#{event_type := <<"planning_resumed_v1">>} = _E, S)       -> apply_resumed(S);
apply_event(#{<<"event_type">> := <<"planning_submitted_v1">>} = E, S) -> apply_submitted(E, S);
apply_event(#{event_type := <<"planning_submitted_v1">>} = E, S)       -> apply_submitted(E, S);

%% Unknown — ignore
apply_event(_E, S) -> S.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#division_planning_state{
        division_id = get_value(division_id, E),
        venture_id = get_value(venture_id, E),
        context_name = get_value(context_name, E),
        status = evoq_bit_flags:set(
            evoq_bit_flags:set(0, ?PLANNING_INITIATED),
            ?PLANNING_OPEN),
        initiated_at = get_value(initiated_at, E),
        initiated_by = get_value(initiated_by, E),
        opened_at = get_value(initiated_at, E)
    }.

apply_archived(#division_planning_state{status = Status} = State) ->
    State#division_planning_state{status = evoq_bit_flags:set(Status, ?PLANNING_ARCHIVED)}.

apply_opened(E, #division_planning_state{status = Status} = State) ->
    State#division_planning_state{
        status = evoq_bit_flags:set(Status, ?PLANNING_OPEN),
        opened_at = get_value(opened_at, E)
    }.

apply_shelved(E, #division_planning_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?PLANNING_OPEN),
    S1 = evoq_bit_flags:set(S0, ?PLANNING_SHELVED),
    State#division_planning_state{
        status = S1,
        shelved_at = get_value(shelved_at, E),
        shelve_reason = get_value(reason, E)
    }.

apply_resumed(#division_planning_state{status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?PLANNING_SHELVED),
    S1 = evoq_bit_flags:set(S0, ?PLANNING_OPEN),
    State#division_planning_state{
        status = S1,
        shelved_at = undefined,
        shelve_reason = undefined
    }.

apply_submitted(_E, #division_planning_state{status = Status} = State) ->
    State#division_planning_state{
        status = evoq_bit_flags:set(Status, ?PLANNING_SUBMITTED)
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

convert_events({ok, Events}, ToMapFn) ->
    {ok, [ToMapFn(E) || E <- Events]};
convert_events({error, _} = Err, _) ->
    Err.
