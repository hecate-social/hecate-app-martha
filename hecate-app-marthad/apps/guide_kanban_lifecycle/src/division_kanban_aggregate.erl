%%% @doc Division kanban aggregate — coordination buffer between agents.
%%%
%%% Stream: division-kanban-{division_id}
%%% Store: martha_store
%%%
%%% Each division has its own kanban board. Planning agents submit items,
%%% crafting agents pick them. The board is the visible handoff point.
%%%
%%% Lifecycle:
%%%   1. initiate_kanban (birth event, auto-activates, triggered by PM)
%%%   2. submit_kanban_item / pick_kanban_item / complete_kanban_item / return_kanban_item
%%%   3. archive_kanban (walking skeleton — only exit state)
%%% @end
-module(division_kanban_aggregate).

-behaviour(evoq_aggregate).

-include("kanban_status.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([flag_map/0]).

-record(division_kanban_state, {
    division_id    :: binary() | undefined,
    venture_id     :: binary() | undefined,
    context_name   :: binary() | undefined,
    status = 0     :: non_neg_integer(),
    initiated_at   :: non_neg_integer() | undefined,
    initiated_by   :: binary() | undefined,
    items = #{}    :: map()  %% item_id => #{status, title, ...}
}).

-type state() :: #division_kanban_state{}.
-export_type([state/0]).

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?KANBAN_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(_AggregateId) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #division_kanban_state{}.

%% --- Execute ---
%% NOTE: evoq calls execute(State, Payload) - State FIRST!

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh aggregate — only initiate allowed
execute(#division_kanban_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"initiate_kanban">> -> execute_initiate_kanban(Payload);
        _ -> {error, kanban_not_initiated}
    end;

%% Archived — nothing allowed
execute(#division_kanban_state{status = S}, _Payload) when S band ?KANBAN_ARCHIVED =/= 0 ->
    {error, kanban_archived};

%% Initiated and not archived — route by command type
execute(#division_kanban_state{status = S} = State, Payload) when S band ?KANBAN_INITIATED =/= 0 ->
    case get_command_type(Payload) of
        <<"archive_kanban">>        -> execute_archive_kanban(Payload);
        <<"submit_kanban_item">>    -> require_active(S, fun() -> execute_submit_kanban_item(Payload, State) end);
        <<"pick_kanban_item">>      -> require_active(S, fun() -> execute_pick_kanban_item(Payload, State) end);
        <<"complete_kanban_item">>  -> require_active(S, fun() -> execute_complete_kanban_item(Payload, State) end);
        <<"return_kanban_item">>    -> require_active(S, fun() -> execute_return_kanban_item(Payload, State) end);
        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Command handlers ---

execute_initiate_kanban(Payload) ->
    {ok, Cmd} = initiate_kanban_v1:from_map(Payload),
    convert_events(maybe_initiate_kanban:handle(Cmd), fun kanban_initiated_v1:to_map/1).

execute_archive_kanban(Payload) ->
    {ok, Cmd} = archive_kanban_v1:from_map(Payload),
    convert_events(maybe_archive_kanban:handle(Cmd), fun kanban_archived_v1:to_map/1).

execute_submit_kanban_item(Payload, #division_kanban_state{items = Items}) ->
    {ok, Cmd} = submit_kanban_item_v1:from_map(Payload),
    Context = #{items => Items},
    convert_events(maybe_submit_kanban_item:handle(Cmd, Context), fun kanban_item_submitted_v1:to_map/1).

execute_pick_kanban_item(Payload, #division_kanban_state{items = Items}) ->
    {ok, Cmd} = pick_kanban_item_v1:from_map(Payload),
    Context = #{items => Items},
    convert_events(maybe_pick_kanban_item:handle(Cmd, Context), fun kanban_item_picked_v1:to_map/1).

execute_complete_kanban_item(Payload, #division_kanban_state{items = Items}) ->
    {ok, Cmd} = complete_kanban_item_v1:from_map(Payload),
    Context = #{items => Items},
    convert_events(maybe_complete_kanban_item:handle(Cmd, Context), fun kanban_item_completed_v1:to_map/1).

execute_return_kanban_item(Payload, #division_kanban_state{items = Items}) ->
    {ok, Cmd} = return_kanban_item_v1:from_map(Payload),
    Context = #{items => Items},
    convert_events(maybe_return_kanban_item:handle(Cmd, Context), fun kanban_item_returned_v1:to_map/1).

require_active(S, Fun) ->
    case S band ?KANBAN_ACTIVE of
        0 -> {error, kanban_not_active};
        _ -> Fun()
    end.

%% --- Apply ---
%% NOTE: evoq calls apply(State, Event) - State FIRST!

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    apply_event(Event, State).

-spec apply_event(map(), state()) -> state().

%% Lifecycle events
apply_event(#{<<"event_type">> := <<"kanban_initiated_v1">>} = E, S) -> apply_initiated(E, S);
apply_event(#{event_type := <<"kanban_initiated_v1">>} = E, S)      -> apply_initiated(E, S);
apply_event(#{<<"event_type">> := <<"kanban_archived_v1">>} = _E, S) -> apply_archived(S);
apply_event(#{event_type := <<"kanban_archived_v1">>} = _E, S)       -> apply_archived(S);

%% Item events
apply_event(#{<<"event_type">> := <<"kanban_item_submitted_v1">>} = E, S) -> apply_item_submitted(E, S);
apply_event(#{event_type := <<"kanban_item_submitted_v1">>} = E, S)       -> apply_item_submitted(E, S);
apply_event(#{<<"event_type">> := <<"kanban_item_picked_v1">>} = E, S)    -> apply_item_picked(E, S);
apply_event(#{event_type := <<"kanban_item_picked_v1">>} = E, S)          -> apply_item_picked(E, S);
apply_event(#{<<"event_type">> := <<"kanban_item_completed_v1">>} = E, S) -> apply_item_completed(E, S);
apply_event(#{event_type := <<"kanban_item_completed_v1">>} = E, S)       -> apply_item_completed(E, S);
apply_event(#{<<"event_type">> := <<"kanban_item_returned_v1">>} = E, S)  -> apply_item_returned(E, S);
apply_event(#{event_type := <<"kanban_item_returned_v1">>} = E, S)        -> apply_item_returned(E, S);

%% Unknown — ignore
apply_event(_E, S) -> S.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#division_kanban_state{
        division_id = get_value(division_id, E),
        venture_id = get_value(venture_id, E),
        context_name = get_value(context_name, E),
        status = evoq_bit_flags:set(
            evoq_bit_flags:set(0, ?KANBAN_INITIATED),
            ?KANBAN_ACTIVE),
        initiated_at = get_value(initiated_at, E),
        initiated_by = get_value(initiated_by, E)
    }.

apply_archived(#division_kanban_state{status = Status} = State) ->
    State#division_kanban_state{status = evoq_bit_flags:set(Status, ?KANBAN_ARCHIVED)}.

apply_item_submitted(E, #division_kanban_state{items = Items} = State) ->
    ItemId = get_value(item_id, E),
    Entry = #{
        status => ready,
        title => get_value(title, E),
        description => get_value(description, E),
        item_type => get_value(item_type, E),
        submitted_by => get_value(submitted_by, E),
        submitted_at => get_value(submitted_at, E)
    },
    State#division_kanban_state{items = Items#{ItemId => Entry}}.

apply_item_picked(E, #division_kanban_state{items = Items} = State) ->
    ItemId = get_value(item_id, E),
    case maps:find(ItemId, Items) of
        {ok, Item} ->
            Updated = Item#{
                status => in_progress,
                picked_by => get_value(picked_by, E),
                picked_at => get_value(picked_at, E)
            },
            State#division_kanban_state{items = Items#{ItemId => Updated}};
        error ->
            State
    end.

apply_item_completed(E, #division_kanban_state{items = Items} = State) ->
    ItemId = get_value(item_id, E),
    case maps:find(ItemId, Items) of
        {ok, Item} ->
            Updated = Item#{
                status => done,
                completed_at => get_value(completed_at, E)
            },
            State#division_kanban_state{items = Items#{ItemId => Updated}};
        error ->
            State
    end.

apply_item_returned(E, #division_kanban_state{items = Items} = State) ->
    ItemId = get_value(item_id, E),
    case maps:find(ItemId, Items) of
        {ok, Item} ->
            Updated = Item#{
                status => ready,
                picked_by => undefined,
                picked_at => undefined,
                return_reason => get_value(reason, E, undefined)
            },
            State#division_kanban_state{items = Items#{ItemId => Updated}};
        error ->
            State
    end.

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
