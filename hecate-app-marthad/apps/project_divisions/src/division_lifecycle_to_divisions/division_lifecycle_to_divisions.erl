-module(division_lifecycle_to_divisions).
-behaviour(evoq_projection).

-include_lib("guide_division_lifecycle/include/division_lifecycle_status.hrl").

-export([interested_in/0, init/1, project/4]).
-export([storming_actions/1, planning_actions/1, kanban_actions/1, crafting_actions/1]).

-define(TABLE, project_divisions_divisions).

interested_in() ->
    [<<"division_initiated_v1">>,
     <<"division_archived_v1">>,
     <<"planning_opened_v1">>,
     <<"planning_shelved_v1">>,
     <<"planning_resumed_v1">>,
     <<"planning_submitted_v1">>,
     <<"crafting_opened_v1">>,
     <<"crafting_shelved_v1">>,
     <<"crafting_resumed_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

%% division_initiated_v1: INSERT new division with all phase statuses
do_project(<<"division_initiated_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    StormingS  = ?STORMING_INITIATED bor ?STORMING_ACTIVE,
    PlanningS  = ?PLANNING_INITIATED bor ?PLANNING_OPEN,
    KanbanS    = ?BOARD_INITIATED bor ?BOARD_ACTIVE,
    CraftingS  = ?CRAFTING_INITIATED bor ?CRAFTING_OPEN,
    Division = #{
        division_id    => DivisionId,
        venture_id     => gf(venture_id, Data),
        context_name   => gf(context_name, Data),
        status         => ?DIV_INITIATED,
        status_label   => evoq_bit_flags:to_string(?DIV_INITIATED, ?DIV_FLAG_MAP),
        initiated_at   => gf(initiated_at, Data),
        initiated_by   => gf(initiated_by, Data),
        %% Storming starts active
        storming_status            => StormingS,
        storming_status_label      => evoq_bit_flags:to_string(StormingS, ?STORMING_FLAG_MAP),
        storming_available_actions => storming_actions(StormingS),
        %% Planning starts open
        planning_status            => PlanningS,
        planning_status_label      => evoq_bit_flags:to_string(PlanningS, ?PLANNING_FLAG_MAP),
        planning_available_actions => planning_actions(PlanningS),
        planning_opened_at         => gf(initiated_at, Data),
        %% Kanban starts active
        kanban_status              => KanbanS,
        kanban_status_label        => evoq_bit_flags:to_string(KanbanS, ?BOARD_FLAG_MAP),
        kanban_available_actions   => kanban_actions(KanbanS),
        %% Crafting starts open
        crafting_status            => CraftingS,
        crafting_status_label      => evoq_bit_flags:to_string(CraftingS, ?CRAFTING_FLAG_MAP),
        crafting_available_actions => crafting_actions(CraftingS),
        crafting_opened_at         => gf(initiated_at, Data)
    },
    {ok, RM2} = evoq_read_model:put(DivisionId, Division, RM),
    {ok, State, RM2};

%% division_archived_v1: set DIV_ARCHIVED
do_project(<<"division_archived_v1">>, Data, State, RM) ->
    update_status(gf(division_id, Data), status, status_label, ?DIV_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?DIV_ARCHIVED) end, State, RM);

%% Planning lifecycle
do_project(<<"planning_opened_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    update_field_and_status(DivisionId, planning_status, planning_status_label, ?PLANNING_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?PLANNING_OPEN) end,
        [{planning_opened_at, gf(opened_at, Data)}], State, RM);

do_project(<<"planning_shelved_v1">>, Data, State, RM) ->
    update_status(gf(division_id, Data), planning_status, planning_status_label, ?PLANNING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?PLANNING_OPEN),
            evoq_bit_flags:set(S1, ?PLANNING_SHELVED)
        end, State, RM);

do_project(<<"planning_resumed_v1">>, Data, State, RM) ->
    update_status(gf(division_id, Data), planning_status, planning_status_label, ?PLANNING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?PLANNING_SHELVED),
            evoq_bit_flags:set(S1, ?PLANNING_OPEN)
        end, State, RM);

do_project(<<"planning_submitted_v1">>, Data, State, RM) ->
    update_status(gf(division_id, Data), planning_status, planning_status_label, ?PLANNING_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?PLANNING_SUBMITTED) end, State, RM);

%% Crafting lifecycle
do_project(<<"crafting_opened_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    update_field_and_status(DivisionId, crafting_status, crafting_status_label, ?CRAFTING_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?CRAFTING_OPEN) end,
        [{crafting_opened_at, gf(opened_at, Data)}], State, RM);

do_project(<<"crafting_shelved_v1">>, Data, State, RM) ->
    update_status(gf(division_id, Data), crafting_status, crafting_status_label, ?CRAFTING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?CRAFTING_OPEN),
            evoq_bit_flags:set(S1, ?CRAFTING_SHELVED)
        end, State, RM);

do_project(<<"crafting_resumed_v1">>, Data, State, RM) ->
    update_status(gf(division_id, Data), crafting_status, crafting_status_label, ?CRAFTING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?CRAFTING_SHELVED),
            evoq_bit_flags:set(S1, ?CRAFTING_OPEN)
        end, State, RM);

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

%% Internal

update_status(DivisionId, StatusKey, LabelKey, FlagMap, StatusFun, State, RM) ->
    case evoq_read_model:get(DivisionId, RM) of
        {ok, V} ->
            OldStatus = maps:get(StatusKey, V, 0),
            NewStatus = StatusFun(OldStatus),
            ActionsKey = actions_key(StatusKey),
            ActionsFun = actions_fun(StatusKey),
            Updated = V#{
                StatusKey => NewStatus,
                LabelKey => evoq_bit_flags:to_string(NewStatus, FlagMap),
                ActionsKey => ActionsFun(NewStatus)
            },
            {ok, RM2} = evoq_read_model:put(DivisionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

update_field_and_status(DivisionId, StatusKey, LabelKey, FlagMap, StatusFun, ExtraFields, State, RM) ->
    case evoq_read_model:get(DivisionId, RM) of
        {ok, V} ->
            OldStatus = maps:get(StatusKey, V, 0),
            NewStatus = StatusFun(OldStatus),
            ActionsKey = actions_key(StatusKey),
            ActionsFun = actions_fun(StatusKey),
            V1 = V#{
                StatusKey => NewStatus,
                LabelKey => evoq_bit_flags:to_string(NewStatus, FlagMap),
                ActionsKey => ActionsFun(NewStatus)
            },
            Updated = lists:foldl(fun({K, Val}, Acc) -> Acc#{K => Val} end, V1, ExtraFields),
            {ok, RM2} = evoq_read_model:put(DivisionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

%% Map status key -> available_actions key
actions_key(storming_status)  -> storming_available_actions;
actions_key(planning_status)  -> planning_available_actions;
actions_key(kanban_status)    -> kanban_available_actions;
actions_key(crafting_status)  -> crafting_available_actions;
actions_key(status)           -> available_actions.

%% Map status key -> actions function
actions_fun(storming_status)  -> fun storming_actions/1;
actions_fun(planning_status)  -> fun planning_actions/1;
actions_fun(kanban_status)    -> fun kanban_actions/1;
actions_fun(crafting_status)  -> fun crafting_actions/1;
actions_fun(status)           -> fun(_) -> [] end.

%% --- Per-phase available actions ---

storming_actions(S) when is_integer(S) ->
    first_match(S, [
        {?STORMING_ARCHIVED,  []},
        {?STORMING_ACTIVE,    [<<"shelve">>, <<"archive">>]},
        {?STORMING_INITIATED, [<<"open">>]}
    ]).

planning_actions(S) when is_integer(S) ->
    first_match(S, [
        {?PLANNING_ARCHIVED,  []},
        {?PLANNING_SUBMITTED, []},
        {?PLANNING_SHELVED,   [<<"resume">>, <<"archive">>]},
        {?PLANNING_OPEN,      [<<"shelve">>, <<"conclude">>, <<"archive">>]},
        {?PLANNING_INITIATED, [<<"open">>]}
    ]).

kanban_actions(S) when is_integer(S) ->
    first_match(S, [
        {?BOARD_ARCHIVED,  []},
        {?BOARD_ACTIVE,    [<<"shelve">>, <<"archive">>]},
        {?BOARD_INITIATED, [<<"open">>]}
    ]).

crafting_actions(S) when is_integer(S) ->
    first_match(S, [
        {?CRAFTING_ARCHIVED,  []},
        {?CRAFTING_SHELVED,   [<<"resume">>, <<"archive">>]},
        {?CRAFTING_OPEN,      [<<"shelve">>, <<"archive">>]},
        {?CRAFTING_INITIATED, [<<"open">>]}
    ]).

first_match(_S, []) -> [];
first_match(S, [{Flag, Actions} | Rest]) ->
    case evoq_bit_flags:has(S, Flag) of
        true  -> Actions;
        false -> first_match(S, Rest)
    end.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.
