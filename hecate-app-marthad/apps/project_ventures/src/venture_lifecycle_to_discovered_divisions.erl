%%% @doc Merged projection: division lifecycle events -> discovered_divisions ETS.
%%%
%%% Maintains division records with phase status fields enriched from
%%% the unified division lifecycle events (division_initiated_v1 sets
%%% all 4 phases at once).
%%% @end
-module(venture_lifecycle_to_discovered_divisions).
-behaviour(evoq_projection).

-include_lib("guide_division_lifecycle/include/division_lifecycle_status.hrl").

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_divisions).

interested_in() ->
    [%% Division identification (from venture aggregate)
     <<"division_identified_v1">>,
     %% Division lifecycle (from division aggregate)
     <<"division_initiated_v1">>,
     <<"division_archived_v1">>,
     %% Planning phase transitions
     <<"planning_opened_v1">>,
     <<"planning_shelved_v1">>,
     <<"planning_resumed_v1">>,
     <<"planning_submitted_v1">>,
     %% Crafting phase transitions
     <<"crafting_opened_v1">>,
     <<"crafting_shelved_v1">>,
     <<"crafting_resumed_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

%% --- division_identified_v1: INSERT new division with empty phase statuses ---

do_project(<<"division_identified_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    Division = #{
        division_id    => DivisionId,
        venture_id     => gf(venture_id, Data),
        context_name   => gf(context_name, Data),
        description    => gf(description, Data),
        identified_by  => gf(identified_by, Data),
        discovered_at  => gf(identified_at, Data),
        %% Phase statuses (enriched by lifecycle events)
        storming_status       => 0,
        storming_status_label => <<"">>,
        planning_status       => 0,
        planning_status_label => <<"">>,
        kanban_status         => 0,
        kanban_status_label   => <<"">>,
        crafting_status       => 0,
        crafting_status_label => <<"">>,
        %% Available actions per phase
        storming_available_actions => [],
        planning_available_actions => [],
        kanban_available_actions   => [],
        crafting_available_actions => []
    },
    {ok, RM2} = evoq_read_model:put(DivisionId, Division, RM),
    {ok, State, RM2};

%% --- division_initiated_v1: Sets all 4 phases to initial active state ---

do_project(<<"division_initiated_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    case evoq_read_model:get(DivisionId, RM) of
        {ok, Div} ->
            StormingS = ?STORMING_INITIATED bor ?STORMING_ACTIVE,
            PlanningS = ?PLANNING_INITIATED bor ?PLANNING_OPEN,
            KanbanS = ?BOARD_INITIATED bor ?BOARD_ACTIVE,
            CraftingS = ?CRAFTING_INITIATED bor ?CRAFTING_OPEN,
            Updated = Div#{
                storming_status => StormingS,
                storming_status_label => evoq_bit_flags:to_string(StormingS, ?STORMING_FLAG_MAP),
                storming_available_actions => available_actions(StormingS, storming),
                planning_status => PlanningS,
                planning_status_label => evoq_bit_flags:to_string(PlanningS, ?PLANNING_FLAG_MAP),
                planning_available_actions => available_actions(PlanningS, planning),
                kanban_status => KanbanS,
                kanban_status_label => evoq_bit_flags:to_string(KanbanS, ?BOARD_FLAG_MAP),
                kanban_available_actions => available_actions(KanbanS, kanban),
                crafting_status => CraftingS,
                crafting_status_label => evoq_bit_flags:to_string(CraftingS, ?CRAFTING_FLAG_MAP),
                crafting_available_actions => available_actions(CraftingS, crafting)
            },
            {ok, RM2} = evoq_read_model:put(DivisionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

%% --- division_archived_v1: Mark all phases as archived ---

do_project(<<"division_archived_v1">>, Data, State, RM) ->
    DivisionId = gf(division_id, Data),
    case evoq_read_model:get(DivisionId, RM) of
        {ok, Div} ->
            Updated = Div#{
                storming_available_actions => [],
                planning_available_actions => [],
                kanban_available_actions => [],
                crafting_available_actions => []
            },
            {ok, RM2} = evoq_read_model:put(DivisionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end;

%% ===================================================================
%% Planning lifecycle
%% ===================================================================

do_project(<<"planning_opened_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        planning_status, planning_status_label, planning_available_actions, planning,
        ?PLANNING_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?PLANNING_OPEN) end,
        State, RM);

do_project(<<"planning_shelved_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        planning_status, planning_status_label, planning_available_actions, planning,
        ?PLANNING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?PLANNING_OPEN),
            evoq_bit_flags:set(S1, ?PLANNING_SHELVED)
        end,
        State, RM);

do_project(<<"planning_resumed_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        planning_status, planning_status_label, planning_available_actions, planning,
        ?PLANNING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?PLANNING_SHELVED),
            evoq_bit_flags:set(S1, ?PLANNING_OPEN)
        end,
        State, RM);

do_project(<<"planning_submitted_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        planning_status, planning_status_label, planning_available_actions, planning,
        ?PLANNING_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?PLANNING_SUBMITTED) end,
        State, RM);

%% ===================================================================
%% Crafting lifecycle
%% ===================================================================

do_project(<<"crafting_opened_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        crafting_status, crafting_status_label, crafting_available_actions, crafting,
        ?CRAFTING_FLAG_MAP,
        fun(S) -> evoq_bit_flags:set(S, ?CRAFTING_OPEN) end,
        State, RM);

do_project(<<"crafting_shelved_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        crafting_status, crafting_status_label, crafting_available_actions, crafting,
        ?CRAFTING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?CRAFTING_OPEN),
            evoq_bit_flags:set(S1, ?CRAFTING_SHELVED)
        end,
        State, RM);

do_project(<<"crafting_resumed_v1">>, Data, State, RM) ->
    update_phase_status(gf(division_id, Data),
        crafting_status, crafting_status_label, crafting_available_actions, crafting,
        ?CRAFTING_FLAG_MAP,
        fun(S) ->
            S1 = evoq_bit_flags:unset(S, ?CRAFTING_SHELVED),
            evoq_bit_flags:set(S1, ?CRAFTING_OPEN)
        end,
        State, RM);

%% --- Unknown ---

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

%%====================================================================
%% Internal
%%====================================================================

update_phase_status(DivisionId, StatusKey, LabelKey, ActionsKey, Phase, FlagMap, StatusFun, State, RM) ->
    case evoq_read_model:get(DivisionId, RM) of
        {ok, Div} ->
            OldStatus = maps:get(StatusKey, Div, 0),
            NewStatus = StatusFun(OldStatus),
            NewLabel = evoq_bit_flags:to_string(NewStatus, FlagMap),
            NewActions = available_actions(NewStatus, Phase),
            Updated = Div#{
                StatusKey => NewStatus,
                LabelKey => NewLabel,
                ActionsKey => NewActions
            },
            {ok, RM2} = evoq_read_model:put(DivisionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

%% @doc Compute available actions for a phase given its current bit-flag status.
%% Uses guard-style pattern: check disqualifiers first, then match active state.
available_actions(Status, storming) ->
    Has = fun(Flag) -> evoq_bit_flags:has(Status, Flag) end,
    storming_actions(Has(?STORMING_ARCHIVED), Has(?STORMING_ACTIVE));
available_actions(Status, planning) ->
    Has = fun(Flag) -> evoq_bit_flags:has(Status, Flag) end,
    planning_actions(Has(?PLANNING_ARCHIVED), Has(?PLANNING_OPEN),
                     Has(?PLANNING_SHELVED), Has(?PLANNING_INITIATED));
available_actions(Status, kanban) ->
    Has = fun(Flag) -> evoq_bit_flags:has(Status, Flag) end,
    kanban_actions(Has(?BOARD_ARCHIVED), Has(?BOARD_ACTIVE));
available_actions(Status, crafting) ->
    Has = fun(Flag) -> evoq_bit_flags:has(Status, Flag) end,
    crafting_actions(Has(?CRAFTING_ARCHIVED), Has(?CRAFTING_OPEN),
                     Has(?CRAFTING_SHELVED), Has(?CRAFTING_INITIATED)).

storming_actions(true, _Active)  -> [];
storming_actions(false, true)    -> [<<"archive">>];
storming_actions(false, false)   -> [].

planning_actions(true, _, _, _)          -> [];
planning_actions(_, true, _, _)          -> [<<"shelve">>, <<"conclude">>];
planning_actions(_, _, true, _)          -> [<<"resume">>];
planning_actions(_, _, _, true)          -> [<<"open">>];
planning_actions(_, _, _, _)             -> [].

kanban_actions(true, _Active) -> [];
kanban_actions(false, true)   -> [<<"archive">>];
kanban_actions(false, false)  -> [].

crafting_actions(true, _, _, _)          -> [];
crafting_actions(_, true, _, _)          -> [<<"shelve">>, <<"conclude">>];
crafting_actions(_, _, true, _)          -> [<<"resume">>];
crafting_actions(_, _, _, true)          -> [<<"open">>];
crafting_actions(_, _, _, _)             -> [].

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(#{<<"event_type">> := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.
