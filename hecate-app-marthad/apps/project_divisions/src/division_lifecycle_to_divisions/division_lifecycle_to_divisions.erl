-module(division_lifecycle_to_divisions).
-behaviour(evoq_projection).

-include_lib("guide_division_lifecycle/include/division_lifecycle_status.hrl").

-export([interested_in/0, init/1, project/4]).

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
    Division = #{
        division_id    => DivisionId,
        venture_id     => gf(venture_id, Data),
        context_name   => gf(context_name, Data),
        status         => ?DIV_INITIATED,
        status_label   => evoq_bit_flags:to_string(?DIV_INITIATED, ?DIV_FLAG_MAP),
        initiated_at   => gf(initiated_at, Data),
        initiated_by   => gf(initiated_by, Data),
        %% Storming starts active
        storming_status       => ?STORMING_INITIATED bor ?STORMING_ACTIVE,
        storming_status_label => evoq_bit_flags:to_string(?STORMING_INITIATED bor ?STORMING_ACTIVE, ?STORMING_FLAG_MAP),
        %% Planning starts open
        planning_status       => ?PLANNING_INITIATED bor ?PLANNING_OPEN,
        planning_status_label => evoq_bit_flags:to_string(?PLANNING_INITIATED bor ?PLANNING_OPEN, ?PLANNING_FLAG_MAP),
        planning_opened_at    => gf(initiated_at, Data),
        %% Kanban starts active
        kanban_status         => ?BOARD_INITIATED bor ?BOARD_ACTIVE,
        kanban_status_label   => evoq_bit_flags:to_string(?BOARD_INITIATED bor ?BOARD_ACTIVE, ?BOARD_FLAG_MAP),
        %% Crafting starts open
        crafting_status       => ?CRAFTING_INITIATED bor ?CRAFTING_OPEN,
        crafting_status_label => evoq_bit_flags:to_string(?CRAFTING_INITIATED bor ?CRAFTING_OPEN, ?CRAFTING_FLAG_MAP),
        crafting_opened_at    => gf(initiated_at, Data)
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
            Updated = V#{
                StatusKey => NewStatus,
                LabelKey => evoq_bit_flags:to_string(NewStatus, FlagMap)
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
            V1 = V#{
                StatusKey => NewStatus,
                LabelKey => evoq_bit_flags:to_string(NewStatus, FlagMap)
            },
            Updated = lists:foldl(fun({K, Val}, Acc) -> Acc#{K => Val} end, V1, ExtraFields),
            {ok, RM2} = evoq_read_model:put(DivisionId, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(#{<<"event_type">> := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.
