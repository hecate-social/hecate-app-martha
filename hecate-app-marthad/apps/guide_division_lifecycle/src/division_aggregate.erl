%%% @doc Unified division aggregate — one stream per division, all phases.
%%%
%%% Stream: division-{division_id}
%%% Store: martha_store
%%%
%%% Consolidates storming, planning, kanban, and crafting into one aggregate.
%%% Birth event: division_initiated_v1 (sets all 4 phases to initial state)
%%% Exit event: division_archived_v1 (sets DIV_ARCHIVED flag)
%%% @end
-module(division_aggregate).

-behaviour(evoq_aggregate).

-include("division_lifecycle_status.hrl").
-include("division_state.hrl").
-include("kanban_card_status.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([division_flag_map/0, storming_flag_map/0, planning_flag_map/0,
         board_flag_map/0, crafting_flag_map/0, card_flag_map/0]).

-type state() :: #division_state{}.
-export_type([state/0]).

division_flag_map() -> ?DIV_FLAG_MAP.
storming_flag_map() -> ?STORMING_FLAG_MAP.
planning_flag_map() -> ?PLANNING_FLAG_MAP.
board_flag_map() -> ?BOARD_FLAG_MAP.
crafting_flag_map() -> ?CRAFTING_FLAG_MAP.
card_flag_map() -> ?CARD_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(_AggregateId) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #division_state{}.

%% --- Execute ---
%% NOTE: evoq calls execute(State, Payload) - State FIRST!

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh aggregate — only initiate allowed
execute(#division_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"initiate_division">> -> execute_initiate_division(Payload);
        _ -> {error, division_not_initiated}
    end;

%% Archived — nothing allowed
execute(#division_state{status = S}, _Payload) when S band ?DIV_ARCHIVED =/= 0 ->
    {error, division_archived};

%% Initiated and not archived — route by command type
execute(#division_state{status = S} = State, Payload) when S band ?DIV_INITIATED =/= 0 ->
    case get_command_type(Payload) of
        %% Division-level lifecycle
        <<"archive_division">>  -> execute_archive_division(Payload);

        %% === STORMING COMMANDS ===
        <<"design_aggregate">>  -> require_storming_active(State, fun() -> execute_design_aggregate(Payload, State) end);
        <<"design_event">>      -> require_storming_active(State, fun() -> execute_design_event(Payload, State) end);
        <<"plan_desk">>         -> require_storming_active(State, fun() -> execute_plan_desk(Payload, State) end);
        <<"plan_dependency">>   -> require_storming_active(State, fun() -> execute_plan_dependency(Payload, State) end);

        %% === PLANNING COMMANDS ===
        <<"open_planning">>     -> execute_open_planning(Payload, State);
        <<"shelve_planning">>   -> execute_shelve_planning(Payload, State);
        <<"resume_planning">>   -> execute_resume_planning(Payload, State);
        <<"submit_planning">>   -> execute_submit_planning(Payload, State);

        %% === KANBAN COMMANDS ===
        <<"post_kanban_card">>      -> require_kanban_active(State, fun() -> execute_post_card(Payload, State) end);
        <<"pick_kanban_card">>      -> require_kanban_active(State, fun() -> execute_pick_card(Payload, State) end);
        <<"finish_kanban_card">>    -> require_kanban_active(State, fun() -> execute_finish_card(Payload, State) end);
        <<"unpick_kanban_card">>    -> require_kanban_active(State, fun() -> execute_unpick_card(Payload, State) end);
        <<"park_kanban_card">>      -> require_kanban_active(State, fun() -> execute_park_card(Payload, State) end);
        <<"unpark_kanban_card">>    -> require_kanban_active(State, fun() -> execute_unpark_card(Payload, State) end);
        <<"block_kanban_card">>     -> require_kanban_active(State, fun() -> execute_block_card(Payload, State) end);
        <<"unblock_kanban_card">>   -> require_kanban_active(State, fun() -> execute_unblock_card(Payload, State) end);

        %% === CRAFTING COMMANDS ===
        <<"open_crafting">>     -> execute_open_crafting(Payload, State);
        <<"shelve_crafting">>   -> execute_shelve_crafting(Payload, State);
        <<"resume_crafting">>   -> execute_resume_crafting(Payload, State);
        <<"generate_module">>     -> require_crafting_open(State, fun() -> execute_generate_module(Payload) end);
        <<"generate_test">>       -> require_crafting_open(State, fun() -> execute_generate_test(Payload) end);
        <<"run_test_suite">>      -> require_crafting_open(State, fun() -> execute_run_test_suite(Payload) end);
        <<"record_test_result">>  -> require_crafting_open(State, fun() -> execute_record_test_result(Payload) end);
        <<"deliver_release">>     -> require_crafting_open(State, fun() -> execute_deliver_release(Payload) end);
        <<"stage_delivery">>      -> require_crafting_open(State, fun() -> execute_stage_delivery(Payload) end);

        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Division lifecycle commands ---

execute_initiate_division(Payload) ->
    {ok, Cmd} = initiate_division_v1:from_map(Payload),
    convert_events(maybe_initiate_division:handle(Cmd), fun division_initiated_v1:to_map/1).

execute_archive_division(Payload) ->
    {ok, Cmd} = archive_division_v1:from_map(Payload),
    convert_events(maybe_archive_division:handle(Cmd), fun division_archived_v1:to_map/1).

%% --- Storming commands ---

execute_design_aggregate(Payload, #division_state{designed_aggregates = Aggs}) ->
    {ok, Cmd} = design_aggregate_v1:from_map(Payload),
    Context = #{designed_aggregates => Aggs},
    convert_events(maybe_design_aggregate:handle(Cmd, Context), fun aggregate_designed_v1:to_map/1).

execute_design_event(Payload, #division_state{designed_events = Evts}) ->
    {ok, Cmd} = design_event_v1:from_map(Payload),
    Context = #{designed_events => Evts},
    convert_events(maybe_design_event:handle(Cmd, Context), fun event_designed_v1:to_map/1).

execute_plan_desk(Payload, #division_state{planned_desks = Desks}) ->
    {ok, Cmd} = plan_desk_v1:from_map(Payload),
    Context = #{planned_desks => Desks},
    convert_events(maybe_plan_desk:handle(Cmd, Context), fun desk_planned_v1:to_map/1).

execute_plan_dependency(Payload, #division_state{planned_dependencies = Deps}) ->
    {ok, Cmd} = plan_dependency_v1:from_map(Payload),
    Context = #{planned_dependencies => Deps},
    convert_events(maybe_plan_dependency:handle(Cmd, Context), fun dependency_planned_v1:to_map/1).

%% --- Planning commands ---

execute_open_planning(Payload, #division_state{planning_status = S}) ->
    case S band ?PLANNING_OPEN of
        0 ->
            {ok, Cmd} = open_planning_v1:from_map(Payload),
            convert_events(maybe_open_planning:handle(Cmd), fun planning_opened_v1:to_map/1);
        _ ->
            {error, planning_already_open}
    end.

execute_shelve_planning(Payload, #division_state{planning_status = S}) ->
    case S band ?PLANNING_OPEN of
        0 -> {error, planning_not_open};
        _ ->
            {ok, Cmd} = shelve_planning_v1:from_map(Payload),
            convert_events(maybe_shelve_planning:handle(Cmd), fun planning_shelved_v1:to_map/1)
    end.

execute_resume_planning(Payload, #division_state{planning_status = S}) ->
    case S band ?PLANNING_SHELVED of
        0 -> {error, planning_not_shelved};
        _ ->
            {ok, Cmd} = resume_planning_v1:from_map(Payload),
            convert_events(maybe_resume_planning:handle(Cmd), fun planning_resumed_v1:to_map/1)
    end.

execute_submit_planning(Payload, #division_state{planning_status = S, venture_id = VId, context_name = CName}) ->
    case S band ?PLANNING_OPEN of
        0 -> {error, planning_not_open};
        _ ->
            {ok, Cmd} = submit_planning_v1:from_map(Payload),
            Context = #{venture_id => VId, context_name => CName},
            convert_events(maybe_submit_planning:handle(Cmd, Context), fun planning_submitted_v1:to_map/1)
    end.

%% --- Kanban commands ---

execute_post_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = post_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_post_kanban_card:handle(Cmd, Context), fun kanban_card_posted_v1:to_map/1).

execute_pick_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = pick_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_pick_kanban_card:handle(Cmd, Context), fun kanban_card_picked_v1:to_map/1).

execute_finish_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = finish_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_finish_kanban_card:handle(Cmd, Context), fun kanban_card_finished_v1:to_map/1).

execute_unpick_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = unpick_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_unpick_kanban_card:handle(Cmd, Context), fun kanban_card_unpicked_v1:to_map/1).

execute_park_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = park_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_park_kanban_card:handle(Cmd, Context), fun kanban_card_parked_v1:to_map/1).

execute_unpark_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = unpark_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_unpark_kanban_card:handle(Cmd, Context), fun kanban_card_unparked_v1:to_map/1).

execute_block_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = block_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_block_kanban_card:handle(Cmd, Context), fun kanban_card_blocked_v1:to_map/1).

execute_unblock_card(Payload, #division_state{cards = Cards}) ->
    {ok, Cmd} = unblock_kanban_card_v1:from_map(Payload),
    Context = #{cards => Cards},
    convert_events(maybe_unblock_kanban_card:handle(Cmd, Context), fun kanban_card_unblocked_v1:to_map/1).

%% --- Crafting commands ---

execute_open_crafting(Payload, #division_state{crafting_status = S}) ->
    case S band ?CRAFTING_OPEN of
        0 ->
            {ok, Cmd} = open_crafting_v1:from_map(Payload),
            convert_events(maybe_open_crafting:handle(Cmd), fun crafting_opened_v1:to_map/1);
        _ ->
            {error, crafting_already_open}
    end.

execute_shelve_crafting(Payload, #division_state{crafting_status = S}) ->
    case S band ?CRAFTING_OPEN of
        0 -> {error, crafting_not_open};
        _ ->
            {ok, Cmd} = shelve_crafting_v1:from_map(Payload),
            convert_events(maybe_shelve_crafting:handle(Cmd), fun crafting_shelved_v1:to_map/1)
    end.

execute_resume_crafting(Payload, #division_state{crafting_status = S}) ->
    case S band ?CRAFTING_SHELVED of
        0 -> {error, crafting_not_shelved};
        _ ->
            {ok, Cmd} = resume_crafting_v1:from_map(Payload),
            convert_events(maybe_resume_crafting:handle(Cmd), fun crafting_resumed_v1:to_map/1)
    end.

execute_generate_module(Payload) ->
    {ok, Cmd} = generate_module_v1:from_map(Payload),
    convert_events(maybe_generate_module:handle(Cmd), fun module_generated_v1:to_map/1).

execute_generate_test(Payload) ->
    {ok, Cmd} = generate_test_v1:from_map(Payload),
    convert_events(maybe_generate_test:handle(Cmd), fun test_generated_v1:to_map/1).

execute_run_test_suite(Payload) ->
    {ok, Cmd} = run_test_suite_v1:from_map(Payload),
    convert_events(maybe_run_test_suite:handle(Cmd), fun test_suite_run_v1:to_map/1).

execute_record_test_result(Payload) ->
    {ok, Cmd} = record_test_result_v1:from_map(Payload),
    convert_events(maybe_record_test_result:handle(Cmd), fun test_result_recorded_v1:to_map/1).

execute_deliver_release(Payload) ->
    {ok, Cmd} = deliver_release_v1:from_map(Payload),
    convert_events(maybe_deliver_release:handle(Cmd), fun release_delivered_v1:to_map/1).

execute_stage_delivery(Payload) ->
    {ok, Cmd} = stage_delivery_v1:from_map(Payload),
    convert_events(maybe_stage_delivery:handle(Cmd), fun delivery_staged_v1:to_map/1).

%% --- Guards ---

require_storming_active(#division_state{storming_status = S}, Fun) ->
    case S band ?STORMING_ACTIVE of
        0 -> {error, storming_not_active};
        _ -> Fun()
    end.

require_kanban_active(#division_state{kanban_status = S}, Fun) ->
    case S band ?BOARD_ACTIVE of
        0 -> {error, board_not_active};
        _ -> Fun()
    end.

require_crafting_open(#division_state{crafting_status = S}, Fun) ->
    case S band ?CRAFTING_OPEN of
        0 -> {error, crafting_not_open};
        _ -> Fun()
    end.

%% --- Apply ---
%% NOTE: evoq calls apply(State, Event) - State FIRST!

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    apply_event(Event, State).

-spec apply_event(map(), state()) -> state().

%% Division lifecycle
apply_event(#{<<"event_type">> := <<"division_initiated_v1">>} = E, S) -> apply_division_initiated(E, S);
apply_event(#{event_type := <<"division_initiated_v1">>} = E, S)      -> apply_division_initiated(E, S);
apply_event(#{<<"event_type">> := <<"division_archived_v1">>}, S)      -> apply_division_archived(S);
apply_event(#{event_type := <<"division_archived_v1">>}, S)            -> apply_division_archived(S);

%% Storming events (kept as-is from old aggregate)
apply_event(#{<<"event_type">> := <<"aggregate_designed_v1">>} = E, S) -> apply_aggregate_designed(E, S);
apply_event(#{event_type := <<"aggregate_designed_v1">>} = E, S)       -> apply_aggregate_designed(E, S);
apply_event(#{<<"event_type">> := <<"event_designed_v1">>} = E, S)     -> apply_event_designed(E, S);
apply_event(#{event_type := <<"event_designed_v1">>} = E, S)           -> apply_event_designed(E, S);
apply_event(#{<<"event_type">> := <<"desk_planned_v1">>} = E, S)      -> apply_desk_planned(E, S);
apply_event(#{event_type := <<"desk_planned_v1">>} = E, S)            -> apply_desk_planned(E, S);
apply_event(#{<<"event_type">> := <<"dependency_planned_v1">>} = E, S) -> apply_dependency_planned(E, S);
apply_event(#{event_type := <<"dependency_planned_v1">>} = E, S)       -> apply_dependency_planned(E, S);

%% Planning events
apply_event(#{<<"event_type">> := <<"planning_opened_v1">>} = E, S)   -> apply_planning_opened(E, S);
apply_event(#{event_type := <<"planning_opened_v1">>} = E, S)         -> apply_planning_opened(E, S);
apply_event(#{<<"event_type">> := <<"planning_shelved_v1">>} = E, S)  -> apply_planning_shelved(E, S);
apply_event(#{event_type := <<"planning_shelved_v1">>} = E, S)        -> apply_planning_shelved(E, S);
apply_event(#{<<"event_type">> := <<"planning_resumed_v1">>}, S)      -> apply_planning_resumed(S);
apply_event(#{event_type := <<"planning_resumed_v1">>}, S)            -> apply_planning_resumed(S);
apply_event(#{<<"event_type">> := <<"planning_submitted_v1">>}, S)    -> apply_planning_submitted(S);
apply_event(#{event_type := <<"planning_submitted_v1">>}, S)          -> apply_planning_submitted(S);

%% Kanban events
apply_event(#{<<"event_type">> := <<"kanban_card_posted_v1">>} = E, S)     -> apply_card_posted(E, S);
apply_event(#{event_type := <<"kanban_card_posted_v1">>} = E, S)           -> apply_card_posted(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_picked_v1">>} = E, S)     -> apply_card_picked(E, S);
apply_event(#{event_type := <<"kanban_card_picked_v1">>} = E, S)           -> apply_card_picked(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_finished_v1">>} = E, S)   -> apply_card_finished(E, S);
apply_event(#{event_type := <<"kanban_card_finished_v1">>} = E, S)         -> apply_card_finished(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_unpicked_v1">>} = E, S)   -> apply_card_unpicked(E, S);
apply_event(#{event_type := <<"kanban_card_unpicked_v1">>} = E, S)         -> apply_card_unpicked(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_parked_v1">>} = E, S)     -> apply_card_parked(E, S);
apply_event(#{event_type := <<"kanban_card_parked_v1">>} = E, S)           -> apply_card_parked(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_unparked_v1">>} = E, S)   -> apply_card_unparked(E, S);
apply_event(#{event_type := <<"kanban_card_unparked_v1">>} = E, S)         -> apply_card_unparked(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_blocked_v1">>} = E, S)    -> apply_card_blocked(E, S);
apply_event(#{event_type := <<"kanban_card_blocked_v1">>} = E, S)          -> apply_card_blocked(E, S);
apply_event(#{<<"event_type">> := <<"kanban_card_unblocked_v1">>} = E, S)  -> apply_card_unblocked(E, S);
apply_event(#{event_type := <<"kanban_card_unblocked_v1">>} = E, S)        -> apply_card_unblocked(E, S);

%% Crafting lifecycle events
apply_event(#{<<"event_type">> := <<"crafting_opened_v1">>} = E, S)   -> apply_crafting_opened(E, S);
apply_event(#{event_type := <<"crafting_opened_v1">>} = E, S)         -> apply_crafting_opened(E, S);
apply_event(#{<<"event_type">> := <<"crafting_shelved_v1">>} = E, S)  -> apply_crafting_shelved(E, S);
apply_event(#{event_type := <<"crafting_shelved_v1">>} = E, S)        -> apply_crafting_shelved(E, S);
apply_event(#{<<"event_type">> := <<"crafting_resumed_v1">>}, S)      -> apply_crafting_resumed(S);
apply_event(#{event_type := <<"crafting_resumed_v1">>}, S)            -> apply_crafting_resumed(S);

%% Crafting domain events
apply_event(#{<<"event_type">> := <<"module_generated_v1">>} = E, S)      -> apply_module_generated(E, S);
apply_event(#{event_type := <<"module_generated_v1">>} = E, S)            -> apply_module_generated(E, S);
apply_event(#{<<"event_type">> := <<"test_generated_v1">>} = E, S)        -> apply_test_generated(E, S);
apply_event(#{event_type := <<"test_generated_v1">>} = E, S)              -> apply_test_generated(E, S);
apply_event(#{<<"event_type">> := <<"test_suite_run_v1">>} = E, S)        -> apply_test_suite_run(E, S);
apply_event(#{event_type := <<"test_suite_run_v1">>} = E, S)              -> apply_test_suite_run(E, S);
apply_event(#{<<"event_type">> := <<"test_result_recorded_v1">>} = E, S)  -> apply_test_result_recorded(E, S);
apply_event(#{event_type := <<"test_result_recorded_v1">>} = E, S)        -> apply_test_result_recorded(E, S);
apply_event(#{<<"event_type">> := <<"release_delivered_v1">>} = E, S)     -> apply_release_delivered(E, S);
apply_event(#{event_type := <<"release_delivered_v1">>} = E, S)           -> apply_release_delivered(E, S);
apply_event(#{<<"event_type">> := <<"delivery_staged_v1">>} = E, S)       -> apply_delivery_staged(E, S);
apply_event(#{event_type := <<"delivery_staged_v1">>} = E, S)             -> apply_delivery_staged(E, S);

%% Unknown — ignore
apply_event(_E, S) -> S.

%% --- Division lifecycle apply helpers ---

apply_division_initiated(E, State) ->
    State#division_state{
        division_id = gv(division_id, E),
        venture_id = gv(venture_id, E),
        context_name = gv(context_name, E),
        status = ?DIV_INITIATED,
        initiated_at = gv(initiated_at, E),
        initiated_by = gv(initiated_by, E),
        %% All phases start in their initial active state
        storming_status = ?STORMING_INITIATED bor ?STORMING_ACTIVE,
        planning_status = ?PLANNING_INITIATED bor ?PLANNING_OPEN,
        planning_opened_at = gv(initiated_at, E),
        kanban_status = ?BOARD_INITIATED bor ?BOARD_ACTIVE,
        crafting_status = ?CRAFTING_INITIATED bor ?CRAFTING_OPEN,
        crafting_opened_at = gv(initiated_at, E)
    }.

apply_division_archived(#division_state{status = Status} = State) ->
    State#division_state{status = evoq_bit_flags:set(Status, ?DIV_ARCHIVED)}.

%% --- Storming apply helpers ---

apply_aggregate_designed(E, #division_state{designed_aggregates = Aggs} = State) ->
    AggName = gv(aggregate_name, E),
    AggData = #{
        description => gv(description, E),
        stream_prefix => gv(stream_prefix, E),
        fields => gv(fields, E, []),
        designed_at => gv(designed_at, E)
    },
    State#division_state{designed_aggregates = Aggs#{AggName => AggData}}.

apply_event_designed(E, #division_state{designed_events = Evts} = State) ->
    EvtName = gv(event_name, E),
    EvtData = #{
        description => gv(description, E),
        aggregate_name => gv(aggregate_name, E),
        fields => gv(fields, E, []),
        designed_at => gv(designed_at, E)
    },
    State#division_state{designed_events = Evts#{EvtName => EvtData}}.

apply_desk_planned(E, #division_state{planned_desks = Desks} = State) ->
    DeskName = gv(desk_name, E),
    DeskData = #{
        department => gv(department, E),
        description => gv(description, E),
        commands => gv(commands, E, []),
        planned_at => gv(planned_at, E)
    },
    State#division_state{planned_desks = Desks#{DeskName => DeskData}}.

apply_dependency_planned(E, #division_state{planned_dependencies = Deps} = State) ->
    DepId = gv(dependency_id, E),
    DepData = #{
        from_desk => gv(from_desk, E),
        to_desk => gv(to_desk, E),
        dep_type => gv(dep_type, E),
        planned_at => gv(planned_at, E)
    },
    State#division_state{planned_dependencies = Deps#{DepId => DepData}}.

%% --- Planning apply helpers ---

apply_planning_opened(E, #division_state{planning_status = Status} = State) ->
    State#division_state{
        planning_status = evoq_bit_flags:set(Status, ?PLANNING_OPEN),
        planning_opened_at = gv(opened_at, E)
    }.

apply_planning_shelved(E, #division_state{planning_status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?PLANNING_OPEN),
    S1 = evoq_bit_flags:set(S0, ?PLANNING_SHELVED),
    State#division_state{
        planning_status = S1,
        planning_shelved_at = gv(shelved_at, E),
        planning_shelve_reason = gv(reason, E)
    }.

apply_planning_resumed(#division_state{planning_status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?PLANNING_SHELVED),
    S1 = evoq_bit_flags:set(S0, ?PLANNING_OPEN),
    State#division_state{
        planning_status = S1,
        planning_shelved_at = undefined,
        planning_shelve_reason = undefined
    }.

apply_planning_submitted(#division_state{planning_status = Status} = State) ->
    State#division_state{
        planning_status = evoq_bit_flags:set(Status, ?PLANNING_SUBMITTED)
    }.

%% --- Kanban apply helpers ---

apply_card_posted(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    Entry = #{
        status => ?CARD_POSTED,
        title => gv(title, E),
        description => gv(description, E),
        card_type => gv(card_type, E),
        posted_by => gv(posted_by, E),
        posted_at => gv(posted_at, E)
    },
    State#division_state{cards = Cards#{CardId => Entry}}.

apply_card_picked(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{
                status => ?CARD_PICKED,
                picked_by => gv(picked_by, E),
                picked_at => gv(picked_at, E)
            },
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_finished(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_FINISHED, finished_at => gv(finished_at, E)},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_unpicked(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_POSTED, picked_by => undefined, picked_at => undefined},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_parked(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{
                status => ?CARD_PARKED,
                parked_by => gv(parked_by, E),
                parked_at => gv(parked_at, E),
                park_reason => gv(park_reason, E)
            },
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_unparked(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_POSTED, parked_by => undefined, parked_at => undefined, park_reason => undefined},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_blocked(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{
                status => ?CARD_BLOCKED,
                blocked_by => gv(blocked_by, E),
                blocked_at => gv(blocked_at, E),
                block_reason => gv(block_reason, E)
            },
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_unblocked(E, #division_state{cards = Cards} = State) ->
    CardId = gv(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_POSTED, blocked_by => undefined, blocked_at => undefined, block_reason => undefined},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

%% --- Crafting apply helpers ---

apply_crafting_opened(E, #division_state{crafting_status = Status} = State) ->
    State#division_state{
        crafting_status = evoq_bit_flags:set(Status, ?CRAFTING_OPEN),
        crafting_opened_at = gv(opened_at, E)
    }.

apply_crafting_shelved(E, #division_state{crafting_status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?CRAFTING_OPEN),
    S1 = evoq_bit_flags:set(S0, ?CRAFTING_SHELVED),
    State#division_state{
        crafting_status = S1,
        crafting_shelved_at = gv(shelved_at, E),
        crafting_shelve_reason = gv(reason, E, undefined)
    }.

apply_crafting_resumed(#division_state{crafting_status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?CRAFTING_SHELVED),
    S1 = evoq_bit_flags:set(S0, ?CRAFTING_OPEN),
    State#division_state{
        crafting_status = S1,
        crafting_shelved_at = undefined,
        crafting_shelve_reason = undefined
    }.

apply_module_generated(E, #division_state{generated_modules = Modules} = State) ->
    ModuleName = gv(module_name, E),
    Entry = #{module_type => gv(module_type, E), path => gv(path, E), generated_at => gv(generated_at, E)},
    State#division_state{generated_modules = Modules#{ModuleName => Entry}}.

apply_test_generated(E, #division_state{generated_tests = Tests} = State) ->
    TestName = gv(test_name, E),
    Entry = #{module_name => gv(module_name, E), path => gv(path, E), generated_at => gv(generated_at, E)},
    State#division_state{generated_tests = Tests#{TestName => Entry}}.

apply_test_suite_run(E, #division_state{test_suites = Suites} = State) ->
    SuiteId = gv(suite_id, E),
    Entry = #{suite_name => gv(suite_name, E), run_at => gv(run_at, E)},
    State#division_state{test_suites = Suites#{SuiteId => Entry}}.

apply_test_result_recorded(E, #division_state{test_results = Results} = State) ->
    ResultId = gv(result_id, E),
    Entry = #{suite_id => gv(suite_id, E), passed => gv(passed, E), failed => gv(failed, E), recorded_at => gv(recorded_at, E)},
    State#division_state{test_results = Results#{ResultId => Entry}}.

apply_release_delivered(E, #division_state{releases = Releases} = State) ->
    ReleaseId = gv(release_id, E),
    Entry = #{version => gv(version, E), delivered_at => gv(delivered_at, E)},
    State#division_state{releases = Releases#{ReleaseId => Entry}}.

apply_delivery_staged(E, #division_state{delivery_stages = Stages} = State) ->
    StageId = gv(stage_id, E),
    Entry = #{release_id => gv(release_id, E), stage_name => gv(stage_name, E), staged_at => gv(staged_at, E)},
    State#division_state{delivery_stages = Stages#{StageId => Entry}}.

%% --- Internal ---

get_command_type(#{<<"command_type">> := T}) -> T;
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
