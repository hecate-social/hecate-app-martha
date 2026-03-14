%%% @doc Division state module — implements evoq_state behaviour.
%%%
%%% Owns the division_state record, initial state creation, event folding,
%%% and serialization. Extracted from division_aggregate to separate
%%% state concerns from command validation.
%%% @end
-module(division_state).

-behaviour(evoq_state).

-include("division_lifecycle_status.hrl").
-include("division_state.hrl").
-include("kanban_card_status.hrl").

-export([new/1, apply_event/2, to_map/1]).

-type state() :: #division_state{}.
-export_type([state/0]).

%% --- evoq_state callbacks ---

-spec new(binary()) -> state().
new(_AggregateId) ->
    #division_state{}.

-spec apply_event(state(), map()) -> state().

%% Normalize atom event_type to binary (from typed evoq_event modules)
apply_event(S, #{event_type := Type} = E) when is_atom(Type) ->
    apply_event(S, E#{event_type := atom_to_binary(Type, utf8)});

%% Division lifecycle
apply_event(S, #{event_type := <<"division_initiated_v1">>} = E)      -> apply_division_initiated(E, S);
apply_event(S, #{event_type := <<"division_archived_v1">>} = _E)      -> apply_division_archived(S);

%% Storming events
apply_event(S, #{event_type := <<"aggregate_designed_v1">>} = E)       -> apply_aggregate_designed(E, S);
apply_event(S, #{event_type := <<"event_designed_v1">>} = E)           -> apply_event_designed(E, S);
apply_event(S, #{event_type := <<"desk_planned_v1">>} = E)            -> apply_desk_planned(E, S);
apply_event(S, #{event_type := <<"dependency_planned_v1">>} = E)       -> apply_dependency_planned(E, S);

%% Planning events
apply_event(S, #{event_type := <<"planning_opened_v1">>} = E)         -> apply_planning_opened(E, S);
apply_event(S, #{event_type := <<"planning_shelved_v1">>} = E)        -> apply_planning_shelved(E, S);
apply_event(S, #{event_type := <<"planning_resumed_v1">>} = _E)       -> apply_planning_resumed(S);
apply_event(S, #{event_type := <<"planning_submitted_v1">>} = _E)     -> apply_planning_submitted(S);

%% Kanban events
apply_event(S, #{event_type := <<"kanban_card_posted_v1">>} = E)           -> apply_card_posted(E, S);
apply_event(S, #{event_type := <<"kanban_card_picked_v1">>} = E)           -> apply_card_picked(E, S);
apply_event(S, #{event_type := <<"kanban_card_finished_v1">>} = E)         -> apply_card_finished(E, S);
apply_event(S, #{event_type := <<"kanban_card_unpicked_v1">>} = E)         -> apply_card_unpicked(E, S);
apply_event(S, #{event_type := <<"kanban_card_parked_v1">>} = E)           -> apply_card_parked(E, S);
apply_event(S, #{event_type := <<"kanban_card_unparked_v1">>} = E)         -> apply_card_unparked(E, S);
apply_event(S, #{event_type := <<"kanban_card_blocked_v1">>} = E)          -> apply_card_blocked(E, S);
apply_event(S, #{event_type := <<"kanban_card_unblocked_v1">>} = E)        -> apply_card_unblocked(E, S);

%% Crafting lifecycle events
apply_event(S, #{event_type := <<"crafting_opened_v1">>} = E)         -> apply_crafting_opened(E, S);
apply_event(S, #{event_type := <<"crafting_shelved_v1">>} = E)        -> apply_crafting_shelved(E, S);
apply_event(S, #{event_type := <<"crafting_resumed_v1">>} = _E)       -> apply_crafting_resumed(S);

%% Crafting domain events
apply_event(S, #{event_type := <<"module_generated_v1">>} = E)            -> apply_module_generated(E, S);
apply_event(S, #{event_type := <<"test_generated_v1">>} = E)              -> apply_test_generated(E, S);
apply_event(S, #{event_type := <<"test_suite_run_v1">>} = E)              -> apply_test_suite_run(E, S);
apply_event(S, #{event_type := <<"test_result_recorded_v1">>} = E)        -> apply_test_result_recorded(E, S);
apply_event(S, #{event_type := <<"release_delivered_v1">>} = E)           -> apply_release_delivered(E, S);
apply_event(S, #{event_type := <<"delivery_staged_v1">>} = E)             -> apply_delivery_staged(E, S);

%% Unknown — ignore
apply_event(S, _E) -> S.

%% --- to_map ---

-spec to_map(state()) -> map().
to_map(#division_state{} = S) ->
    #{
        division_id => S#division_state.division_id,
        venture_id => S#division_state.venture_id,
        context_name => S#division_state.context_name,
        status => S#division_state.status,
        initiated_at => S#division_state.initiated_at,
        initiated_by => S#division_state.initiated_by,
        storming_status => S#division_state.storming_status,
        designed_aggregates => S#division_state.designed_aggregates,
        designed_events => S#division_state.designed_events,
        planned_desks => S#division_state.planned_desks,
        planned_dependencies => S#division_state.planned_dependencies,
        planning_status => S#division_state.planning_status,
        planning_opened_at => S#division_state.planning_opened_at,
        planning_shelved_at => S#division_state.planning_shelved_at,
        planning_shelve_reason => S#division_state.planning_shelve_reason,
        kanban_status => S#division_state.kanban_status,
        cards => S#division_state.cards,
        crafting_status => S#division_state.crafting_status,
        crafting_opened_at => S#division_state.crafting_opened_at,
        crafting_shelved_at => S#division_state.crafting_shelved_at,
        crafting_shelve_reason => S#division_state.crafting_shelve_reason,
        generated_modules => S#division_state.generated_modules,
        generated_tests => S#division_state.generated_tests,
        test_suites => S#division_state.test_suites,
        test_results => S#division_state.test_results,
        releases => S#division_state.releases,
        delivery_stages => S#division_state.delivery_stages
    }.

%% --- Division lifecycle apply helpers ---

apply_division_initiated(E, State) ->
    State#division_state{
        division_id = get_value(division_id, E),
        venture_id = get_value(venture_id, E),
        context_name = get_value(context_name, E),
        status = ?DIV_INITIATED,
        initiated_at = get_value(initiated_at, E),
        initiated_by = get_value(initiated_by, E),
        %% All phases start in their initial active state
        storming_status = ?STORMING_INITIATED bor ?STORMING_ACTIVE,
        planning_status = ?PLANNING_INITIATED bor ?PLANNING_OPEN,
        planning_opened_at = get_value(initiated_at, E),
        kanban_status = ?BOARD_INITIATED bor ?BOARD_ACTIVE,
        crafting_status = ?CRAFTING_INITIATED bor ?CRAFTING_OPEN,
        crafting_opened_at = get_value(initiated_at, E)
    }.

apply_division_archived(#division_state{status = Status} = State) ->
    State#division_state{status = evoq_bit_flags:set(Status, ?DIV_ARCHIVED)}.

%% --- Storming apply helpers ---

apply_aggregate_designed(E, #division_state{designed_aggregates = Aggs} = State) ->
    AggName = get_value(aggregate_name, E),
    AggData = #{
        description => get_value(description, E),
        stream_prefix => get_value(stream_prefix, E),
        fields => get_value(fields, E, []),
        designed_at => get_value(designed_at, E)
    },
    State#division_state{designed_aggregates = Aggs#{AggName => AggData}}.

apply_event_designed(E, #division_state{designed_events = Evts} = State) ->
    EvtName = get_value(event_name, E),
    EvtData = #{
        description => get_value(description, E),
        aggregate_name => get_value(aggregate_name, E),
        fields => get_value(fields, E, []),
        designed_at => get_value(designed_at, E)
    },
    State#division_state{designed_events = Evts#{EvtName => EvtData}}.

apply_desk_planned(E, #division_state{planned_desks = Desks} = State) ->
    DeskName = get_value(desk_name, E),
    DeskData = #{
        department => get_value(department, E),
        description => get_value(description, E),
        commands => get_value(commands, E, []),
        planned_at => get_value(planned_at, E)
    },
    State#division_state{planned_desks = Desks#{DeskName => DeskData}}.

apply_dependency_planned(E, #division_state{planned_dependencies = Deps} = State) ->
    DepId = get_value(dependency_id, E),
    DepData = #{
        from_desk => get_value(from_desk, E),
        to_desk => get_value(to_desk, E),
        dep_type => get_value(dep_type, E),
        planned_at => get_value(planned_at, E)
    },
    State#division_state{planned_dependencies = Deps#{DepId => DepData}}.

%% --- Planning apply helpers ---

apply_planning_opened(E, #division_state{planning_status = Status} = State) ->
    State#division_state{
        planning_status = evoq_bit_flags:set(Status, ?PLANNING_OPEN),
        planning_opened_at = get_value(opened_at, E)
    }.

apply_planning_shelved(E, #division_state{planning_status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?PLANNING_OPEN),
    S1 = evoq_bit_flags:set(S0, ?PLANNING_SHELVED),
    State#division_state{
        planning_status = S1,
        planning_shelved_at = get_value(shelved_at, E),
        planning_shelve_reason = get_value(reason, E)
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
    CardId = get_value(card_id, E),
    Entry = #{
        status => ?CARD_POSTED,
        title => get_value(title, E),
        description => get_value(description, E),
        card_type => get_value(card_type, E),
        posted_by => get_value(posted_by, E),
        posted_at => get_value(posted_at, E)
    },
    State#division_state{cards = Cards#{CardId => Entry}}.

apply_card_picked(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{
                status => ?CARD_PICKED,
                picked_by => get_value(picked_by, E),
                picked_at => get_value(picked_at, E)
            },
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_finished(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_FINISHED, finished_at => get_value(finished_at, E)},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_unpicked(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_POSTED, picked_by => undefined, picked_at => undefined},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_parked(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{
                status => ?CARD_PARKED,
                parked_by => get_value(parked_by, E),
                parked_at => get_value(parked_at, E),
                park_reason => get_value(park_reason, E)
            },
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_unparked(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{status => ?CARD_POSTED, parked_by => undefined, parked_at => undefined, park_reason => undefined},
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_blocked(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
    case maps:find(CardId, Cards) of
        {ok, Card} ->
            Updated = Card#{
                status => ?CARD_BLOCKED,
                blocked_by => get_value(blocked_by, E),
                blocked_at => get_value(blocked_at, E),
                block_reason => get_value(block_reason, E)
            },
            State#division_state{cards = Cards#{CardId => Updated}};
        error -> State
    end.

apply_card_unblocked(E, #division_state{cards = Cards} = State) ->
    CardId = get_value(card_id, E),
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
        crafting_opened_at = get_value(opened_at, E)
    }.

apply_crafting_shelved(E, #division_state{crafting_status = Status} = State) ->
    S0 = evoq_bit_flags:unset(Status, ?CRAFTING_OPEN),
    S1 = evoq_bit_flags:set(S0, ?CRAFTING_SHELVED),
    State#division_state{
        crafting_status = S1,
        crafting_shelved_at = get_value(shelved_at, E),
        crafting_shelve_reason = get_value(reason, E, undefined)
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
    ModuleName = get_value(module_name, E),
    Entry = #{module_type => get_value(module_type, E), path => get_value(path, E), generated_at => get_value(generated_at, E)},
    State#division_state{generated_modules = Modules#{ModuleName => Entry}}.

apply_test_generated(E, #division_state{generated_tests = Tests} = State) ->
    TestName = get_value(test_name, E),
    Entry = #{module_name => get_value(module_name, E), path => get_value(path, E), generated_at => get_value(generated_at, E)},
    State#division_state{generated_tests = Tests#{TestName => Entry}}.

apply_test_suite_run(E, #division_state{test_suites = Suites} = State) ->
    SuiteId = get_value(suite_id, E),
    Entry = #{suite_name => get_value(suite_name, E), run_at => get_value(run_at, E)},
    State#division_state{test_suites = Suites#{SuiteId => Entry}}.

apply_test_result_recorded(E, #division_state{test_results = Results} = State) ->
    ResultId = get_value(result_id, E),
    Entry = #{suite_id => get_value(suite_id, E), passed => get_value(passed, E), failed => get_value(failed, E), recorded_at => get_value(recorded_at, E)},
    State#division_state{test_results = Results#{ResultId => Entry}}.

apply_release_delivered(E, #division_state{releases = Releases} = State) ->
    ReleaseId = get_value(release_id, E),
    Entry = #{version => get_value(version, E), delivered_at => get_value(delivered_at, E)},
    State#division_state{releases = Releases#{ReleaseId => Entry}}.

apply_delivery_staged(E, #division_state{delivery_stages = Stages} = State) ->
    StageId = get_value(stage_id, E),
    Entry = #{release_id => get_value(release_id, E), stage_name => get_value(stage_name, E), staged_at => get_value(staged_at, E)},
    State#division_state{delivery_stages = Stages#{StageId => Entry}}.

%% --- Internal ---

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
