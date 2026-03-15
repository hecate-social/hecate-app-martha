%%% @doc Tests for division_aggregate (execute/2 + apply_event/2).
%%%
%%% Pure logic tests — no external deps (no ReckonDB, no mesh, no ETS).
%%% Tests all command types through the unified aggregate, state guards,
%%% bit flag transitions, and full lifecycle walkthrough.
-module(division_aggregate_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("guide_division_lifecycle/include/division_lifecycle_status.hrl").
-include_lib("guide_division_lifecycle/include/division_state.hrl").
-include_lib("guide_division_lifecycle/include/kanban_card_status.hrl").

%% ===================================================================
%% Test generators
%% ===================================================================

initiate_test_() ->
    [
        {"initiate on fresh state succeeds",        fun exec_initiate_ok/0},
        {"initiate requires division_id",           fun exec_initiate_missing_fields/0},
        {"only initiate allowed on fresh state",    fun exec_non_initiate_on_fresh/0}
    ].

archive_test_() ->
    [
        {"archive succeeds after initiate",         fun exec_archive_ok/0},
        {"archived division rejects all commands",  fun exec_archived_rejects_all/0},
        {"unknown command returns error",           fun exec_unknown_command/0}
    ].

storming_test_() ->
    [
        {"design aggregate succeeds",               fun exec_design_aggregate_ok/0},
        {"design aggregate blocked when not active", fun exec_design_aggregate_not_active/0},
        {"design event succeeds",                   fun exec_design_event_ok/0},
        {"plan desk succeeds",                      fun exec_plan_desk_ok/0},
        {"plan dependency succeeds",                fun exec_plan_dependency_ok/0}
    ].

planning_test_() ->
    [
        {"planning starts open after initiate",     fun planning_starts_open/0},
        {"open planning fails when already open",   fun exec_open_planning_already_open/0},
        {"shelve planning succeeds when open",      fun exec_shelve_planning_ok/0},
        {"shelve planning fails when not open",     fun exec_shelve_planning_not_open/0},
        {"resume planning succeeds when shelved",   fun exec_resume_planning_ok/0},
        {"resume planning fails when not shelved",  fun exec_resume_planning_not_shelved/0},
        {"submit planning succeeds when open",      fun exec_submit_planning_ok/0},
        {"submit planning fails when not open",     fun exec_submit_planning_not_open/0}
    ].

kanban_test_() ->
    [
        {"post card succeeds",                      fun exec_post_card_ok/0},
        {"post card blocked when board not active",  fun exec_post_card_not_active/0},
        {"pick card succeeds",                      fun exec_pick_card_ok/0},
        {"finish card succeeds",                    fun exec_finish_card_ok/0},
        {"unpick card succeeds",                    fun exec_unpick_card_ok/0},
        {"park card succeeds",                      fun exec_park_card_ok/0},
        {"unpark card succeeds",                    fun exec_unpark_card_ok/0},
        {"block card succeeds",                     fun exec_block_card_ok/0},
        {"unblock card succeeds",                   fun exec_unblock_card_ok/0}
    ].

crafting_test_() ->
    [
        {"crafting starts open after initiate",     fun crafting_starts_open/0},
        {"open crafting fails when already open",   fun exec_open_crafting_already_open/0},
        {"shelve crafting succeeds when open",      fun exec_shelve_crafting_ok/0},
        {"shelve crafting fails when not open",     fun exec_shelve_crafting_not_open/0},
        {"resume crafting succeeds when shelved",   fun exec_resume_crafting_ok/0},
        {"resume crafting fails when not shelved",  fun exec_resume_crafting_not_shelved/0},
        {"generate module succeeds when open",      fun exec_generate_module_ok/0},
        {"generate module blocked when not open",   fun exec_generate_module_not_open/0},
        {"generate test succeeds",                  fun exec_generate_test_ok/0},
        {"run test suite succeeds",                 fun exec_run_test_suite_ok/0},
        {"record test result succeeds",             fun exec_record_test_result_ok/0},
        {"deliver release succeeds",                fun exec_deliver_release_ok/0},
        {"stage delivery succeeds",                 fun exec_stage_delivery_ok/0}
    ].

apply_test_() ->
    [
        {"apply division_initiated sets all fields",    fun apply_initiated/0},
        {"apply division_archived sets flag",           fun apply_archived/0},
        {"apply aggregate_designed adds to map",        fun apply_aggregate_designed/0},
        {"apply event_designed adds to map",            fun apply_event_designed/0},
        {"apply desk_planned adds to map",              fun apply_desk_planned/0},
        {"apply dependency_planned adds to map",        fun apply_dependency_planned/0},
        {"apply planning_opened sets flag",             fun apply_planning_opened/0},
        {"apply planning_shelved toggles flags",        fun apply_planning_shelved/0},
        {"apply planning_resumed toggles flags",        fun apply_planning_resumed/0},
        {"apply planning_submitted sets flag",          fun apply_planning_submitted/0},
        {"apply card_posted adds card",                 fun apply_card_posted/0},
        {"apply card_picked updates card",              fun apply_card_picked/0},
        {"apply card_finished updates card",            fun apply_card_finished/0},
        {"apply card_unpicked resets card",             fun apply_card_unpicked/0},
        {"apply card_parked updates card",              fun apply_card_parked/0},
        {"apply card_unparked resets card",             fun apply_card_unparked/0},
        {"apply card_blocked updates card",             fun apply_card_blocked/0},
        {"apply card_unblocked resets card",            fun apply_card_unblocked/0},
        {"apply crafting_opened sets flag",             fun apply_crafting_opened/0},
        {"apply crafting_shelved toggles flags",        fun apply_crafting_shelved/0},
        {"apply crafting_resumed toggles flags",        fun apply_crafting_resumed/0},
        {"apply module_generated adds to map",          fun apply_module_generated/0},
        {"apply test_generated adds to map",            fun apply_test_generated/0},
        {"apply test_suite_run adds to map",            fun apply_test_suite_run/0},
        {"apply test_result_recorded adds to map",      fun apply_test_result_recorded/0},
        {"apply release_delivered adds to map",         fun apply_release_delivered/0},
        {"apply delivery_staged adds to map",           fun apply_delivery_staged/0},
        {"apply unknown event leaves state unchanged",  fun apply_unknown_event/0},
        {"apply handles binary keys",                   fun apply_binary_keys/0},
        {"apply handles atom keys",                     fun apply_atom_keys/0}
    ].

lifecycle_test_() ->
    [
        {"full division lifecycle: all 4 phases",   fun full_lifecycle/0}
    ].

%% ===================================================================
%% Helpers
%% ===================================================================

fresh() -> division_state:new(<<>>).

apply_events(Events) ->
    lists:foldl(fun(E, S) -> division_aggregate:apply(S, E) end, fresh(), Events).

%% Execute command, apply resulting event(s), return new state
exec_and_apply(State, CmdMap) ->
    {ok, Events} = division_aggregate:execute(State, CmdMap),
    lists:foldl(fun(E, S) -> division_aggregate:apply(S, E) end, State, Events).

initiated_event() ->
    #{event_type => <<"division_initiated_v1">>,
      <<"division_id">> => <<"div-test-1">>,
      <<"venture_id">> => <<"v-test-1">>,
      <<"context_name">> => <<"payments">>,
      <<"initiated_by">> => <<"agent@test">>,
      <<"initiated_at">> => 1000}.

initiated_state() ->
    apply_events([initiated_event()]).

archived_state() ->
    apply_events([
        initiated_event(),
        #{event_type => <<"division_archived_v1">>}
    ]).

%% State with planning shelved (for resume tests)
planning_shelved_state() ->
    apply_events([
        initiated_event(),
        #{event_type => <<"planning_shelved_v1">>,
          <<"shelved_at">> => 2000,
          <<"reason">> => <<"waiting">>}
    ]).

%% State with crafting shelved
crafting_shelved_state() ->
    apply_events([
        initiated_event(),
        #{event_type => <<"crafting_shelved_v1">>,
          <<"shelved_at">> => 2000,
          <<"reason">> => <<"blocked">>}
    ]).

%% State with a posted kanban card
card_posted_state() ->
    apply_events([
        initiated_event(),
        #{event_type => <<"kanban_card_posted_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"title">> => <<"Build auth">>,
          <<"description">> => <<"Auth module">>,
          <<"card_type">> => <<"feature">>,
          <<"posted_by">> => <<"agent@test">>,
          <<"posted_at">> => 2000}
    ]).

%% State with a picked kanban card
card_picked_state() ->
    apply_events([
        initiated_event(),
        #{event_type => <<"kanban_card_posted_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"title">> => <<"Build auth">>,
          <<"card_type">> => <<"feature">>,
          <<"posted_by">> => <<"agent@test">>,
          <<"posted_at">> => 2000},
        #{event_type => <<"kanban_card_picked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"picked_by">> => <<"crafter@test">>,
          <<"picked_at">> => 3000}
    ]).

%% ===================================================================
%% Execute: Initiate Division
%% ===================================================================

exec_initiate_ok() ->
    Cmd = #{command_type => <<"initiate_division_v1">>,
            <<"division_id">> => <<"div-new">>,
            <<"venture_id">> => <<"v-1">>,
            <<"context_name">> => <<"auth">>},
    {ok, [Event]} = division_aggregate:execute(fresh(), Cmd),
    ?assertEqual(division_initiated_v1, maps:get(event_type, Event)),
    ?assertEqual(<<"div-new">>, maps:get(division_id, Event)),
    ?assertEqual(<<"auth">>, maps:get(context_name, Event)).

exec_initiate_missing_fields() ->
    Cmd = #{command_type => <<"initiate_division_v1">>},
    ?assertError({badmatch, {error, missing_required_fields}},
                 division_aggregate:execute(fresh(), Cmd)).

exec_non_initiate_on_fresh() ->
    Cmd = #{command_type => <<"design_aggregate_v1">>,
            <<"division_id">> => <<"div-1">>},
    ?assertEqual({error, division_not_initiated},
                 division_aggregate:execute(fresh(), Cmd)).

%% ===================================================================
%% Execute: Archive + Guards
%% ===================================================================

exec_archive_ok() ->
    Cmd = #{command_type => <<"archive_division_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(division_archived_v1, maps:get(event_type, Event)).

exec_archived_rejects_all() ->
    Cmd = #{command_type => <<"design_aggregate_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"aggregate_name">> => <<"order">>},
    ?assertEqual({error, division_archived},
                 division_aggregate:execute(archived_state(), Cmd)).

exec_unknown_command() ->
    Cmd = #{command_type => <<"do_something_weird_v1">>},
    ?assertEqual({error, unknown_command},
                 division_aggregate:execute(initiated_state(), Cmd)).

%% ===================================================================
%% Execute: Storming
%% ===================================================================

exec_design_aggregate_ok() ->
    Cmd = #{command_type => <<"design_aggregate_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"aggregate_name">> => <<"order_aggregate">>,
            <<"description">> => <<"Manages orders">>,
            <<"stream_prefix">> => <<"order-">>,
            <<"fields">> => [<<"order_id">>, <<"status">>]},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(aggregate_designed_v1, maps:get(event_type, Event)),
    ?assertEqual(<<"order_aggregate">>, maps:get(aggregate_name, Event)).

exec_design_aggregate_not_active() ->
    %% Archive storming by setting the flag manually via events
    S = apply_events([
        initiated_event(),
        #{event_type => <<"division_archived_v1">>}
    ]),
    Cmd = #{command_type => <<"design_aggregate_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"aggregate_name">> => <<"x">>},
    ?assertEqual({error, division_archived},
                 division_aggregate:execute(S, Cmd)).

exec_design_event_ok() ->
    Cmd = #{command_type => <<"design_event_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"event_name">> => <<"order_placed_v1">>,
            <<"description">> => <<"Order was placed">>,
            <<"aggregate_name">> => <<"order_aggregate">>,
            <<"fields">> => [<<"order_id">>, <<"amount">>]},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(event_designed_v1, maps:get(event_type, Event)).

exec_plan_desk_ok() ->
    Cmd = #{command_type => <<"plan_desk_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"desk_name">> => <<"place_order">>,
            <<"department">> => <<"cmd">>,
            <<"description">> => <<"Handles order placement">>,
            <<"commands">> => [<<"place_order_v1">>]},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(desk_planned_v1, maps:get(event_type, Event)).

exec_plan_dependency_ok() ->
    Cmd = #{command_type => <<"plan_dependency_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"dependency_id">> => <<"dep-1">>,
            <<"from_desk">> => <<"place_order">>,
            <<"to_desk">> => <<"ship_order">>,
            <<"dep_type">> => <<"event">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(dependency_planned_v1, maps:get(event_type, Event)).

%% ===================================================================
%% Execute: Planning
%% ===================================================================

planning_starts_open() ->
    S = initiated_state(),
    ?assert(S#division_state.planning_status band ?PLANNING_OPEN =/= 0),
    ?assert(S#division_state.planning_status band ?PLANNING_INITIATED =/= 0).

exec_open_planning_already_open() ->
    %% Planning starts open after initiate, so opening again should fail
    Cmd = #{command_type => <<"open_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, planning_already_open},
                 division_aggregate:execute(initiated_state(), Cmd)).

exec_shelve_planning_ok() ->
    Cmd = #{command_type => <<"shelve_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"reason">> => <<"need more research">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(planning_shelved_v1, maps:get(event_type, Event)).

exec_shelve_planning_not_open() ->
    Cmd = #{command_type => <<"shelve_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, planning_not_open},
                 division_aggregate:execute(planning_shelved_state(), Cmd)).

exec_resume_planning_ok() ->
    Cmd = #{command_type => <<"resume_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    {ok, [Event]} = division_aggregate:execute(planning_shelved_state(), Cmd),
    ?assertEqual(planning_resumed_v1, maps:get(event_type, Event)).

exec_resume_planning_not_shelved() ->
    Cmd = #{command_type => <<"resume_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, planning_not_shelved},
                 division_aggregate:execute(initiated_state(), Cmd)).

exec_submit_planning_ok() ->
    Cmd = #{command_type => <<"submit_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(planning_submitted_v1, maps:get(event_type, Event)).

exec_submit_planning_not_open() ->
    Cmd = #{command_type => <<"submit_planning_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, planning_not_open},
                 division_aggregate:execute(planning_shelved_state(), Cmd)).

%% ===================================================================
%% Execute: Kanban
%% ===================================================================

exec_post_card_ok() ->
    Cmd = #{command_type => <<"post_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-new">>,
            <<"title">> => <<"Implement auth">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(kanban_card_posted_v1, maps:get(event_type, Event)).

exec_post_card_not_active() ->
    Cmd = #{command_type => <<"post_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>,
            <<"title">> => <<"X">>},
    ?assertEqual({error, division_archived},
                 division_aggregate:execute(archived_state(), Cmd)).

exec_pick_card_ok() ->
    Cmd = #{command_type => <<"pick_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>,
            <<"picked_by">> => <<"crafter@test">>},
    {ok, [Event]} = division_aggregate:execute(card_posted_state(), Cmd),
    ?assertEqual(kanban_card_picked_v1, maps:get(event_type, Event)).

exec_finish_card_ok() ->
    Cmd = #{command_type => <<"finish_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>},
    {ok, [Event]} = division_aggregate:execute(card_picked_state(), Cmd),
    ?assertEqual(kanban_card_finished_v1, maps:get(event_type, Event)).

exec_unpick_card_ok() ->
    Cmd = #{command_type => <<"unpick_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>},
    {ok, [Event]} = division_aggregate:execute(card_picked_state(), Cmd),
    ?assertEqual(kanban_card_unpicked_v1, maps:get(event_type, Event)).

exec_park_card_ok() ->
    Cmd = #{command_type => <<"park_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>,
            <<"parked_by">> => <<"agent@test">>,
            <<"park_reason">> => <<"blocked by dependency">>},
    {ok, [Event]} = division_aggregate:execute(card_posted_state(), Cmd),
    ?assertEqual(kanban_card_parked_v1, maps:get(event_type, Event)).

exec_unpark_card_ok() ->
    S = apply_events([
        initiated_event(),
        #{event_type => <<"kanban_card_posted_v1">>,
          <<"card_id">> => <<"card-1">>, <<"title">> => <<"X">>,
          <<"card_type">> => <<"feature">>,
          <<"posted_by">> => <<"a">>, <<"posted_at">> => 2000},
        #{event_type => <<"kanban_card_parked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"parked_by">> => <<"a">>, <<"parked_at">> => 3000,
          <<"park_reason">> => <<"wait">>}
    ]),
    Cmd = #{command_type => <<"unpark_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>},
    {ok, [Event]} = division_aggregate:execute(S, Cmd),
    ?assertEqual(kanban_card_unparked_v1, maps:get(event_type, Event)).

exec_block_card_ok() ->
    Cmd = #{command_type => <<"block_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>,
            <<"blocked_by">> => <<"agent@test">>,
            <<"block_reason">> => <<"needs clarification">>},
    {ok, [Event]} = division_aggregate:execute(card_posted_state(), Cmd),
    ?assertEqual(kanban_card_blocked_v1, maps:get(event_type, Event)).

exec_unblock_card_ok() ->
    S = apply_events([
        initiated_event(),
        #{event_type => <<"kanban_card_posted_v1">>,
          <<"card_id">> => <<"card-1">>, <<"title">> => <<"X">>,
          <<"card_type">> => <<"feature">>,
          <<"posted_by">> => <<"a">>, <<"posted_at">> => 2000},
        #{event_type => <<"kanban_card_blocked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"blocked_by">> => <<"a">>, <<"blocked_at">> => 3000,
          <<"block_reason">> => <<"unclear">>}
    ]),
    Cmd = #{command_type => <<"unblock_kanban_card_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"card_id">> => <<"card-1">>},
    {ok, [Event]} = division_aggregate:execute(S, Cmd),
    ?assertEqual(kanban_card_unblocked_v1, maps:get(event_type, Event)).

%% ===================================================================
%% Execute: Crafting
%% ===================================================================

crafting_starts_open() ->
    S = initiated_state(),
    ?assert(S#division_state.crafting_status band ?CRAFTING_OPEN =/= 0),
    ?assert(S#division_state.crafting_status band ?CRAFTING_INITIATED =/= 0).

exec_open_crafting_already_open() ->
    Cmd = #{command_type => <<"open_crafting_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, crafting_already_open},
                 division_aggregate:execute(initiated_state(), Cmd)).

exec_shelve_crafting_ok() ->
    Cmd = #{command_type => <<"shelve_crafting_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"reason">> => <<"blocked">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(crafting_shelved_v1, maps:get(event_type, Event)).

exec_shelve_crafting_not_open() ->
    Cmd = #{command_type => <<"shelve_crafting_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, crafting_not_open},
                 division_aggregate:execute(crafting_shelved_state(), Cmd)).

exec_resume_crafting_ok() ->
    Cmd = #{command_type => <<"resume_crafting_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    {ok, [Event]} = division_aggregate:execute(crafting_shelved_state(), Cmd),
    ?assertEqual(crafting_resumed_v1, maps:get(event_type, Event)).

exec_resume_crafting_not_shelved() ->
    Cmd = #{command_type => <<"resume_crafting_v1">>,
            <<"division_id">> => <<"div-test-1">>},
    ?assertEqual({error, crafting_not_shelved},
                 division_aggregate:execute(initiated_state(), Cmd)).

exec_generate_module_ok() ->
    Cmd = #{command_type => <<"generate_module_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"module_name">> => <<"order_handler">>,
            <<"module_type">> => <<"handler">>,
            <<"path">> => <<"src/order_handler.erl">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(module_generated_v1, maps:get(event_type, Event)).

exec_generate_module_not_open() ->
    Cmd = #{command_type => <<"generate_module_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"module_name">> => <<"x">>,
            <<"module_type">> => <<"handler">>,
            <<"path">> => <<"src/x.erl">>},
    ?assertEqual({error, crafting_not_open},
                 division_aggregate:execute(crafting_shelved_state(), Cmd)).

exec_generate_test_ok() ->
    Cmd = #{command_type => <<"generate_test_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"test_name">> => <<"order_handler_tests">>,
            <<"module_name">> => <<"order_handler">>,
            <<"path">> => <<"test/order_handler_tests.erl">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(test_generated_v1, maps:get(event_type, Event)).

exec_run_test_suite_ok() ->
    Cmd = #{command_type => <<"run_test_suite_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"suite_id">> => <<"suite-1">>,
            <<"suite_name">> => <<"unit_tests">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(test_suite_run_v1, maps:get(event_type, Event)).

exec_record_test_result_ok() ->
    Cmd = #{command_type => <<"record_test_result_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"result_id">> => <<"result-1">>,
            <<"suite_id">> => <<"suite-1">>,
            <<"passed">> => 10,
            <<"failed">> => 0},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(test_result_recorded_v1, maps:get(event_type, Event)).

exec_deliver_release_ok() ->
    Cmd = #{command_type => <<"deliver_release_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"release_id">> => <<"rel-1">>,
            <<"version">> => <<"0.1.0">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(release_delivered_v1, maps:get(event_type, Event)).

exec_stage_delivery_ok() ->
    Cmd = #{command_type => <<"stage_delivery_v1">>,
            <<"division_id">> => <<"div-test-1">>,
            <<"stage_id">> => <<"stage-1">>,
            <<"release_id">> => <<"rel-1">>,
            <<"stage_name">> => <<"staging">>},
    {ok, [Event]} = division_aggregate:execute(initiated_state(), Cmd),
    ?assertEqual(delivery_staged_v1, maps:get(event_type, Event)).

%% ===================================================================
%% Apply: State Transitions
%% ===================================================================

apply_initiated() ->
    S = initiated_state(),
    ?assertEqual(<<"div-test-1">>, S#division_state.division_id),
    ?assertEqual(<<"v-test-1">>, S#division_state.venture_id),
    ?assertEqual(<<"payments">>, S#division_state.context_name),
    ?assertEqual(<<"agent@test">>, S#division_state.initiated_by),
    ?assertEqual(1000, S#division_state.initiated_at),
    %% Division-level
    ?assert(S#division_state.status band ?DIV_INITIATED =/= 0),
    %% Storming: initiated + active
    ?assert(S#division_state.storming_status band ?STORMING_INITIATED =/= 0),
    ?assert(S#division_state.storming_status band ?STORMING_ACTIVE =/= 0),
    %% Planning: initiated + open
    ?assert(S#division_state.planning_status band ?PLANNING_INITIATED =/= 0),
    ?assert(S#division_state.planning_status band ?PLANNING_OPEN =/= 0),
    %% Kanban: initiated + active
    ?assert(S#division_state.kanban_status band ?BOARD_INITIATED =/= 0),
    ?assert(S#division_state.kanban_status band ?BOARD_ACTIVE =/= 0),
    %% Crafting: initiated + open
    ?assert(S#division_state.crafting_status band ?CRAFTING_INITIATED =/= 0),
    ?assert(S#division_state.crafting_status band ?CRAFTING_OPEN =/= 0).

apply_archived() ->
    S = archived_state(),
    ?assert(S#division_state.status band ?DIV_ARCHIVED =/= 0).

apply_aggregate_designed() ->
    E = #{event_type => <<"aggregate_designed_v1">>,
          <<"aggregate_name">> => <<"order_agg">>,
          <<"description">> => <<"Orders">>,
          <<"stream_prefix">> => <<"order-">>,
          <<"fields">> => [<<"id">>],
          <<"designed_at">> => 2000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"order_agg">>, S#division_state.designed_aggregates)),
    #{<<"order_agg">> := Agg} = S#division_state.designed_aggregates,
    ?assertEqual(<<"Orders">>, maps:get(description, Agg)).

apply_event_designed() ->
    E = #{event_type => <<"event_designed_v1">>,
          <<"event_name">> => <<"order_placed_v1">>,
          <<"description">> => <<"Placed">>,
          <<"aggregate_name">> => <<"order_agg">>,
          <<"fields">> => [],
          <<"designed_at">> => 2000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"order_placed_v1">>, S#division_state.designed_events)).

apply_desk_planned() ->
    E = #{event_type => <<"desk_planned_v1">>,
          <<"desk_name">> => <<"place_order">>,
          <<"department">> => <<"cmd">>,
          <<"description">> => <<"Place order desk">>,
          <<"commands">> => [<<"place_order_v1">>],
          <<"planned_at">> => 2000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"place_order">>, S#division_state.planned_desks)).

apply_dependency_planned() ->
    E = #{event_type => <<"dependency_planned_v1">>,
          <<"dependency_id">> => <<"dep-1">>,
          <<"from_desk">> => <<"a">>,
          <<"to_desk">> => <<"b">>,
          <<"dep_type">> => <<"event">>,
          <<"planned_at">> => 2000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"dep-1">>, S#division_state.planned_dependencies)).

apply_planning_opened() ->
    %% Shelve first, then open
    S0 = planning_shelved_state(),
    E = #{event_type => <<"planning_opened_v1">>,
          <<"opened_at">> => 5000},
    S = division_aggregate:apply(S0, E),
    ?assert(S#division_state.planning_status band ?PLANNING_OPEN =/= 0),
    ?assertEqual(5000, S#division_state.planning_opened_at).

apply_planning_shelved() ->
    E = #{event_type => <<"planning_shelved_v1">>,
          <<"shelved_at">> => 3000,
          <<"reason">> => <<"blocked">>},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(S#division_state.planning_status band ?PLANNING_SHELVED =/= 0),
    ?assert(S#division_state.planning_status band ?PLANNING_OPEN =:= 0),
    ?assertEqual(3000, S#division_state.planning_shelved_at),
    ?assertEqual(<<"blocked">>, S#division_state.planning_shelve_reason).

apply_planning_resumed() ->
    E = #{event_type => <<"planning_resumed_v1">>},
    S = division_aggregate:apply(planning_shelved_state(), E),
    ?assert(S#division_state.planning_status band ?PLANNING_OPEN =/= 0),
    ?assert(S#division_state.planning_status band ?PLANNING_SHELVED =:= 0),
    ?assertEqual(undefined, S#division_state.planning_shelved_at),
    ?assertEqual(undefined, S#division_state.planning_shelve_reason).

apply_planning_submitted() ->
    E = #{event_type => <<"planning_submitted_v1">>},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(S#division_state.planning_status band ?PLANNING_SUBMITTED =/= 0).

apply_card_posted() ->
    S = card_posted_state(),
    Cards = S#division_state.cards,
    ?assert(maps:is_key(<<"card-1">>, Cards)),
    #{<<"card-1">> := Card} = Cards,
    ?assertEqual(?CARD_POSTED, maps:get(status, Card)),
    ?assertEqual(<<"Build auth">>, maps:get(title, Card)).

apply_card_picked() ->
    S = card_picked_state(),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_PICKED, maps:get(status, Card)),
    ?assertEqual(<<"crafter@test">>, maps:get(picked_by, Card)).

apply_card_finished() ->
    E = #{event_type => <<"kanban_card_finished_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"finished_at">> => 4000},
    S = division_aggregate:apply(card_picked_state(), E),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_FINISHED, maps:get(status, Card)),
    ?assertEqual(4000, maps:get(finished_at, Card)).

apply_card_unpicked() ->
    E = #{event_type => <<"kanban_card_unpicked_v1">>,
          <<"card_id">> => <<"card-1">>},
    S = division_aggregate:apply(card_picked_state(), E),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_POSTED, maps:get(status, Card)),
    ?assertEqual(undefined, maps:get(picked_by, Card)).

apply_card_parked() ->
    E = #{event_type => <<"kanban_card_parked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"parked_by">> => <<"pm@test">>,
          <<"parked_at">> => 3000,
          <<"park_reason">> => <<"waiting">>},
    S = division_aggregate:apply(card_posted_state(), E),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_PARKED, maps:get(status, Card)),
    ?assertEqual(<<"waiting">>, maps:get(park_reason, Card)).

apply_card_unparked() ->
    S0 = division_aggregate:apply(
        card_posted_state(),
        #{event_type => <<"kanban_card_parked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"parked_by">> => <<"a">>, <<"parked_at">> => 3000,
          <<"park_reason">> => <<"x">>}),
    E = #{event_type => <<"kanban_card_unparked_v1">>,
          <<"card_id">> => <<"card-1">>},
    S = division_aggregate:apply(S0, E),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_POSTED, maps:get(status, Card)),
    ?assertEqual(undefined, maps:get(parked_by, Card)).

apply_card_blocked() ->
    E = #{event_type => <<"kanban_card_blocked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"blocked_by">> => <<"pm@test">>,
          <<"blocked_at">> => 3000,
          <<"block_reason">> => <<"unclear spec">>},
    S = division_aggregate:apply(card_posted_state(), E),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_BLOCKED, maps:get(status, Card)),
    ?assertEqual(<<"unclear spec">>, maps:get(block_reason, Card)).

apply_card_unblocked() ->
    S0 = division_aggregate:apply(
        card_posted_state(),
        #{event_type => <<"kanban_card_blocked_v1">>,
          <<"card_id">> => <<"card-1">>,
          <<"blocked_by">> => <<"a">>, <<"blocked_at">> => 3000,
          <<"block_reason">> => <<"x">>}),
    E = #{event_type => <<"kanban_card_unblocked_v1">>,
          <<"card_id">> => <<"card-1">>},
    S = division_aggregate:apply(S0, E),
    #{<<"card-1">> := Card} = S#division_state.cards,
    ?assertEqual(?CARD_POSTED, maps:get(status, Card)),
    ?assertEqual(undefined, maps:get(blocked_by, Card)).

apply_crafting_opened() ->
    S0 = crafting_shelved_state(),
    E = #{event_type => <<"crafting_opened_v1">>,
          <<"opened_at">> => 5000},
    S = division_aggregate:apply(S0, E),
    ?assert(S#division_state.crafting_status band ?CRAFTING_OPEN =/= 0),
    ?assertEqual(5000, S#division_state.crafting_opened_at).

apply_crafting_shelved() ->
    E = #{event_type => <<"crafting_shelved_v1">>,
          <<"shelved_at">> => 3000,
          <<"reason">> => <<"blocked">>},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(S#division_state.crafting_status band ?CRAFTING_SHELVED =/= 0),
    ?assert(S#division_state.crafting_status band ?CRAFTING_OPEN =:= 0),
    ?assertEqual(3000, S#division_state.crafting_shelved_at),
    ?assertEqual(<<"blocked">>, S#division_state.crafting_shelve_reason).

apply_crafting_resumed() ->
    E = #{event_type => <<"crafting_resumed_v1">>},
    S = division_aggregate:apply(crafting_shelved_state(), E),
    ?assert(S#division_state.crafting_status band ?CRAFTING_OPEN =/= 0),
    ?assert(S#division_state.crafting_status band ?CRAFTING_SHELVED =:= 0),
    ?assertEqual(undefined, S#division_state.crafting_shelved_at),
    ?assertEqual(undefined, S#division_state.crafting_shelve_reason).

apply_module_generated() ->
    E = #{event_type => <<"module_generated_v1">>,
          <<"module_name">> => <<"order_handler">>,
          <<"module_type">> => <<"handler">>,
          <<"path">> => <<"src/order_handler.erl">>,
          <<"generated_at">> => 3000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"order_handler">>, S#division_state.generated_modules)).

apply_test_generated() ->
    E = #{event_type => <<"test_generated_v1">>,
          <<"test_name">> => <<"order_tests">>,
          <<"module_name">> => <<"order_handler">>,
          <<"path">> => <<"test/order_tests.erl">>,
          <<"generated_at">> => 3000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"order_tests">>, S#division_state.generated_tests)).

apply_test_suite_run() ->
    E = #{event_type => <<"test_suite_run_v1">>,
          <<"suite_id">> => <<"s-1">>,
          <<"suite_name">> => <<"unit">>,
          <<"run_at">> => 3000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"s-1">>, S#division_state.test_suites)).

apply_test_result_recorded() ->
    E = #{event_type => <<"test_result_recorded_v1">>,
          <<"result_id">> => <<"r-1">>,
          <<"suite_id">> => <<"s-1">>,
          <<"passed">> => 10,
          <<"failed">> => 0,
          <<"recorded_at">> => 4000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"r-1">>, S#division_state.test_results)).

apply_release_delivered() ->
    E = #{event_type => <<"release_delivered_v1">>,
          <<"release_id">> => <<"rel-1">>,
          <<"version">> => <<"0.1.0">>,
          <<"delivered_at">> => 5000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"rel-1">>, S#division_state.releases)).

apply_delivery_staged() ->
    E = #{event_type => <<"delivery_staged_v1">>,
          <<"stage_id">> => <<"stg-1">>,
          <<"release_id">> => <<"rel-1">>,
          <<"stage_name">> => <<"staging">>,
          <<"staged_at">> => 6000},
    S = division_aggregate:apply(initiated_state(), E),
    ?assert(maps:is_key(<<"stg-1">>, S#division_state.delivery_stages)).

apply_unknown_event() ->
    E = #{event_type => <<"something_weird_v1">>},
    S = initiated_state(),
    ?assertEqual(S, division_aggregate:apply(S, E)).

apply_binary_keys() ->
    E = #{event_type => <<"division_initiated_v1">>,
          <<"division_id">> => <<"div-bin">>,
          <<"venture_id">> => <<"v-bin">>,
          <<"context_name">> => <<"ctx">>,
          <<"initiated_at">> => 1000},
    S = division_aggregate:apply(fresh(), E),
    ?assertEqual(<<"div-bin">>, S#division_state.division_id).

apply_atom_keys() ->
    E = #{event_type => <<"division_initiated_v1">>,
          division_id => <<"div-atom">>,
          venture_id => <<"v-atom">>,
          context_name => <<"ctx">>,
          initiated_at => 2000},
    S = division_aggregate:apply(fresh(), E),
    ?assertEqual(<<"div-atom">>, S#division_state.division_id).

%% ===================================================================
%% Full Lifecycle: Walk through all 4 phases
%% ===================================================================

full_lifecycle() ->
    S0 = fresh(),

    %% 1. Initiate division (sets all 4 phases to active/open)
    S1 = exec_and_apply(S0,
        #{command_type => <<"initiate_division_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"venture_id">> => <<"v-full">>,
          <<"context_name">> => <<"orders">>}),
    ?assert(S1#division_state.status band ?DIV_INITIATED =/= 0),
    ?assert(S1#division_state.storming_status band ?STORMING_ACTIVE =/= 0),
    ?assert(S1#division_state.planning_status band ?PLANNING_OPEN =/= 0),
    ?assert(S1#division_state.kanban_status band ?BOARD_ACTIVE =/= 0),
    ?assert(S1#division_state.crafting_status band ?CRAFTING_OPEN =/= 0),

    %% 2. STORMING: design an aggregate
    S2 = exec_and_apply(S1,
        #{command_type => <<"design_aggregate_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"aggregate_name">> => <<"order_agg">>,
          <<"description">> => <<"Manages orders">>}),
    ?assertEqual(1, maps:size(S2#division_state.designed_aggregates)),

    %% 3. STORMING: design an event
    S3 = exec_and_apply(S2,
        #{command_type => <<"design_event_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"event_name">> => <<"order_placed_v1">>,
          <<"aggregate_name">> => <<"order_agg">>}),
    ?assertEqual(1, maps:size(S3#division_state.designed_events)),

    %% 4. STORMING: plan a desk
    S4 = exec_and_apply(S3,
        #{command_type => <<"plan_desk_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"desk_name">> => <<"place_order">>,
          <<"department">> => <<"cmd">>}),
    ?assertEqual(1, maps:size(S4#division_state.planned_desks)),

    %% 5. STORMING: plan a dependency
    S5 = exec_and_apply(S4,
        #{command_type => <<"plan_dependency_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"dependency_id">> => <<"dep-1">>,
          <<"from_desk">> => <<"place_order">>,
          <<"to_desk">> => <<"ship_order">>}),
    ?assertEqual(1, maps:size(S5#division_state.planned_dependencies)),

    %% 6. PLANNING: shelve
    S6 = exec_and_apply(S5,
        #{command_type => <<"shelve_planning_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"reason">> => <<"need stakeholder input">>}),
    ?assert(S6#division_state.planning_status band ?PLANNING_SHELVED =/= 0),
    ?assert(S6#division_state.planning_status band ?PLANNING_OPEN =:= 0),

    %% 7. PLANNING: resume
    S7 = exec_and_apply(S6,
        #{command_type => <<"resume_planning_v1">>,
          <<"division_id">> => <<"div-full">>}),
    ?assert(S7#division_state.planning_status band ?PLANNING_OPEN =/= 0),
    ?assert(S7#division_state.planning_status band ?PLANNING_SHELVED =:= 0),

    %% 8. PLANNING: submit
    S8 = exec_and_apply(S7,
        #{command_type => <<"submit_planning_v1">>,
          <<"division_id">> => <<"div-full">>}),
    ?assert(S8#division_state.planning_status band ?PLANNING_SUBMITTED =/= 0),

    %% 9. KANBAN: post a card
    S9 = exec_and_apply(S8,
        #{command_type => <<"post_kanban_card_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"card_id">> => <<"card-1">>,
          <<"title">> => <<"Build order aggregate">>}),
    ?assertEqual(1, maps:size(S9#division_state.cards)),

    %% 10. KANBAN: pick the card
    S10 = exec_and_apply(S9,
        #{command_type => <<"pick_kanban_card_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"card_id">> => <<"card-1">>,
          <<"picked_by">> => <<"crafter-agent">>}),
    #{<<"card-1">> := PickedCard} = S10#division_state.cards,
    ?assertEqual(?CARD_PICKED, maps:get(status, PickedCard)),

    %% 11. KANBAN: finish the card
    S11 = exec_and_apply(S10,
        #{command_type => <<"finish_kanban_card_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"card_id">> => <<"card-1">>}),
    #{<<"card-1">> := FinishedCard} = S11#division_state.cards,
    ?assertEqual(?CARD_FINISHED, maps:get(status, FinishedCard)),

    %% 12. CRAFTING: generate a module
    S12 = exec_and_apply(S11,
        #{command_type => <<"generate_module_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"module_name">> => <<"order_handler">>,
          <<"module_type">> => <<"handler">>,
          <<"path">> => <<"src/order_handler.erl">>}),
    ?assertEqual(1, maps:size(S12#division_state.generated_modules)),

    %% 13. CRAFTING: generate a test
    S13 = exec_and_apply(S12,
        #{command_type => <<"generate_test_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"test_name">> => <<"order_handler_tests">>,
          <<"module_name">> => <<"order_handler">>,
          <<"path">> => <<"test/order_handler_tests.erl">>}),
    ?assertEqual(1, maps:size(S13#division_state.generated_tests)),

    %% 14. CRAFTING: run a test suite
    S14 = exec_and_apply(S13,
        #{command_type => <<"run_test_suite_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"suite_id">> => <<"suite-1">>,
          <<"suite_name">> => <<"eunit">>}),
    ?assertEqual(1, maps:size(S14#division_state.test_suites)),

    %% 15. CRAFTING: record test result
    S15 = exec_and_apply(S14,
        #{command_type => <<"record_test_result_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"result_id">> => <<"result-1">>,
          <<"suite_id">> => <<"suite-1">>,
          <<"passed">> => 42,
          <<"failed">> => 0}),
    ?assertEqual(1, maps:size(S15#division_state.test_results)),

    %% 16. CRAFTING: deliver a release
    S16 = exec_and_apply(S15,
        #{command_type => <<"deliver_release_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"release_id">> => <<"rel-1">>,
          <<"version">> => <<"1.0.0">>}),
    ?assertEqual(1, maps:size(S16#division_state.releases)),

    %% 17. CRAFTING: stage the delivery
    S17 = exec_and_apply(S16,
        #{command_type => <<"stage_delivery_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"stage_id">> => <<"stage-1">>,
          <<"release_id">> => <<"rel-1">>,
          <<"stage_name">> => <<"production">>}),
    ?assertEqual(1, maps:size(S17#division_state.delivery_stages)),

    %% 18. CRAFTING: shelve
    S18 = exec_and_apply(S17,
        #{command_type => <<"shelve_crafting_v1">>,
          <<"division_id">> => <<"div-full">>,
          <<"reason">> => <<"release done">>}),
    ?assert(S18#division_state.crafting_status band ?CRAFTING_SHELVED =/= 0),

    %% 19. CRAFTING: resume
    S19 = exec_and_apply(S18,
        #{command_type => <<"resume_crafting_v1">>,
          <<"division_id">> => <<"div-full">>}),
    ?assert(S19#division_state.crafting_status band ?CRAFTING_OPEN =/= 0),

    %% 20. ARCHIVE the division
    S20 = exec_and_apply(S19,
        #{command_type => <<"archive_division_v1">>,
          <<"division_id">> => <<"div-full">>}),
    ?assert(S20#division_state.status band ?DIV_ARCHIVED =/= 0),

    %% Verify archived blocks everything
    ?assertEqual({error, division_archived},
                 division_aggregate:execute(S20,
                     #{command_type => <<"design_aggregate_v1">>,
                       <<"division_id">> => <<"div-full">>,
                       <<"aggregate_name">> => <<"x">>})),
    ?assertEqual({error, division_archived},
                 division_aggregate:execute(S20,
                     #{command_type => <<"post_kanban_card_v1">>,
                       <<"division_id">> => <<"div-full">>,
                       <<"card_id">> => <<"c">>,
                       <<"title">> => <<"x">>})),
    ?assertEqual({error, division_archived},
                 division_aggregate:execute(S20,
                     #{command_type => <<"generate_module_v1">>,
                       <<"division_id">> => <<"div-full">>,
                       <<"module_name">> => <<"x">>,
                       <<"module_type">> => <<"y">>,
                       <<"path">> => <<"z">>})).
