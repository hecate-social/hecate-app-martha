%%% @doc Domain tests for venture projections and queries.
%%%
%%% Tests the full flow: event data -> projection -> ETS -> query.
%%% Uses real ETS tables (no mocks). No ReckonDB needed.
%%% @end
-module(venture_projection_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("guide_venture_lifecycle/include/venture_lifecycle_status.hrl").

%% ===================================================================
%% Test generators
%% ===================================================================

projection_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
         {"project venture_initiated_v1 creates venture in ETS",
          fun project_initiated_creates_venture/0},
         {"get_venture returns projected venture",
          fun get_venture_after_projection/0},
         {"list_ventures returns projected ventures sorted by initiated_at",
          fun list_ventures_sorted/0},
         {"vision_refined updates venture fields",
          fun project_vision_refined_updates/0},
         {"vision_refined with partial fields uses COALESCE",
          fun project_vision_refined_partial/0},
         {"discovery lifecycle toggles status flags",
          fun project_discovery_lifecycle/0},
         {"venture_archived sets archived flag",
          fun project_archived/0},
         {"list_ventures_active excludes archived",
          fun list_ventures_active_excludes_archived/0},
         {"full lifecycle: initiate -> query -> verify",
          fun full_initiate_query_flow/0},
         {"division identified projects to divisions table",
          fun project_division_identified/0},
         {"count_divisions returns correct count",
          fun count_divisions_works/0}
     ]}.

%% ===================================================================
%% Setup / Teardown
%% ===================================================================

tables() ->
    [project_ventures_ventures,
     project_ventures_divisions,
     project_ventures_storm_sessions,
     project_ventures_stickies,
     project_ventures_stacks,
     project_ventures_clusters,
     project_ventures_arrows].

setup() ->
    %% Delete any existing tables so evoq_read_model:new/2 can recreate them
    lists:foreach(fun(T) ->
        case ets:whereis(T) of
            undefined -> ok;
            _ -> ets:delete(T)
        end
    end, tables()),
    %% Clear cached read models from process dictionary
    lists:foreach(fun(T) -> erase({rm, T}) end, tables()),
    ok.

teardown(_) ->
    lists:foreach(fun(T) ->
        case ets:whereis(T) of
            undefined -> ok;
            _ -> ets:delete(T)
        end
    end, tables()),
    lists:foreach(fun(T) -> erase({rm, T}) end, tables()),
    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

%% Simulate what evoq_projection does: call project/4 directly
project_event(Module, EventType, Data) ->
    Event = #{
        event_type => EventType,
        data => Data,
        stream_id => <<"test-stream">>,
        version => 1
    },
    Metadata = #{version => 1},
    TableName = get_table_for_module(Module),
    RM = get_or_create_rm(TableName),
    Module:project(Event, Metadata, #{}, RM).

get_or_create_rm(TableName) ->
    case get({rm, TableName}) of
        undefined ->
            {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => TableName}),
            put({rm, TableName}, RM),
            RM;
        RM ->
            RM
    end.

get_table_for_module(venture_lifecycle_to_ventures) -> project_ventures_ventures;
get_table_for_module(venture_lifecycle_to_discovered_divisions) -> project_ventures_divisions.

initiated_event_data() ->
    #{
        <<"venture_id">> => <<"v-test-1">>,
        <<"name">> => <<"Test Venture">>,
        <<"brief">> => <<"A test venture">>,
        <<"repos">> => [<<"repo1">>],
        <<"skills">> => [<<"erlang">>],
        <<"context_map">> => #{<<"domain">> => <<"test">>},
        <<"initiated_by">> => <<"user@test">>,
        <<"initiated_at">> => 1000
    }.

%% ===================================================================
%% Projection Tests
%% ===================================================================

project_initiated_creates_venture() ->
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        initiated_event_data()
    ),
    %% Verify in ETS
    [{<<"v-test-1">>, V}] = ets:lookup(project_ventures_ventures, <<"v-test-1">>),
    ?assertEqual(<<"Test Venture">>, maps:get(name, V)),
    ?assertEqual(<<"A test venture">>, maps:get(brief, V)),
    ?assertEqual([<<"repo1">>], maps:get(repos, V)),
    ?assertEqual([<<"erlang">>], maps:get(skills, V)),
    ?assertEqual(1000, maps:get(initiated_at, V)),
    ?assertEqual(<<"user@test">>, maps:get(initiated_by, V)),
    %% Status flag check
    Status = maps:get(status, V),
    ?assert(Status band ?VL_INITIATED =/= 0).

get_venture_after_projection() ->
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        initiated_event_data()
    ),
    {ok, V} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assertEqual(<<"Test Venture">>, maps:get(name, V)),
    ?assertEqual({error, not_found}, project_ventures_store:get_venture(<<"nonexistent">>)).

list_ventures_sorted() ->
    %% Insert two ventures with different timestamps
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        (initiated_event_data())#{<<"venture_id">> => <<"v-1">>, <<"initiated_at">> => 1000}
    ),
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        (initiated_event_data())#{<<"venture_id">> => <<"v-2">>, <<"initiated_at">> => 2000}
    ),
    {ok, Ventures} = project_ventures_store:list_ventures(),
    ?assertEqual(2, length(Ventures)),
    %% Should be sorted by initiated_at DESC (newest first)
    [First, Second] = Ventures,
    ?assertEqual(<<"v-2">>, maps:get(venture_id, First)),
    ?assertEqual(<<"v-1">>, maps:get(venture_id, Second)).

project_vision_refined_updates() ->
    %% First initiate
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        initiated_event_data()
    ),
    %% Then refine vision
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"vision_refined_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>,
          <<"brief">> => <<"Updated brief">>,
          <<"repos">> => [<<"repo2">>],
          <<"skills">> => [<<"go">>],
          <<"context_map">> => #{<<"new">> => <<"map">>}}
    ),
    {ok, V} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assertEqual(<<"Updated brief">>, maps:get(brief, V)),
    ?assertEqual([<<"repo2">>], maps:get(repos, V)),
    ?assertEqual([<<"go">>], maps:get(skills, V)),
    Status = maps:get(status, V),
    ?assert(Status band ?VL_VISION_REFINED =/= 0).

project_vision_refined_partial() ->
    %% Initiate with full data
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        initiated_event_data()
    ),
    %% Refine with only brief (other fields undefined -> coalesce preserves)
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"vision_refined_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>,
          <<"brief">> => <<"New brief only">>}
    ),
    {ok, V} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assertEqual(<<"New brief only">>, maps:get(brief, V)),
    %% Original values preserved
    ?assertEqual([<<"repo1">>], maps:get(repos, V)),
    ?assertEqual([<<"erlang">>], maps:get(skills, V)).

project_discovery_lifecycle() ->
    %% Initiate
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        initiated_event_data()
    ),
    %% Start discovery
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"discovery_started_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>}
    ),
    {ok, V1} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assert(maps:get(status, V1) band ?VL_DISCOVERING =/= 0),
    %% Pause
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"discovery_paused_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>}
    ),
    {ok, V2} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assert(maps:get(status, V2) band ?VL_DISCOVERING =:= 0),
    ?assert(maps:get(status, V2) band ?VL_DISCOVERY_PAUSED =/= 0),
    %% Resume
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"discovery_resumed_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>}
    ),
    {ok, V3} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assert(maps:get(status, V3) band ?VL_DISCOVERING =/= 0),
    ?assert(maps:get(status, V3) band ?VL_DISCOVERY_PAUSED =:= 0),
    %% Complete
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"discovery_completed_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>}
    ),
    {ok, V4} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assert(maps:get(status, V4) band ?VL_DISCOVERING =:= 0),
    ?assert(maps:get(status, V4) band ?VL_DISCOVERY_COMPLETED =/= 0).

project_archived() ->
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        initiated_event_data()
    ),
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_archived_v1">>,
        #{<<"venture_id">> => <<"v-test-1">>}
    ),
    {ok, V} = project_ventures_store:get_venture(<<"v-test-1">>),
    ?assert(maps:get(status, V) band ?VL_ARCHIVED =/= 0).

list_ventures_active_excludes_archived() ->
    %% Create two ventures
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        (initiated_event_data())#{<<"venture_id">> => <<"v-active">>}
    ),
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        (initiated_event_data())#{<<"venture_id">> => <<"v-archived">>}
    ),
    %% Archive one
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_archived_v1">>,
        #{<<"venture_id">> => <<"v-archived">>}
    ),
    {ok, Active} = project_ventures_store:list_ventures_active(),
    ?assertEqual(1, length(Active)),
    ?assertEqual(<<"v-active">>, maps:get(venture_id, hd(Active))).

full_initiate_query_flow() ->
    %% This is THE test that would have caught the original bug:
    %% initiate a venture, then query the list — it must appear.
    {ok, _, _} = project_event(
        venture_lifecycle_to_ventures,
        <<"venture_initiated_v1">>,
        #{<<"venture_id">> => <<"v-flow-test">>,
          <<"name">> => <<"Flow Test Venture">>,
          <<"brief">> => <<"Testing the full flow">>,
          <<"repos">> => [],
          <<"skills">> => [],
          <<"context_map">> => #{},
          <<"initiated_by">> => <<"tester">>,
          <<"initiated_at">> => erlang:system_time(millisecond)}
    ),
    %% Query: get_ventures_page must return the venture
    {ok, Ventures} = get_ventures_page:get(#{}),
    ?assertEqual(1, length(Ventures)),
    ?assertEqual(<<"Flow Test Venture">>, maps:get(name, hd(Ventures))),
    %% Query: get_venture_by_id must find it
    {ok, V} = get_venture_by_id:get(<<"v-flow-test">>),
    ?assertEqual(<<"Flow Test Venture">>, maps:get(name, V)),
    %% Query: list_ventures_active must include it
    {ok, Active} = project_ventures_store:list_ventures_active(),
    ?assertEqual(1, length(Active)).

project_division_identified() ->
    DivData = #{
        <<"division_id">> => <<"div-1">>,
        <<"venture_id">> => <<"v-test-1">>,
        <<"context_name">> => <<"payments">>,
        <<"description">> => <<"Payment processing">>,
        <<"identified_by">> => <<"user@test">>,
        <<"identified_at">> => 2000
    },
    {ok, _, _} = project_event(
        venture_lifecycle_to_discovered_divisions,
        <<"division_identified_v1">>,
        DivData
    ),
    {ok, Divs} = project_ventures_store:list_divisions_by_venture(<<"v-test-1">>),
    ?assertEqual(1, length(Divs)),
    ?assertEqual(<<"payments">>, maps:get(context_name, hd(Divs))).

count_divisions_works() ->
    lists:foreach(fun(N) ->
        DivId = list_to_binary("div-" ++ integer_to_list(N)),
        {ok, _, _} = project_event(
            venture_lifecycle_to_discovered_divisions,
            <<"division_identified_v1">>,
            #{<<"division_id">> => DivId,
              <<"venture_id">> => <<"v-test-1">>,
              <<"context_name">> => DivId,
              <<"identified_at">> => N * 1000}
        )
    end, [1, 2, 3]),
    ?assertEqual(3, project_ventures_store:count_divisions(<<"v-test-1">>)),
    ?assertEqual(0, project_ventures_store:count_divisions(<<"nonexistent">>)).
