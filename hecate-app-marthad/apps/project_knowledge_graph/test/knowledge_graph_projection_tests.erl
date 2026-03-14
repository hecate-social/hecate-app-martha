%%% @doc Tests for knowledge_graph_lifecycle_to_knowledge_graph projection.
-module(knowledge_graph_projection_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("guide_knowledge_graph/include/knowledge_graph_status.hrl").

-define(TABLE, project_knowledge_graph).
-define(VID, <<"v-test">>).

%% ===================================================================
%% Setup / Teardown
%% ===================================================================

setup() ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    RM.

cleanup(RM) ->
    catch ets:delete(?TABLE),
    RM.

%% ===================================================================
%% Helpers
%% ===================================================================

wrap_event(Data) ->
    #{data => Data, event_type => maps:get(event_type, Data, maps:get(<<"event_type">>, Data, undefined))}.

project(Data, State, RM) ->
    knowledge_graph_lifecycle_to_knowledge_graph:project(wrap_event(Data), #{}, State, RM).

make_initiated() ->
    #{event_type => <<"knowledge_graph_initiated_v1">>,
      venture_id => ?VID,
      initiated_at => 1000}.

make_insight(InsightId) ->
    #{event_type => <<"insight_captured_v1">>,
      venture_id => ?VID,
      insight_id => InsightId,
      content => <<"Test insight">>,
      source_agent => <<"visionary">>,
      source_session => <<"sess-1">>,
      insight_type => <<"preference">>,
      captured_at => 2000}.

make_entity(EntityId) ->
    #{event_type => <<"entity_recognized_v1">>,
      venture_id => ?VID,
      entity_id => EntityId,
      entity_type => <<"concept">>,
      name => <<"Test Entity">>,
      description => <<"A test entity">>,
      source_agent => <<"architect">>,
      captured_at => 3000}.

make_relationship(RelId) ->
    #{event_type => <<"relationship_drawn_v1">>,
      venture_id => ?VID,
      rel_id => RelId,
      from_entity => <<"ent-1">>,
      to_entity => <<"ent-2">>,
      rel_type => <<"depends_on">>,
      strength => 0.8,
      drawn_at => 4000}.

make_supersede(InsightId) ->
    #{event_type => <<"insight_superseded_v1">>,
      venture_id => ?VID,
      insight_id => InsightId,
      superseded_by => <<"ins-new">>,
      superseded_at => 5000}.

make_archived() ->
    #{event_type => <<"knowledge_graph_archived_v1">>,
      venture_id => ?VID,
      reason => <<"done">>}.

%% ===================================================================
%% Interested In
%% ===================================================================

interested_in_returns_all_events_test() ->
    Events = knowledge_graph_lifecycle_to_knowledge_graph:interested_in(),
    ?assertEqual(6, length(Events)),
    ?assert(lists:member(<<"knowledge_graph_initiated_v1">>, Events)),
    ?assert(lists:member(<<"insight_captured_v1">>, Events)),
    ?assert(lists:member(<<"entity_recognized_v1">>, Events)),
    ?assert(lists:member(<<"relationship_drawn_v1">>, Events)),
    ?assert(lists:member(<<"insight_superseded_v1">>, Events)),
    ?assert(lists:member(<<"knowledge_graph_archived_v1">>, Events)).

%% ===================================================================
%% Initiated
%% ===================================================================

initiated_creates_graph_test() ->
    RM = setup(),
    try
        {ok, _S, RM1} = project(make_initiated(), #{}, RM),
        {ok, G} = evoq_read_model:get(?VID, RM1),
        ?assertEqual(?VID, maps:get(venture_id, G)),
        ?assertNotEqual(0, maps:get(status, G) band ?KG_INITIATED),
        ?assertNotEqual(0, maps:get(status, G) band ?KG_ACTIVE),
        ?assertEqual(#{}, maps:get(entities, G)),
        ?assertEqual(#{}, maps:get(relationships, G)),
        ?assertEqual([], maps:get(insights, G)),
        ?assertEqual(0, maps:get(entity_count, G)),
        ?assertEqual(0, maps:get(relationship_count, G)),
        ?assertEqual(0, maps:get(insight_count, G)),
        ?assertEqual(1000, maps:get(initiated_at, G)),
        ?assert(is_binary(maps:get(status_label, G)))
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Insight Captured
%% ===================================================================

insight_captured_adds_to_graph_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, _S2, RM2} = project(make_insight(<<"ins-1">>), S1, RM1),
        {ok, G} = evoq_read_model:get(?VID, RM2),
        ?assertEqual(1, maps:get(insight_count, G)),
        [Insight] = maps:get(insights, G),
        ?assertEqual(<<"ins-1">>, maps:get(insight_id, Insight)),
        ?assertEqual(<<"Test insight">>, maps:get(content, Insight)),
        ?assertEqual(false, maps:get(superseded, Insight))
    after
        cleanup(RM)
    end.

multiple_insights_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, S2, RM2} = project(make_insight(<<"ins-1">>), S1, RM1),
        {ok, _S3, RM3} = project(make_insight(<<"ins-2">>), S2, RM2),
        {ok, G} = evoq_read_model:get(?VID, RM3),
        ?assertEqual(2, maps:get(insight_count, G)),
        ?assertEqual(2, length(maps:get(insights, G)))
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Entity Recognized
%% ===================================================================

entity_recognized_adds_to_graph_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, _S2, RM2} = project(make_entity(<<"ent-1">>), S1, RM1),
        {ok, G} = evoq_read_model:get(?VID, RM2),
        ?assertEqual(1, maps:get(entity_count, G)),
        Entities = maps:get(entities, G),
        #{<<"ent-1">> := Entity} = Entities,
        ?assertEqual(<<"Test Entity">>, maps:get(name, Entity)),
        ?assertEqual(<<"concept">>, maps:get(entity_type, Entity))
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Relationship Drawn
%% ===================================================================

relationship_drawn_adds_to_graph_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, _S2, RM2} = project(make_relationship(<<"rel-1">>), S1, RM1),
        {ok, G} = evoq_read_model:get(?VID, RM2),
        ?assertEqual(1, maps:get(relationship_count, G)),
        Rels = maps:get(relationships, G),
        #{<<"rel-1">> := Rel} = Rels,
        ?assertEqual(<<"depends_on">>, maps:get(rel_type, Rel)),
        ?assertEqual(0.8, maps:get(strength, Rel))
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Insight Superseded
%% ===================================================================

insight_superseded_marks_insight_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, S2, RM2} = project(make_insight(<<"ins-1">>), S1, RM1),
        {ok, _S3, RM3} = project(make_supersede(<<"ins-1">>), S2, RM2),
        {ok, G} = evoq_read_model:get(?VID, RM3),
        [Insight] = maps:get(insights, G),
        ?assertEqual(true, maps:get(superseded, Insight)),
        ?assertEqual(<<"ins-new">>, maps:get(superseded_by, Insight)),
        ?assertEqual(5000, maps:get(superseded_at, Insight))
    after
        cleanup(RM)
    end.

supersede_only_marks_matching_insight_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, S2, RM2} = project(make_insight(<<"ins-1">>), S1, RM1),
        {ok, S3, RM3} = project(make_insight(<<"ins-2">>), S2, RM2),
        {ok, _S4, RM4} = project(make_supersede(<<"ins-1">>), S3, RM3),
        {ok, G} = evoq_read_model:get(?VID, RM4),
        Insights = maps:get(insights, G),
        Ins1 = lists:filter(fun(I) -> maps:get(insight_id, I) =:= <<"ins-1">> end, Insights),
        Ins2 = lists:filter(fun(I) -> maps:get(insight_id, I) =:= <<"ins-2">> end, Insights),
        [I1] = Ins1,
        [I2] = Ins2,
        ?assertEqual(true, maps:get(superseded, I1)),
        ?assertEqual(false, maps:get(superseded, I2))
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Archived
%% ===================================================================

archived_sets_status_flag_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        {ok, _S2, RM2} = project(make_archived(), S1, RM1),
        {ok, G} = evoq_read_model:get(?VID, RM2),
        ?assertNotEqual(0, maps:get(status, G) band ?KG_ARCHIVED),
        ?assert(is_binary(maps:get(status_label, G)))
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Edge Cases
%% ===================================================================

event_before_initiate_skips_test() ->
    RM = setup(),
    try
        {skip, _S, _RM1} = project(make_insight(<<"ins-1">>), #{}, RM)
    after
        cleanup(RM)
    end.

unknown_event_type_passes_through_test() ->
    RM = setup(),
    try
        Data = #{event_type => <<"some_unknown_v1">>, venture_id => ?VID},
        {ok, _S, _RM1} = project(Data, #{}, RM)
    after
        cleanup(RM)
    end.

%% ===================================================================
%% Binary Key Events (stored events from ReckonDB)
%% ===================================================================

binary_key_initiated_test() ->
    RM = setup(),
    try
        Data = #{<<"event_type">> => <<"knowledge_graph_initiated_v1">>,
                 <<"venture_id">> => ?VID,
                 <<"initiated_at">> => 9999},
        {ok, _S, RM1} = project(Data, #{}, RM),
        {ok, G} = evoq_read_model:get(?VID, RM1),
        ?assertEqual(?VID, maps:get(venture_id, G)),
        ?assertEqual(9999, maps:get(initiated_at, G))
    after
        cleanup(RM)
    end.

binary_key_entity_recognized_test() ->
    RM = setup(),
    try
        {ok, S1, RM1} = project(make_initiated(), #{}, RM),
        Data = #{<<"event_type">> => <<"entity_recognized_v1">>,
                 <<"venture_id">> => ?VID,
                 <<"entity_id">> => <<"ent-bin">>,
                 <<"entity_type">> => <<"system">>,
                 <<"name">> => <<"Auth">>,
                 <<"description">> => <<"Auth system">>,
                 <<"source_agent">> => <<"architect">>,
                 <<"captured_at">> => 8888},
        {ok, _S2, RM2} = project(Data, S1, RM1),
        {ok, G} = evoq_read_model:get(?VID, RM2),
        ?assertEqual(1, maps:get(entity_count, G)),
        #{<<"ent-bin">> := Entity} = maps:get(entities, G),
        ?assertEqual(<<"Auth">>, maps:get(name, Entity))
    after
        cleanup(RM)
    end.
