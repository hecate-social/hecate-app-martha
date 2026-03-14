%%% @doc Tests for knowledge_graph_aggregate.
-module(knowledge_graph_aggregate_tests).

-include_lib("eunit/include/eunit.hrl").
-include("knowledge_graph_status.hrl").
-include("knowledge_graph_state.hrl").

%% ===================================================================
%% Helpers
%% ===================================================================

fresh() ->
    knowledge_graph_state:new(<<>>).

initiated_state() ->
    E = #{event_type => <<"knowledge_graph_initiated_v1">>,
          venture_id => <<"v-1">>,
          initiated_at => 1000},
    knowledge_graph_aggregate:apply(fresh(), E).

with_entity(State) ->
    E = #{event_type => <<"entity_recognized_v1">>,
          venture_id => <<"v-1">>,
          entity_id => <<"ent-1">>,
          entity_type => <<"concept">>,
          name => <<"User">>,
          description => <<"A user of the system">>,
          source_agent => <<"visionary">>,
          captured_at => 2000},
    knowledge_graph_aggregate:apply(State, E).

with_insight(State) ->
    E = #{event_type => <<"insight_captured_v1">>,
          venture_id => <<"v-1">>,
          insight_id => <<"ins-1">>,
          content => <<"Users prefer mobile">>,
          source_agent => <<"visionary">>,
          source_session => <<"session-1">>,
          insight_type => <<"preference">>,
          captured_at => 2000},
    knowledge_graph_aggregate:apply(State, E).

%% ===================================================================
%% Initial State
%% ===================================================================

initial_state_test() ->
    S = fresh(),
    ?assertEqual(0, S#knowledge_graph_state.status),
    ?assertEqual(#{}, S#knowledge_graph_state.entities),
    ?assertEqual(#{}, S#knowledge_graph_state.relationships),
    ?assertEqual([], S#knowledge_graph_state.insights),
    ?assertEqual(undefined, S#knowledge_graph_state.venture_id).

%% ===================================================================
%% Execute — Initiate
%% ===================================================================

initiate_fresh_aggregate_test() ->
    Payload = #{command_type => <<"initiate_knowledge_graph">>,
                <<"venture_id">> => <<"v-1">>},
    {ok, [Event]} = knowledge_graph_aggregate:execute(fresh(), Payload),
    ?assertEqual(knowledge_graph_initiated_v1, maps:get(event_type, Event)),
    ?assertEqual(<<"v-1">>, maps:get(venture_id, Event)).

initiate_rejects_non_initiate_on_fresh_test() ->
    Payload = #{command_type => <<"capture_insight">>,
                <<"venture_id">> => <<"v-1">>,
                <<"content">> => <<"test">>},
    {error, knowledge_graph_not_initiated} = knowledge_graph_aggregate:execute(fresh(), Payload).

%% ===================================================================
%% Execute — Capture Insight
%% ===================================================================

capture_insight_on_initiated_test() ->
    Payload = #{command_type => <<"capture_insight">>,
                <<"venture_id">> => <<"v-1">>,
                <<"insight_id">> => <<"ins-1">>,
                <<"content">> => <<"Users prefer mobile">>,
                <<"source_agent">> => <<"visionary">>,
                <<"source_session">> => <<"s-1">>,
                <<"insight_type">> => <<"preference">>},
    {ok, [Event]} = knowledge_graph_aggregate:execute(initiated_state(), Payload),
    ?assertEqual(insight_captured_v1, maps:get(event_type, Event)),
    ?assertEqual(<<"ins-1">>, maps:get(insight_id, Event)).

%% ===================================================================
%% Execute — Recognize Entity
%% ===================================================================

recognize_entity_test() ->
    Payload = #{command_type => <<"recognize_entity">>,
                <<"venture_id">> => <<"v-1">>,
                <<"entity_id">> => <<"ent-1">>,
                <<"entity_type">> => <<"concept">>,
                <<"name">> => <<"User">>,
                <<"description">> => <<"A person">>},
    {ok, [Event]} = knowledge_graph_aggregate:execute(initiated_state(), Payload),
    ?assertEqual(entity_recognized_v1, maps:get(event_type, Event)),
    ?assertEqual(<<"ent-1">>, maps:get(entity_id, Event)).

%% ===================================================================
%% Execute — Draw Relationship
%% ===================================================================

draw_relationship_test() ->
    S = with_entity(initiated_state()),
    Payload = #{command_type => <<"draw_relationship">>,
                <<"venture_id">> => <<"v-1">>,
                <<"rel_id">> => <<"rel-1">>,
                <<"from_entity">> => <<"ent-1">>,
                <<"to_entity">> => <<"ent-2">>,
                <<"rel_type">> => <<"depends_on">>,
                <<"strength">> => 0.8},
    {ok, [Event]} = knowledge_graph_aggregate:execute(S, Payload),
    ?assertEqual(relationship_drawn_v1, maps:get(event_type, Event)).

draw_relationship_rejects_self_relationship_test() ->
    S = with_entity(initiated_state()),
    Payload = #{command_type => <<"draw_relationship">>,
                <<"venture_id">> => <<"v-1">>,
                <<"rel_id">> => <<"rel-1">>,
                <<"from_entity">> => <<"ent-1">>,
                <<"to_entity">> => <<"ent-1">>,
                <<"rel_type">> => <<"depends_on">>},
    {error, self_relationship} = knowledge_graph_aggregate:execute(S, Payload).

%% ===================================================================
%% Execute — Supersede Insight
%% ===================================================================

supersede_insight_test() ->
    S = with_insight(initiated_state()),
    Payload = #{command_type => <<"supersede_insight">>,
                <<"venture_id">> => <<"v-1">>,
                <<"insight_id">> => <<"ins-1">>,
                <<"superseded_by">> => <<"ins-2">>,
                <<"reason">> => <<"Updated preference">>},
    {ok, [Event]} = knowledge_graph_aggregate:execute(S, Payload),
    ?assertEqual(insight_superseded_v1, maps:get(event_type, Event)).

supersede_insight_rejects_missing_test() ->
    S = initiated_state(),
    Payload = #{command_type => <<"supersede_insight">>,
                <<"venture_id">> => <<"v-1">>,
                <<"insight_id">> => <<"nonexistent">>,
                <<"superseded_by">> => <<"ins-2">>},
    {error, insight_not_found} = knowledge_graph_aggregate:execute(S, Payload).

supersede_insight_rejects_self_supersede_test() ->
    S = with_insight(initiated_state()),
    Payload = #{command_type => <<"supersede_insight">>,
                <<"venture_id">> => <<"v-1">>,
                <<"insight_id">> => <<"ins-1">>,
                <<"superseded_by">> => <<"ins-1">>},
    {error, cannot_supersede_self} = knowledge_graph_aggregate:execute(S, Payload).

%% ===================================================================
%% Execute — Archive
%% ===================================================================

archive_initiated_test() ->
    Payload = #{command_type => <<"archive_knowledge_graph">>,
                <<"venture_id">> => <<"v-1">>,
                <<"reason">> => <<"Done">>},
    {ok, [Event]} = knowledge_graph_aggregate:execute(initiated_state(), Payload),
    ?assertEqual(knowledge_graph_archived_v1, maps:get(event_type, Event)).

archive_blocks_further_commands_test() ->
    ArchivedEvent = #{event_type => <<"knowledge_graph_archived_v1">>,
                      venture_id => <<"v-1">>,
                      reason => <<"Done">>},
    S = knowledge_graph_aggregate:apply(initiated_state(), ArchivedEvent),
    Payload = #{command_type => <<"capture_insight">>,
                <<"venture_id">> => <<"v-1">>,
                <<"content">> => <<"too late">>},
    {error, knowledge_graph_archived} = knowledge_graph_aggregate:execute(S, Payload).

%% ===================================================================
%% Apply — State Transitions
%% ===================================================================

apply_initiated_sets_status_test() ->
    S = initiated_state(),
    ?assertNotEqual(0, S#knowledge_graph_state.status band ?KG_INITIATED),
    ?assertNotEqual(0, S#knowledge_graph_state.status band ?KG_ACTIVE),
    ?assertEqual(<<"v-1">>, S#knowledge_graph_state.venture_id),
    ?assertEqual(1000, S#knowledge_graph_state.initiated_at).

apply_insight_captured_adds_to_list_test() ->
    S = with_insight(initiated_state()),
    ?assertEqual(1, length(S#knowledge_graph_state.insights)),
    [Insight] = S#knowledge_graph_state.insights,
    ?assertEqual(<<"ins-1">>, maps:get(insight_id, Insight)),
    ?assertEqual(<<"Users prefer mobile">>, maps:get(content, Insight)),
    ?assertEqual(false, maps:get(superseded, Insight)).

apply_entity_recognized_adds_to_map_test() ->
    S = with_entity(initiated_state()),
    ?assertEqual(1, maps:size(S#knowledge_graph_state.entities)),
    #{<<"ent-1">> := Entity} = S#knowledge_graph_state.entities,
    ?assertEqual(<<"User">>, maps:get(name, Entity)),
    ?assertEqual(<<"concept">>, maps:get(entity_type, Entity)).

apply_relationship_drawn_adds_to_map_test() ->
    E = #{event_type => <<"relationship_drawn_v1">>,
          venture_id => <<"v-1">>,
          rel_id => <<"rel-1">>,
          from_entity => <<"ent-1">>,
          to_entity => <<"ent-2">>,
          rel_type => <<"depends_on">>,
          strength => 0.8,
          drawn_at => 3000},
    S = knowledge_graph_aggregate:apply(initiated_state(), E),
    ?assertEqual(1, maps:size(S#knowledge_graph_state.relationships)),
    #{<<"rel-1">> := Rel} = S#knowledge_graph_state.relationships,
    ?assertEqual(<<"depends_on">>, maps:get(rel_type, Rel)).

apply_insight_superseded_marks_insight_test() ->
    S0 = with_insight(initiated_state()),
    E = #{event_type => <<"insight_superseded_v1">>,
          venture_id => <<"v-1">>,
          insight_id => <<"ins-1">>,
          superseded_by => <<"ins-2">>,
          superseded_at => 5000},
    S = knowledge_graph_aggregate:apply(S0, E),
    [Insight] = S#knowledge_graph_state.insights,
    ?assertEqual(true, maps:get(superseded, Insight)),
    ?assertEqual(<<"ins-2">>, maps:get(superseded_by, Insight)).

apply_archived_sets_flag_test() ->
    E = #{event_type => <<"knowledge_graph_archived_v1">>,
          venture_id => <<"v-1">>},
    S = knowledge_graph_aggregate:apply(initiated_state(), E),
    ?assertNotEqual(0, S#knowledge_graph_state.status band ?KG_ARCHIVED).

%% ===================================================================
%% Apply — Binary Keys (stored events)
%% ===================================================================

apply_binary_keys_initiated_test() ->
    E = #{event_type => <<"knowledge_graph_initiated_v1">>,
          <<"venture_id">> => <<"v-bin">>,
          <<"initiated_at">> => 9999},
    S = knowledge_graph_aggregate:apply(fresh(), E),
    ?assertEqual(<<"v-bin">>, S#knowledge_graph_state.venture_id),
    ?assertEqual(9999, S#knowledge_graph_state.initiated_at).

apply_binary_keys_insight_captured_test() ->
    E = #{event_type => <<"insight_captured_v1">>,
          <<"venture_id">> => <<"v-1">>,
          <<"insight_id">> => <<"ins-b">>,
          <<"content">> => <<"Binary content">>,
          <<"source_agent">> => <<"agent">>,
          <<"source_session">> => <<"sess">>,
          <<"insight_type">> => <<"decision">>,
          <<"captured_at">> => 7777},
    S = knowledge_graph_aggregate:apply(initiated_state(), E),
    ?assertEqual(1, length(S#knowledge_graph_state.insights)),
    [I] = S#knowledge_graph_state.insights,
    ?assertEqual(<<"ins-b">>, maps:get(insight_id, I)).

%% ===================================================================
%% Bounded Insights List
%% ===================================================================

insights_bounded_at_max_test() ->
    S0 = initiated_state(),
    %% Add 502 insights — should be capped at 500
    S = lists:foldl(fun(N, Acc) ->
        Id = <<"ins-", (integer_to_binary(N))/binary>>,
        E = #{event_type => <<"insight_captured_v1">>,
              insight_id => Id,
              content => <<"content">>,
              source_agent => undefined,
              source_session => undefined,
              insight_type => <<"general">>,
              captured_at => N},
        knowledge_graph_aggregate:apply(Acc, E)
    end, S0, lists:seq(1, 502)),
    ?assertEqual(500, length(S#knowledge_graph_state.insights)).

%% ===================================================================
%% Command Routing — Atom Keys
%% ===================================================================

execute_with_atom_command_type_test() ->
    Payload = #{command_type => initiate_knowledge_graph,
                venture_id => <<"v-atom">>},
    {ok, [Event]} = knowledge_graph_aggregate:execute(fresh(), Payload),
    ?assertEqual(knowledge_graph_initiated_v1, maps:get(event_type, Event)).

execute_unknown_command_on_initiated_test() ->
    Payload = #{command_type => <<"do_magic">>,
                <<"venture_id">> => <<"v-1">>},
    {error, unknown_command} = knowledge_graph_aggregate:execute(initiated_state(), Payload).

%% ===================================================================
%% Flag Map
%% ===================================================================

flag_map_test() ->
    FM = knowledge_graph_aggregate:flag_map(),
    ?assert(is_map(FM)),
    ?assert(maps:is_key(?KG_INITIATED, FM)),
    ?assert(maps:is_key(?KG_ACTIVE, FM)),
    ?assert(maps:is_key(?KG_ARCHIVED, FM)).
