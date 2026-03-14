%%% @doc Tests for knowledge graph command and handler modules.
-module(knowledge_graph_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% initiate_knowledge_graph_v1
%% ===================================================================

initiate_new_test() ->
    {ok, Cmd} = initiate_knowledge_graph_v1:new(#{venture_id => <<"v-1">>}),
    ?assertEqual(<<"v-1">>, initiate_knowledge_graph_v1:get_venture_id(Cmd)).

initiate_new_missing_fields_test() ->
    {error, missing_required_fields} = initiate_knowledge_graph_v1:new(#{}).

initiate_from_map_atom_keys_test() ->
    {ok, Cmd} = initiate_knowledge_graph_v1:from_map(#{venture_id => <<"v-1">>}),
    ?assertEqual(<<"v-1">>, initiate_knowledge_graph_v1:get_venture_id(Cmd)).

initiate_from_map_binary_keys_test() ->
    {ok, Cmd} = initiate_knowledge_graph_v1:from_map(#{<<"venture_id">> => <<"v-1">>}),
    ?assertEqual(<<"v-1">>, initiate_knowledge_graph_v1:get_venture_id(Cmd)).

initiate_roundtrip_test() ->
    {ok, Cmd} = initiate_knowledge_graph_v1:new(#{venture_id => <<"v-rt">>}),
    Map = initiate_knowledge_graph_v1:to_map(Cmd),
    {ok, Cmd2} = initiate_knowledge_graph_v1:from_map(Map),
    ?assertEqual(<<"v-rt">>, initiate_knowledge_graph_v1:get_venture_id(Cmd2)),
    ?assertEqual(initiate_knowledge_graph_v1, maps:get(command_type, Map)).

initiate_validate_ok_test() ->
    {ok, Cmd} = initiate_knowledge_graph_v1:new(#{venture_id => <<"v-1">>}),
    {ok, _} = initiate_knowledge_graph_v1:validate(Cmd).

initiate_generate_id_test() ->
    Id = initiate_knowledge_graph_v1:generate_id(),
    ?assertNotEqual(nomatch, binary:match(Id, <<"kg-">>)).

%% ===================================================================
%% maybe_initiate_knowledge_graph
%% ===================================================================

maybe_initiate_handler_test() ->
    {ok, Cmd} = initiate_knowledge_graph_v1:new(#{venture_id => <<"v-1">>}),
    {ok, [Event]} = maybe_initiate_knowledge_graph:handle(Cmd),
    Map = knowledge_graph_initiated_v1:to_map(Event),
    ?assertEqual(knowledge_graph_initiated_v1, maps:get(event_type, Map)),
    ?assertEqual(<<"v-1">>, maps:get(venture_id, Map)).

%% ===================================================================
%% capture_insight_v1
%% ===================================================================

capture_new_test() ->
    {ok, Cmd} = capture_insight_v1:new(#{
        venture_id => <<"v-1">>,
        content => <<"Users prefer mobile">>
    }),
    ?assertEqual(<<"v-1">>, capture_insight_v1:get_venture_id(Cmd)),
    ?assertEqual(<<"Users prefer mobile">>, capture_insight_v1:get_content(Cmd)),
    ?assertEqual(<<"general">>, capture_insight_v1:get_insight_type(Cmd)).

capture_new_with_all_fields_test() ->
    {ok, Cmd} = capture_insight_v1:new(#{
        venture_id => <<"v-1">>,
        insight_id => <<"ins-custom">>,
        content => <<"Content">>,
        source_agent => <<"visionary">>,
        source_session => <<"sess-1">>,
        insight_type => <<"preference">>
    }),
    ?assertEqual(<<"ins-custom">>, capture_insight_v1:get_insight_id(Cmd)),
    ?assertEqual(<<"visionary">>, capture_insight_v1:get_source_agent(Cmd)),
    ?assertEqual(<<"sess-1">>, capture_insight_v1:get_source_session(Cmd)),
    ?assertEqual(<<"preference">>, capture_insight_v1:get_insight_type(Cmd)).

capture_from_map_binary_keys_test() ->
    {ok, Cmd} = capture_insight_v1:from_map(#{
        <<"venture_id">> => <<"v-1">>,
        <<"content">> => <<"Binary content">>,
        <<"insight_id">> => <<"ins-bin">>
    }),
    ?assertEqual(<<"v-1">>, capture_insight_v1:get_venture_id(Cmd)),
    ?assertEqual(<<"ins-bin">>, capture_insight_v1:get_insight_id(Cmd)).

capture_roundtrip_test() ->
    {ok, Cmd} = capture_insight_v1:new(#{
        venture_id => <<"v-1">>,
        content => <<"round trip">>
    }),
    Map = capture_insight_v1:to_map(Cmd),
    {ok, Cmd2} = capture_insight_v1:from_map(Map),
    ?assertEqual(capture_insight_v1:get_content(Cmd), capture_insight_v1:get_content(Cmd2)).

capture_validate_empty_content_test() ->
    {ok, Cmd} = capture_insight_v1:from_map(#{
        <<"venture_id">> => <<"v-1">>,
        <<"content">> => <<>>
    }),
    {error, invalid_content} = capture_insight_v1:validate(Cmd).

%% ===================================================================
%% maybe_capture_insight
%% ===================================================================

maybe_capture_insight_handler_test() ->
    {ok, Cmd} = capture_insight_v1:new(#{
        venture_id => <<"v-1">>,
        content => <<"Users prefer mobile">>
    }),
    {ok, [Event]} = maybe_capture_insight:handle(Cmd),
    Map = insight_captured_v1:to_map(Event),
    ?assertEqual(insight_captured_v1, maps:get(event_type, Map)),
    ?assertEqual(<<"Users prefer mobile">>, maps:get(content, Map)).

%% ===================================================================
%% recognize_entity_v1
%% ===================================================================

recognize_new_test() ->
    {ok, Cmd} = recognize_entity_v1:new(#{
        venture_id => <<"v-1">>,
        name => <<"User">>,
        entity_type => <<"concept">>
    }),
    ?assertEqual(<<"User">>, recognize_entity_v1:get_name(Cmd)),
    ?assertEqual(<<"concept">>, recognize_entity_v1:get_entity_type(Cmd)).

recognize_from_map_binary_keys_test() ->
    {ok, Cmd} = recognize_entity_v1:from_map(#{
        <<"venture_id">> => <<"v-1">>,
        <<"name">> => <<"User">>,
        <<"entity_type">> => <<"concept">>
    }),
    ?assertEqual(<<"User">>, recognize_entity_v1:get_name(Cmd)).

recognize_validate_empty_name_test() ->
    {ok, Cmd} = recognize_entity_v1:from_map(#{
        <<"venture_id">> => <<"v-1">>,
        <<"name">> => <<>>,
        <<"entity_type">> => <<"concept">>
    }),
    {error, invalid_name} = recognize_entity_v1:validate(Cmd).

recognize_roundtrip_test() ->
    {ok, Cmd} = recognize_entity_v1:new(#{
        venture_id => <<"v-1">>,
        entity_id => <<"ent-rt">>,
        entity_type => <<"system">>,
        name => <<"Auth Service">>
    }),
    Map = recognize_entity_v1:to_map(Cmd),
    {ok, Cmd2} = recognize_entity_v1:from_map(Map),
    ?assertEqual(recognize_entity_v1:get_name(Cmd), recognize_entity_v1:get_name(Cmd2)).

%% ===================================================================
%% maybe_recognize_entity
%% ===================================================================

maybe_recognize_entity_handler_test() ->
    {ok, Cmd} = recognize_entity_v1:new(#{
        venture_id => <<"v-1">>,
        name => <<"User">>,
        entity_type => <<"concept">>
    }),
    {ok, [Event]} = maybe_recognize_entity:handle(Cmd, #{entities => #{}}),
    Map = entity_recognized_v1:to_map(Event),
    ?assertEqual(entity_recognized_v1, maps:get(event_type, Map)).

%% ===================================================================
%% draw_relationship_v1
%% ===================================================================

draw_new_test() ->
    {ok, Cmd} = draw_relationship_v1:new(#{
        venture_id => <<"v-1">>,
        from_entity => <<"ent-1">>,
        to_entity => <<"ent-2">>,
        rel_type => <<"depends_on">>
    }),
    ?assertEqual(<<"ent-1">>, draw_relationship_v1:get_from_entity(Cmd)),
    ?assertEqual(<<"ent-2">>, draw_relationship_v1:get_to_entity(Cmd)),
    ?assertEqual(1.0, draw_relationship_v1:get_strength(Cmd)).

draw_validate_self_relationship_test() ->
    {ok, Cmd} = draw_relationship_v1:new(#{
        venture_id => <<"v-1">>,
        from_entity => <<"ent-1">>,
        to_entity => <<"ent-1">>,
        rel_type => <<"depends_on">>
    }),
    {error, self_relationship} = draw_relationship_v1:validate(Cmd).

draw_roundtrip_test() ->
    {ok, Cmd} = draw_relationship_v1:new(#{
        venture_id => <<"v-1">>,
        rel_id => <<"rel-rt">>,
        from_entity => <<"ent-1">>,
        to_entity => <<"ent-2">>,
        rel_type => <<"depends_on">>,
        strength => 0.5
    }),
    Map = draw_relationship_v1:to_map(Cmd),
    {ok, Cmd2} = draw_relationship_v1:from_map(Map),
    ?assertEqual(0.5, draw_relationship_v1:get_strength(Cmd2)),
    ?assertEqual(<<"rel-rt">>, draw_relationship_v1:get_rel_id(Cmd2)).

%% ===================================================================
%% maybe_draw_relationship
%% ===================================================================

maybe_draw_relationship_handler_test() ->
    {ok, Cmd} = draw_relationship_v1:new(#{
        venture_id => <<"v-1">>,
        from_entity => <<"ent-1">>,
        to_entity => <<"ent-2">>,
        rel_type => <<"depends_on">>
    }),
    {ok, [Event]} = maybe_draw_relationship:handle(Cmd, #{entities => #{}}),
    Map = relationship_drawn_v1:to_map(Event),
    ?assertEqual(relationship_drawn_v1, maps:get(event_type, Map)).

%% ===================================================================
%% supersede_insight_v1
%% ===================================================================

supersede_new_test() ->
    {ok, Cmd} = supersede_insight_v1:new(#{
        venture_id => <<"v-1">>,
        insight_id => <<"ins-1">>,
        superseded_by => <<"ins-2">>,
        reason => <<"Updated">>
    }),
    ?assertEqual(<<"ins-1">>, supersede_insight_v1:get_insight_id(Cmd)),
    ?assertEqual(<<"ins-2">>, supersede_insight_v1:get_superseded_by(Cmd)),
    ?assertEqual(<<"Updated">>, supersede_insight_v1:get_reason(Cmd)).

supersede_validate_self_test() ->
    {ok, Cmd} = supersede_insight_v1:new(#{
        venture_id => <<"v-1">>,
        insight_id => <<"ins-1">>,
        superseded_by => <<"ins-1">>
    }),
    {error, cannot_supersede_self} = supersede_insight_v1:validate(Cmd).

supersede_roundtrip_test() ->
    {ok, Cmd} = supersede_insight_v1:new(#{
        venture_id => <<"v-1">>,
        insight_id => <<"ins-1">>,
        superseded_by => <<"ins-2">>
    }),
    Map = supersede_insight_v1:to_map(Cmd),
    {ok, Cmd2} = supersede_insight_v1:from_map(Map),
    ?assertEqual(supersede_insight_v1:get_insight_id(Cmd),
                 supersede_insight_v1:get_insight_id(Cmd2)).

%% ===================================================================
%% maybe_supersede_insight
%% ===================================================================

maybe_supersede_insight_found_test() ->
    {ok, Cmd} = supersede_insight_v1:new(#{
        venture_id => <<"v-1">>,
        insight_id => <<"ins-1">>,
        superseded_by => <<"ins-2">>
    }),
    Context = #{insights => [#{insight_id => <<"ins-1">>, content => <<"old">>}]},
    {ok, [Event]} = maybe_supersede_insight:handle(Cmd, Context),
    Map = insight_superseded_v1:to_map(Event),
    ?assertEqual(insight_superseded_v1, maps:get(event_type, Map)).

maybe_supersede_insight_not_found_test() ->
    {ok, Cmd} = supersede_insight_v1:new(#{
        venture_id => <<"v-1">>,
        insight_id => <<"missing">>,
        superseded_by => <<"ins-2">>
    }),
    Context = #{insights => [#{insight_id => <<"ins-1">>, content => <<"old">>}]},
    {error, insight_not_found} = maybe_supersede_insight:handle(Cmd, Context).

%% ===================================================================
%% archive_knowledge_graph_v1
%% ===================================================================

archive_new_test() ->
    {ok, Cmd} = archive_knowledge_graph_v1:new(#{
        venture_id => <<"v-1">>,
        reason => <<"project complete">>
    }),
    ?assertEqual(<<"v-1">>, archive_knowledge_graph_v1:get_venture_id(Cmd)),
    ?assertEqual(<<"project complete">>, archive_knowledge_graph_v1:get_reason(Cmd)).

archive_from_map_binary_keys_test() ->
    {ok, Cmd} = archive_knowledge_graph_v1:from_map(#{
        <<"venture_id">> => <<"v-1">>,
        <<"reason">> => <<"done">>
    }),
    ?assertEqual(<<"done">>, archive_knowledge_graph_v1:get_reason(Cmd)).

archive_roundtrip_test() ->
    {ok, Cmd} = archive_knowledge_graph_v1:new(#{venture_id => <<"v-1">>}),
    Map = archive_knowledge_graph_v1:to_map(Cmd),
    {ok, Cmd2} = archive_knowledge_graph_v1:from_map(Map),
    ?assertEqual(archive_knowledge_graph_v1:get_venture_id(Cmd),
                 archive_knowledge_graph_v1:get_venture_id(Cmd2)).

%% ===================================================================
%% maybe_archive_knowledge_graph
%% ===================================================================

maybe_archive_handler_test() ->
    {ok, Cmd} = archive_knowledge_graph_v1:new(#{
        venture_id => <<"v-1">>,
        reason => <<"done">>
    }),
    {ok, [Event]} = maybe_archive_knowledge_graph:handle(Cmd),
    Map = knowledge_graph_archived_v1:to_map(Event),
    ?assertEqual(knowledge_graph_archived_v1, maps:get(event_type, Map)).
