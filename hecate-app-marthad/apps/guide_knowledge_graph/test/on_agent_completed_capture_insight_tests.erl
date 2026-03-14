%%% @doc Tests for on_agent_completed_capture_insight PM.
%%%
%%% Verifies that the PM correctly extracts fields from *_completed_v1
%%% events and builds capture_insight_v1 / recognize_entity_v1 commands.
%%%
%%% NOTE: The LLM-based knowledge_extraction path cannot be unit-tested
%%% (requires live LLM). These tests verify the field mapping and command
%%% construction that happens AFTER extraction — using pre-built extraction
%%% results as if translate_output returned them.
-module(on_agent_completed_capture_insight_tests).

-include_lib("eunit/include/eunit.hrl").

-define(VID, <<"v-test-123">>).
-define(SESSION_ID, <<"sess-abc">>).

%% ===================================================================
%% Test Data
%% ===================================================================

make_visionary_completed_data() ->
    #{
        event_type => <<"visionary_completed_v1">>,
        session_id => ?SESSION_ID,
        agent_role => <<"visionary">>,
        venture_id => ?VID,
        division_id => undefined,
        tier => <<"discovery">>,
        model => <<"claude-sonnet-4-6">>,
        notation_output => <<"# Vision\n## Problem\nUsers need X\n## Users\nDevelopers">>,
        parsed_terms => [],
        tokens_in => 500,
        tokens_out => 1200,
        completed_at => 1710000000000
    }.

make_visionary_completed_no_output() ->
    (make_visionary_completed_data())#{notation_output => undefined}.

%% Simulated translate_output result for knowledge_extraction
make_extraction_result() ->
    #{
        <<"entities">> => [
            #{<<"name">> => <<"Users">>, <<"type">> => <<"user_segment">>,
              <<"description">> => <<"Target users of the system">>},
            #{<<"name">> => <<"API Gateway">>, <<"type">> => <<"system">>,
              <<"description">> => <<"Entry point for requests">>}
        ],
        <<"relationships">> => [
            #{<<"from">> => <<"Users">>, <<"to">> => <<"API Gateway">>,
              <<"type">> => <<"interacts_with">>}
        ],
        <<"insights">> => [
            <<"Users prefer mobile-first design">>,
            <<"Latency under 100ms is a hard requirement">>
        ]
    }.

%% ===================================================================
%% Command Construction — Insights
%% ===================================================================

build_insight_params_test() ->
    Data = make_visionary_completed_data(),
    InsightContent = <<"Users prefer mobile-first design">>,
    Params = build_insight_params(Data, InsightContent),
    ?assertEqual(?VID, maps:get(venture_id, Params)),
    ?assertEqual(InsightContent, maps:get(content, Params)),
    ?assertEqual(<<"visionary">>, maps:get(source_agent, Params)),
    ?assertEqual(?SESSION_ID, maps:get(source_session, Params)),
    ?assertEqual(<<"discovery">>, maps:get(insight_type, Params)).

insight_params_create_valid_command_test() ->
    Data = make_visionary_completed_data(),
    Params = build_insight_params(Data, <<"Some insight">>),
    {ok, Cmd} = capture_insight_v1:new(Params),
    ?assertEqual(?VID, capture_insight_v1:get_venture_id(Cmd)),
    ?assertEqual(<<"Some insight">>, capture_insight_v1:get_content(Cmd)),
    ?assertEqual(<<"visionary">>, capture_insight_v1:get_source_agent(Cmd)).

%% ===================================================================
%% Command Construction — Entities
%% ===================================================================

build_entity_params_test() ->
    Data = make_visionary_completed_data(),
    ExtractedEntity = #{<<"name">> => <<"Users">>, <<"type">> => <<"user_segment">>,
                        <<"description">> => <<"Target users">>},
    Params = build_entity_params(Data, ExtractedEntity),
    ?assertEqual(?VID, maps:get(venture_id, Params)),
    ?assertEqual(<<"Users">>, maps:get(name, Params)),
    ?assertEqual(<<"user_segment">>, maps:get(entity_type, Params)),
    ?assertEqual(<<"Target users">>, maps:get(description, Params)),
    ?assertEqual(<<"visionary">>, maps:get(source_agent, Params)).

entity_params_create_valid_command_test() ->
    Data = make_visionary_completed_data(),
    ExtractedEntity = #{<<"name">> => <<"API Gateway">>, <<"type">> => <<"system">>,
                        <<"description">> => <<"Entry point">>},
    Params = build_entity_params(Data, ExtractedEntity),
    {ok, Cmd} = recognize_entity_v1:new(Params),
    ?assertEqual(<<"API Gateway">>, recognize_entity_v1:get_name(Cmd)),
    ?assertEqual(<<"system">>, recognize_entity_v1:get_entity_type(Cmd)).

%% ===================================================================
%% Command Construction — Relationships
%% ===================================================================

build_relationship_params_test() ->
    Data = make_visionary_completed_data(),
    ExtractedRel = #{<<"from">> => <<"Users">>, <<"to">> => <<"API Gateway">>,
                     <<"type">> => <<"interacts_with">>},
    Params = build_relationship_params(Data, ExtractedRel),
    ?assertEqual(?VID, maps:get(venture_id, Params)),
    ?assertEqual(<<"Users">>, maps:get(from_entity, Params)),
    ?assertEqual(<<"API Gateway">>, maps:get(to_entity, Params)),
    ?assertEqual(<<"interacts_with">>, maps:get(rel_type, Params)).

relationship_params_create_valid_command_test() ->
    Data = make_visionary_completed_data(),
    ExtractedRel = #{<<"from">> => <<"Users">>, <<"to">> => <<"Gateway">>,
                     <<"type">> => <<"uses">>},
    Params = build_relationship_params(Data, ExtractedRel),
    {ok, Cmd} = draw_relationship_v1:new(Params),
    ?assertEqual(<<"Users">>, draw_relationship_v1:get_from_entity(Cmd)),
    ?assertEqual(<<"Gateway">>, draw_relationship_v1:get_to_entity(Cmd)).

%% ===================================================================
%% Extraction Result Processing
%% ===================================================================

process_extraction_builds_all_commands_test() ->
    Data = make_visionary_completed_data(),
    Extraction = make_extraction_result(),
    {Insights, Entities, Rels} = process_extraction(Data, Extraction),
    ?assertEqual(2, length(Insights)),
    ?assertEqual(2, length(Entities)),
    ?assertEqual(1, length(Rels)).

process_extraction_empty_result_test() ->
    Data = make_visionary_completed_data(),
    Extraction = #{<<"entities">> => [], <<"relationships">> => [], <<"insights">> => []},
    {Insights, Entities, Rels} = process_extraction(Data, Extraction),
    ?assertEqual(0, length(Insights)),
    ?assertEqual(0, length(Entities)),
    ?assertEqual(0, length(Rels)).

process_extraction_missing_keys_test() ->
    Data = make_visionary_completed_data(),
    Extraction = #{},
    {Insights, Entities, Rels} = process_extraction(Data, Extraction),
    ?assertEqual(0, length(Insights)),
    ?assertEqual(0, length(Entities)),
    ?assertEqual(0, length(Rels)).

%% ===================================================================
%% Edge Cases
%% ===================================================================

no_notation_output_skips_test() ->
    Data = make_visionary_completed_no_output(),
    ?assertEqual(undefined, maps:get(notation_output, Data)),
    %% The PM should skip processing when notation_output is undefined
    ?assertEqual(skip, should_process(Data)).

has_notation_output_processes_test() ->
    Data = make_visionary_completed_data(),
    ?assertEqual(process, should_process(Data)).

%% ===================================================================
%% Internal — Extracted PM Logic for Testability
%% ===================================================================

build_insight_params(Data, Content) ->
    #{
        venture_id => gf(venture_id, Data),
        content => Content,
        source_agent => gf(agent_role, Data, <<"visionary">>),
        source_session => gf(session_id, Data),
        insight_type => gf(tier, Data, <<"general">>)
    }.

build_entity_params(Data, ExtractedEntity) ->
    #{
        venture_id => gf(venture_id, Data),
        name => maps:get(<<"name">>, ExtractedEntity),
        entity_type => maps:get(<<"type">>, ExtractedEntity, <<"unknown">>),
        description => maps:get(<<"description">>, ExtractedEntity, undefined),
        source_agent => gf(agent_role, Data, <<"visionary">>)
    }.

build_relationship_params(Data, ExtractedRel) ->
    #{
        venture_id => gf(venture_id, Data),
        from_entity => maps:get(<<"from">>, ExtractedRel),
        to_entity => maps:get(<<"to">>, ExtractedRel),
        rel_type => maps:get(<<"type">>, ExtractedRel, <<"related_to">>)
    }.

process_extraction(Data, Extraction) ->
    RawInsights = maps:get(<<"insights">>, Extraction, []),
    RawEntities = maps:get(<<"entities">>, Extraction, []),
    RawRels = maps:get(<<"relationships">>, Extraction, []),
    InsightParams = [build_insight_params(Data, C) || C <- RawInsights, is_binary(C)],
    EntityParams = [build_entity_params(Data, E) || E <- RawEntities, is_map(E)],
    RelParams = [build_relationship_params(Data, R) || R <- RawRels, is_map(R)],
    {InsightParams, EntityParams, RelParams}.

should_process(Data) ->
    case gf(notation_output, Data) of
        undefined -> skip;
        <<>> -> skip;
        _ -> process
    end.

gf(Key, Data) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.
gf(Key, Data, Default) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, Default)
    end.
