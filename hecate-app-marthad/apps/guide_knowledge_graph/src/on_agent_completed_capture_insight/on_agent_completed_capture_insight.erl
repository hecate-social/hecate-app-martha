%%% @doc Process Manager: *_completed_v1 -> capture_insight/recognize_entity/draw_relationship
%%%
%%% When any agent session completes (visionary, explorer, architect, stormer,
%%% reviewer), extract knowledge from its notation output and persist it in the
%%% knowledge graph. Uses translate_output with the knowledge_extraction schema
%%% (LLM-assisted) to extract entities, relationships, and insights.
%%%
%%% Subscribes to: orchestration_store (*_completed_v1)
%%% Dispatches to: knowledge_graph_store (capture_insight, recognize_entity, draw_relationship)
%%% @end
-module(on_agent_completed_capture_insight).

-behaviour(evoq_event_handler).

-export([interested_in/0, init/1, handle_event/4]).

interested_in() ->
    [<<"visionary_completed_v1">>,
     <<"explorer_completed_v1">>,
     <<"architect_completed_v1">>,
     <<"stormer_completed_v1">>,
     <<"reviewer_completed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    case should_process(Data) of
        skip ->
            logger:debug("[KG-PM] Agent completed with no output, skipping"),
            {ok, State};
        process ->
            spawn_link(fun() -> extract_and_dispatch(Data) end),
            {ok, State}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

extract_and_dispatch(Data) ->
    VentureId = gf(venture_id, Data),
    Content = gf(notation_output, Data),
    logger:info("[KG-PM] Extracting knowledge from agent output for venture ~s", [VentureId]),
    case translate_output:translate(Content, <<"knowledge_extraction">>) of
        {ok, Extraction} ->
            dispatch_all(Data, Extraction);
        {error, Reason} ->
            logger:warning("[KG-PM] Knowledge extraction failed for ~s: ~p", [VentureId, Reason]),
            %% Fallback: capture the raw output as a single insight
            dispatch_raw_insight(Data, Content)
    end.

dispatch_all(Data, Extraction) ->
    VentureId = gf(venture_id, Data),
    {InsightParams, EntityParams, RelParams} = process_extraction(Data, Extraction),
    dispatch_insights(InsightParams),
    dispatch_entities(EntityParams),
    dispatch_relationships(RelParams),
    Total = length(InsightParams) + length(EntityParams) + length(RelParams),
    logger:info("[KG-PM] Dispatched ~B knowledge items for venture ~s", [Total, VentureId]).

dispatch_insights(ParamsList) ->
    lists:foreach(fun(Params) ->
        case capture_insight_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_capture_insight:dispatch(Cmd) of
                    {ok, _, _} -> ok;
                    {error, Reason} ->
                        logger:warning("[KG-PM] Failed to capture insight: ~p", [Reason])
                end;
            {error, Reason} ->
                logger:warning("[KG-PM] Invalid insight params: ~p", [Reason])
        end
    end, ParamsList).

dispatch_entities(ParamsList) ->
    lists:foreach(fun(Params) ->
        case recognize_entity_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_recognize_entity:dispatch(Cmd) of
                    {ok, _, _} -> ok;
                    {error, Reason} ->
                        logger:warning("[KG-PM] Failed to recognize entity: ~p", [Reason])
                end;
            {error, Reason} ->
                logger:warning("[KG-PM] Invalid entity params: ~p", [Reason])
        end
    end, ParamsList).

dispatch_relationships(ParamsList) ->
    lists:foreach(fun(Params) ->
        case draw_relationship_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_draw_relationship:dispatch(Cmd) of
                    {ok, _, _} -> ok;
                    {error, Reason} ->
                        logger:warning("[KG-PM] Failed to draw relationship: ~p", [Reason])
                end;
            {error, Reason} ->
                logger:warning("[KG-PM] Invalid relationship params: ~p", [Reason])
        end
    end, ParamsList).

dispatch_raw_insight(Data, Content) ->
    Params = build_insight_params(Data, Content),
    case capture_insight_v1:new(Params) of
        {ok, Cmd} ->
            case maybe_capture_insight:dispatch(Cmd) of
                {ok, _, _} -> ok;
                {error, Reason} ->
                    logger:warning("[KG-PM] Failed to capture raw insight: ~p", [Reason])
            end;
        {error, Reason} ->
            logger:warning("[KG-PM] Invalid raw insight params: ~p", [Reason])
    end.

%% ===================================================================
%% Field Mapping (extracted for testability — see test module)
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
