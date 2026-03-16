%%% @doc Process Manager: on meditation finding contributed, fan out to knowledge graph.
%%%
%%% Reacts to meditation_finding_contributed_v1 events and dispatches to the
%%% knowledge graph aggregate:
%%% - domain_concept / terminology -> recognize_entity_v1
%%% - business_rule / industry_pattern / risk / prior_art -> capture_insight_v1
%%% @end
-module(on_meditation_finding_contributed_fan_out).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"meditation_finding_contributed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    VentureId = gv(venture_id, Data),
    ParticipantId = gv(participant_id, Data),
    FindingType = gv(finding_type, Data),
    Content = gv(content, Data),
    spawn_link(fun() -> fan_out(VentureId, ParticipantId, FindingType, Content) end),
    {ok, State}.

%% Internal

fan_out(VentureId, ParticipantId, FindingType, Content) ->
    case classify_finding(FindingType) of
        entity ->
            dispatch_recognize_entity(VentureId, ParticipantId, FindingType, Content);
        insight ->
            dispatch_capture_insight(VentureId, ParticipantId, FindingType, Content)
    end.

classify_finding(<<"domain_concept">>) -> entity;
classify_finding(<<"terminology">>) -> entity;
classify_finding(<<"business_rule">>) -> insight;
classify_finding(<<"industry_pattern">>) -> insight;
classify_finding(<<"risk">>) -> insight;
classify_finding(<<"prior_art">>) -> insight;
classify_finding(_) -> insight.

dispatch_recognize_entity(VentureId, ParticipantId, FindingType, Content) ->
    Params = #{
        venture_id => VentureId,
        entity_type => FindingType,
        name => Content,
        source_agent => <<"meditation:", ParticipantId/binary>>
    },
    case recognize_entity_v1:from_map(Params) of
        {ok, Cmd} ->
            case maybe_recognize_entity:dispatch(Cmd) of
                {ok, _, _} -> ok;
                {error, Reason} ->
                    logger:warning("[~s] failed to recognize entity: ~p", [?MODULE, Reason])
            end;
        {error, Reason} ->
            logger:warning("[~s] failed to create entity command: ~p", [?MODULE, Reason])
    end.

dispatch_capture_insight(VentureId, ParticipantId, FindingType, Content) ->
    Params = #{
        venture_id => VentureId,
        content => Content,
        insight_type => FindingType,
        source_agent => <<"meditation:", ParticipantId/binary>>
    },
    case capture_insight_v1:from_map(Params) of
        {ok, Cmd} ->
            case maybe_capture_insight:dispatch(Cmd) of
                {ok, _, _} -> ok;
                {error, Reason} ->
                    logger:warning("[~s] failed to capture insight: ~p", [?MODULE, Reason])
            end;
        {error, Reason} ->
            logger:warning("[~s] failed to create insight command: ~p", [?MODULE, Reason])
    end.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
