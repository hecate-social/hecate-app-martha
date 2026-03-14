%%% @doc PM: on agent completed, audit quality against knowledge graph.
%%% Subscribes to orchestration_store for *_completed_v1 events.
%%% Runs quality audit and captures any warnings as insights.
-module(on_agent_completed_audit_quality).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() ->
    [<<"visionary_completed_v1">>,
     <<"explorer_completed_v1">>,
     <<"architect_completed_v1">>,
     <<"stormer_completed_v1">>,
     <<"reviewer_completed_v1">>,
     <<"erlang_coder_completed_v1">>,
     <<"svelte_coder_completed_v1">>,
     <<"sql_coder_completed_v1">>,
     <<"tester_completed_v1">>,
     <<"coordinator_completed_v1">>,
     <<"mentor_completed_v1">>,
     <<"delivery_manager_completed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    case should_process(Data) of
        true ->
            spawn_link(fun() -> do_audit(Data) end);
        false ->
            ok
    end,
    {ok, State}.

%% Internal

should_process(Data) ->
    Output = gv(notation_output, Data),
    Output =/= undefined andalso Output =/= <<>> andalso Output =/= null.

do_audit(Data) ->
    SessionId = gv(session_id, Data),
    VentureId = gv(venture_id, Data),
    AgentRole = gv(agent_role, Data),
    NotationOutput = gv(notation_output, Data),
    case {VentureId, AgentRole, NotationOutput} of
        {undefined, _, _} -> ok;
        {_, undefined, _} -> ok;
        {_, _, undefined} -> ok;
        _ ->
            case audit_agent_quality:audit(VentureId, AgentRole, NotationOutput) of
                {ok, []} ->
                    logger:debug("[~s] no quality warnings for session ~s", [?MODULE, SessionId]);
                {ok, Warnings} ->
                    logger:info("[~s] found ~b quality warnings for session ~s",
                               [?MODULE, length(Warnings), SessionId]),
                    capture_warnings(VentureId, AgentRole, SessionId, Warnings)
            end
    end.

capture_warnings(VentureId, AgentRole, SessionId, Warnings) ->
    lists:foreach(fun(Warning) ->
        Params = #{
            venture_id => VentureId,
            content => Warning,
            source_agent => <<"quality_auditor">>,
            source_session => SessionId,
            insight_type => <<"quality_warning">>
        },
        case capture_insight_v1:new(Params) of
            {ok, Cmd} ->
                case maybe_capture_insight:dispatch(Cmd) of
                    {ok, _, _} ->
                        logger:info("[~s] captured quality warning for ~s/~s",
                                   [?MODULE, AgentRole, SessionId]);
                    {error, Reason} ->
                        logger:warning("[~s] failed to dispatch quality warning: ~p",
                                      [?MODULE, Reason])
                end;
            {error, Reason} ->
                logger:warning("[~s] failed to create capture_insight: ~p", [?MODULE, Reason])
        end
    end, Warnings).

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
