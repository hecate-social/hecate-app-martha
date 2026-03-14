%%% @doc Process manager: on {role}_initiated_v1, assign agent to division team.
%%% When an agent session is initiated with a division_id, assigns it
%%% to the corresponding division team.
-module(on_agent_initiated_assign_agent_to_team).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [
    <<"visionary_initiated_v1">>,
    <<"explorer_initiated_v1">>,
    <<"stormer_initiated_v1">>,
    <<"architect_initiated_v1">>,
    <<"erlang_coder_initiated_v1">>,
    <<"svelte_coder_initiated_v1">>,
    <<"sql_coder_initiated_v1">>,
    <<"tester_initiated_v1">>,
    <<"reviewer_initiated_v1">>,
    <<"coordinator_initiated_v1">>,
    <<"delivery_manager_initiated_v1">>,
    <<"mentor_initiated_v1">>
].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    SessionId = gv(session_id, Data),
    AgentRole = gv(agent_role, Data),
    DivisionId = gv(division_id, Data),
    case DivisionId of
        undefined ->
            %% Venture-level agents (visionary, explorer) have no division — skip
            ok;
        <<>> ->
            ok;
        _ ->
            CmdParams = #{
                division_id => DivisionId,
                agent_role => AgentRole,
                session_id => SessionId
            },
            case assign_agent_to_team_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_assign_agent_to_team:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] assigned ~s (~s) to team ~s",
                                       [?MODULE, SessionId, AgentRole, DivisionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to assign ~s to team ~s: ~p",
                                          [?MODULE, SessionId, DivisionId, Reason])
                    end;
                {error, Reason} ->
                    logger:warning("[~s] failed to create command: ~p", [?MODULE, Reason])
            end
    end,
    {ok, State}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
