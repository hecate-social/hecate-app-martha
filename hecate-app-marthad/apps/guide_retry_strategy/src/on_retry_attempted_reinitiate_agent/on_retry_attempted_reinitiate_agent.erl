%%% @doc PM: on retry attempted, reinitiate the agent with adjusted context.
%%% Subscribes to retry_strategy_store for retry_attempted_v1 events.
%%% Reads the retry state to get venture_id/agent_role, then dispatches
%%% the appropriate initiate_{role}_v1 command back to orchestration_store
%%% with adjusted input_context.
-module(on_retry_attempted_reinitiate_agent).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"retry_attempted_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    SessionId = gv(<<"session_id">>, Data),
    Adjustment = gv(<<"adjustment">>, Data, #{}),
    spawn_link(fun() -> do_reinitiate(SessionId, Adjustment) end),
    {ok, State}.

%% Internal

do_reinitiate(SessionId, Adjustment) ->
    %% Look up the retry state from the projection to get venture_id and role
    case project_retry_strategy_store:get_retry(SessionId) of
        {ok, RetryInfo} ->
            VentureId = maps:get(<<"venture_id">>, RetryInfo, maps:get(venture_id, RetryInfo, <<>>)),
            AgentRole = maps:get(<<"agent_role">>, RetryInfo, maps:get(agent_role, RetryInfo, <<>>)),
            Emphasis = maps:get(<<"emphasis">>, Adjustment, <<>>),
            OrigRejection = maps:get(<<"original_rejection">>, Adjustment, <<>>),
            AdjustedContext = build_adjusted_context(OrigRejection, Emphasis),
            dispatch_initiate(AgentRole, VentureId, AdjustedContext);
        {error, not_found} ->
            logger:warning("[~s] retry state not found for session ~s", [?MODULE, SessionId])
    end.

build_adjusted_context(OrigRejection, Emphasis) ->
    Parts = [
        <<"[RETRY] This is a retry attempt. Previous output was rejected.\n\n">>,
        case OrigRejection of
            <<>> -> <<>>;
            _ -> <<"Rejection reason: ", OrigRejection/binary, "\n\n">>
        end,
        case Emphasis of
            <<>> -> <<>>;
            _ -> <<"Guidance for this attempt: ", Emphasis/binary, "\n\n">>
        end,
        <<"Please produce improved output that addresses the rejection feedback.">>
    ],
    iolist_to_binary(Parts).

dispatch_initiate(AgentRole, VentureId, AdjustedContext) ->
    %% Use the role_modules lookup from orchestrate_agents to find the right
    %% initiate command module and handler
    case agent_orchestration_aggregate:role_modules(initiate, AgentRole) of
        {ok, CmdMod, _HandlerMod, _ToMapFn} ->
            %% Generate a new session ID for the retry (new aggregate instance)
            NewSessionId = generate_retry_session_id(AgentRole),
            CmdParams = #{
                session_id => NewSessionId,
                venture_id => VentureId,
                input_context => AdjustedContext
            },
            case CmdMod:new(CmdParams) of
                {ok, Cmd} ->
                    %% Find the dispatch module (maybe_initiate_{role})
                    DispatchMod = dispatch_module(AgentRole),
                    case DispatchMod:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] reinitiated ~s agent as session ~s",
                                       [?MODULE, AgentRole, NewSessionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to dispatch reinitiate for ~s: ~p",
                                          [?MODULE, AgentRole, Reason])
                    end;
                {error, Reason} ->
                    logger:warning("[~s] failed to create initiate command for ~s: ~p",
                                  [?MODULE, AgentRole, Reason])
            end;
        {error, role_not_implemented} ->
            logger:warning("[~s] role ~s not implemented, cannot reinitiate", [?MODULE, AgentRole])
    end.

dispatch_module(<<"visionary">>)        -> maybe_initiate_visionary;
dispatch_module(<<"explorer">>)         -> maybe_initiate_explorer;
dispatch_module(<<"architect">>)        -> maybe_initiate_architect;
dispatch_module(<<"stormer">>)          -> maybe_initiate_stormer;
dispatch_module(<<"reviewer">>)         -> maybe_initiate_reviewer;
dispatch_module(<<"erlang_coder">>)     -> maybe_initiate_erlang_coder;
dispatch_module(<<"svelte_coder">>)     -> maybe_initiate_svelte_coder;
dispatch_module(<<"sql_coder">>)        -> maybe_initiate_sql_coder;
dispatch_module(<<"tester">>)           -> maybe_initiate_tester;
dispatch_module(<<"coordinator">>)      -> maybe_initiate_coordinator;
dispatch_module(<<"mentor">>)           -> maybe_initiate_mentor;
dispatch_module(<<"delivery_manager">>) -> maybe_initiate_delivery_manager;
dispatch_module(_) -> maybe_initiate_visionary.  %% fallback

generate_retry_session_id(Role) ->
    Rand = integer_to_binary(erlang:unique_integer([positive, monotonic])),
    RolePrefix = binary:part(Role, 0, min(3, byte_size(Role))),
    <<"retry-", RolePrefix/binary, "-", Rand/binary>>.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_binary(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            case maps:find(binary_to_atom(Key), Map) of
                {ok, V2} -> V2;
                error -> Default
            end
    end.
