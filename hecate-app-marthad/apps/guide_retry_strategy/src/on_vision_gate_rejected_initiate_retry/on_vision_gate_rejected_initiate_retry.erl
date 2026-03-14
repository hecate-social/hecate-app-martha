%%% @doc PM: on vision gate rejected, initiate retry strategy.
%%% Subscribes to orchestration_store for vision_gate_rejected_v1 events.
%%% Reads the rejection reason, determines adjustments, and dispatches
%%% initiate_retry_v1 to retry_strategy_store.
-module(on_vision_gate_rejected_initiate_retry).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"vision_gate_rejected_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    SessionId = gv(session_id, Data),
    VentureId = gv(venture_id, Data),
    AgentRole = gv(agent_role, Data),
    RejectionReason = gv(rejection_reason, Data),
    spawn_link(fun() ->
        do_initiate_retry(SessionId, VentureId, AgentRole, RejectionReason)
    end),
    {ok, State}.

%% Internal

do_initiate_retry(SessionId, VentureId, AgentRole, RejectionReason) ->
    case initiate_retry_v1:new(#{
        session_id => SessionId,
        venture_id => VentureId,
        agent_role => AgentRole,
        failure_reason => RejectionReason,
        max_attempts => 3
    }) of
        {ok, Cmd} ->
            case maybe_initiate_retry:dispatch(Cmd) of
                {ok, _, _} ->
                    logger:info("[~s] initiated retry for session ~s (reason: ~s)",
                               [?MODULE, SessionId, RejectionReason]),
                    %% Now attempt the first retry
                    do_attempt_retry(SessionId, RejectionReason);
                {error, Reason} ->
                    logger:warning("[~s] failed to initiate retry for ~s: ~p",
                                  [?MODULE, SessionId, Reason])
            end;
        {error, Reason} ->
            logger:warning("[~s] failed to create initiate_retry cmd for ~s: ~p",
                          [?MODULE, SessionId, Reason])
    end.

do_attempt_retry(SessionId, RejectionReason) ->
    Adjustment = compute_adjustment(RejectionReason),
    case attempt_retry_v1:new(#{
        session_id => SessionId,
        adjustment => Adjustment
    }) of
        {ok, Cmd} ->
            case maybe_attempt_retry:dispatch(Cmd) of
                {ok, _, _} ->
                    logger:info("[~s] dispatched retry attempt for ~s", [?MODULE, SessionId]);
                {error, Reason} ->
                    logger:warning("[~s] failed to dispatch retry attempt for ~s: ~p",
                                  [?MODULE, SessionId, Reason])
            end;
        {error, Reason} ->
            logger:warning("[~s] failed to create attempt_retry cmd for ~s: ~p",
                          [?MODULE, SessionId, Reason])
    end.

%% Rule-based adjustment computation from rejection reason
compute_adjustment(undefined) ->
    #{<<"emphasis">> => <<"Please address the previous rejection and improve your output.">>};
compute_adjustment(Reason) when is_binary(Reason) ->
    Emphasis = compute_emphasis(Reason),
    #{
        <<"emphasis">> => Emphasis,
        <<"original_rejection">> => Reason
    }.

compute_emphasis(Reason) ->
    Lower = string:lowercase(Reason),
    case {contains(Lower, "scope"), contains(Lower, "detail"),
          contains(Lower, "technical"), contains(Lower, "user")} of
        {true, _, _, _} ->
            <<"Focus on narrow scope. Do not expand beyond the core problem statement.">>;
        {_, true, _, _} ->
            <<"Provide specific, concrete details. Avoid vague or generic statements.">>;
        {_, _, true, _} ->
            <<"Include technical implementation specifics and feasibility analysis.">>;
        {_, _, _, true} ->
            <<"Center the analysis on user needs, personas, and real-world usage scenarios.">>;
        _ ->
            <<"Previous attempt was rejected: ", Reason/binary, ". Please address the feedback.">>
    end.

contains(Hay, Needle) ->
    string:find(Hay, Needle) =/= nomatch.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
