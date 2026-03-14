%%% @doc Process manager: on reviewer_completed_v1, auto-escalate to review_gate.
-module(on_reviewer_completed_escalate_review_gate).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"reviewer_completed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    SessionId = gv(session_id, Data),
    case SessionId of
        undefined ->
            logger:warning("[~s] missing session_id in event", [?MODULE]);
        _ ->
            CmdParams = #{session_id => SessionId},
            case escalate_review_gate_v1:new(CmdParams) of
                {ok, Cmd} ->
                    case maybe_escalate_review_gate:dispatch(Cmd) of
                        {ok, _, _} ->
                            logger:info("[~s] escalated ~s to review_gate",
                                       [?MODULE, SessionId]);
                        {error, Reason} ->
                            logger:warning("[~s] failed to escalate ~s: ~p",
                                          [?MODULE, SessionId, Reason])
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
