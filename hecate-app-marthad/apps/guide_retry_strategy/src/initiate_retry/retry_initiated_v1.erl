%%% @doc retry_initiated_v1 event.
%%% Emitted when a retry strategy is initiated for an agent session.
-module(retry_initiated_v1).

-behaviour(evoq_event).
-export([new/1, to_map/1, from_map/1]).
-export([event_type/0]).

-record(retry_initiated_v1, {
    session_id     :: binary(),
    venture_id     :: binary(),
    agent_role     :: binary(),
    failure_reason :: binary() | undefined,
    max_attempts   :: non_neg_integer(),
    initiated_at   :: integer()
}).

-opaque retry_initiated_v1() :: #retry_initiated_v1{}.
-export_type([retry_initiated_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> retry_initiated_v1().
-spec event_type() -> atom().
event_type() -> retry_initiated_v1.

new(#{session_id := SId} = P) ->
    #retry_initiated_v1{
        session_id = SId,
        venture_id = maps:get(venture_id, P, <<>>),
        agent_role = maps:get(agent_role, P, <<>>),
        failure_reason = maps:get(failure_reason, P, undefined),
        max_attempts = maps:get(max_attempts, P, 3),
        initiated_at = erlang:system_time(millisecond)
    }.

-spec to_map(retry_initiated_v1()) -> map().
to_map(#retry_initiated_v1{} = E) ->
    #{
        event_type => retry_initiated_v1,
        session_id => E#retry_initiated_v1.session_id,
        venture_id => E#retry_initiated_v1.venture_id,
        agent_role => E#retry_initiated_v1.agent_role,
        failure_reason => E#retry_initiated_v1.failure_reason,
        max_attempts => E#retry_initiated_v1.max_attempts,
        initiated_at => E#retry_initiated_v1.initiated_at
    }.

-spec from_map(map()) -> {ok, retry_initiated_v1()} | {error, term()}.
from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #retry_initiated_v1{
                session_id = SId,
                venture_id = gv(venture_id, Map, <<>>),
                agent_role = gv(agent_role, Map, <<>>),
                failure_reason = gv(failure_reason, Map),
                max_attempts = gv(max_attempts, Map, 3),
                initiated_at = gv(initiated_at, Map, 0)
            }}
    end.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
