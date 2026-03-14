%%% @doc Handler for attempt_retry command.
%%% Validates attempt count against max, produces retry_attempted_v1 or exhaust.
-module(maybe_attempt_retry).
-export([handle/2, dispatch/1]).

-include("retry_state.hrl").

-spec handle(map(), #retry_state{}) -> {ok, [map()]} | {error, term()}.
handle(Payload, #retry_state{attempt_count = Count, max_attempts = Max}) when Count >= Max ->
    %% Max attempts reached — exhaust instead
    SessionId = gv(<<"session_id">>, Payload),
    Event = retry_exhausted_v1:new(#{session_id => SessionId}),
    {ok, [retry_exhausted_v1:to_map(Event)]};
handle(Payload, #retry_state{attempt_count = Count}) ->
    case attempt_retry_v1:from_map(Payload) of
        {ok, Cmd} ->
            Event = retry_attempted_v1:new(#{
                session_id => attempt_retry_v1:get_session_id(Cmd),
                attempt_number => Count + 1,
                adjustment => attempt_retry_v1:get_adjustment(Cmd)
            }),
            {ok, [retry_attempted_v1:to_map(Event)]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(attempt_retry_v1:attempt_retry_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    SessionId = attempt_retry_v1:get_session_id(Cmd),
    EvoqCmd = #{
        command_type => attempt_retry,
        aggregate_type => retry_strategy_aggregate,
        aggregate_id => SessionId,
        payload => attempt_retry_v1:to_map(Cmd),
        metadata => #{
            timestamp => erlang:system_time(millisecond),
            aggregate_type => retry_strategy_aggregate
        },
        causation_id => undefined,
        correlation_id => undefined
    },
    evoq_dispatcher:dispatch(EvoqCmd, #{
        store_id => retry_strategy_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    }).

gv(Key, Map) when is_binary(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(binary_to_atom(Key), Map, undefined)
    end.
