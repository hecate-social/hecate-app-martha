%%% @doc Handler for exhaust_retry command.
-module(maybe_exhaust_retry).
-export([handle/1, dispatch/1]).

-spec handle(map()) -> {ok, [map()]} | {error, term()}.
handle(Payload) ->
    case exhaust_retry_v1:from_map(Payload) of
        {ok, Cmd} ->
            Event = retry_exhausted_v1:new(#{
                session_id => exhaust_retry_v1:get_session_id(Cmd)
            }),
            {ok, [retry_exhausted_v1:to_map(Event)]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(exhaust_retry_v1:exhaust_retry_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    SessionId = exhaust_retry_v1:get_session_id(Cmd),
    EvoqCmd = #{
        command_type => exhaust_retry,
        aggregate_type => retry_strategy_aggregate,
        aggregate_id => SessionId,
        payload => exhaust_retry_v1:to_map(Cmd),
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
