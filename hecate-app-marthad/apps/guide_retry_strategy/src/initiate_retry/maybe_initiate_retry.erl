%%% @doc Handler for initiate_retry command.
%%% Validates and produces retry_initiated_v1 event.
-module(maybe_initiate_retry).
-export([handle/1, dispatch/1]).

-spec handle(map()) -> {ok, [map()]} | {error, term()}.
handle(Payload) ->
    case initiate_retry_v1:from_map(Payload) of
        {ok, Cmd} ->
            Event = retry_initiated_v1:new(#{
                session_id => initiate_retry_v1:get_session_id(Cmd),
                venture_id => initiate_retry_v1:get_venture_id(Cmd),
                agent_role => initiate_retry_v1:get_agent_role(Cmd)
            }),
            {ok, [retry_initiated_v1:to_map(Event)]};
        {error, _} = Err ->
            Err
    end.

-spec dispatch(initiate_retry_v1:initiate_retry_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    SessionId = initiate_retry_v1:get_session_id(Cmd),
    EvoqCmd = #{
        command_type => initiate_retry,
        aggregate_type => retry_strategy_aggregate,
        aggregate_id => SessionId,
        payload => initiate_retry_v1:to_map(Cmd),
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
