%%% @doc maybe_breach_cost_budget handler
%%% Business logic for explicitly breaching a cost budget.
-module(maybe_breach_cost_budget).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).


%% @doc Handle breach_cost_budget_v1 command (business logic only)
-spec handle(breach_cost_budget_v1:breach_cost_budget_v1()) ->
    {ok, [cost_budget_breached_v1:cost_budget_breached_v1()]} | {error, term()}.
handle(Cmd) ->
    handle(Cmd, undefined).

%% @doc Handle with state (for aggregate pattern)
-spec handle(breach_cost_budget_v1:breach_cost_budget_v1(), term()) ->
    {ok, [cost_budget_breached_v1:cost_budget_breached_v1()]} | {error, term()}.
handle(Cmd, _State) ->
    VentureId = breach_cost_budget_v1:get_venture_id(Cmd),
    case validate_command(VentureId) of
        ok ->
            Event = create_event(Cmd),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Dispatch command via evoq (persists to ReckonDB)
-spec dispatch(breach_cost_budget_v1:breach_cost_budget_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = breach_cost_budget_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = breach_cost_budget,
        aggregate_type = cost_budget_aggregate,
        aggregate_id = VentureId,
        payload = breach_cost_budget_v1:to_map(Cmd),
        metadata = #{timestamp => Timestamp, aggregate_type => cost_budget_aggregate},
        causation_id = undefined,
        correlation_id = undefined
    },

    Opts = #{
        store_id => cost_budget_store,
        adapter => reckon_evoq_adapter,
        consistency => eventual
    },

    evoq_dispatcher:dispatch(EvoqCmd, Opts).

%% Internal

validate_command(VentureId) when is_binary(VentureId), byte_size(VentureId) > 0 ->
    ok;
validate_command(_) ->
    {error, invalid_venture_id}.

create_event(Cmd) ->
    cost_budget_breached_v1:new(#{
        venture_id => breach_cost_budget_v1:get_venture_id(Cmd)
    }).
