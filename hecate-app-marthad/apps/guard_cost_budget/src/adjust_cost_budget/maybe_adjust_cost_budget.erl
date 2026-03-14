%%% @doc maybe_adjust_cost_budget handler
%%% Business logic for adjusting a cost budget limit.
-module(maybe_adjust_cost_budget).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).


%% @doc Handle adjust_cost_budget_v1 command (business logic only)
-spec handle(adjust_cost_budget_v1:adjust_cost_budget_v1()) ->
    {ok, [cost_budget_adjusted_v1:cost_budget_adjusted_v1()]} | {error, term()}.
handle(Cmd) ->
    handle(Cmd, undefined).

%% @doc Handle with state (for aggregate pattern)
-spec handle(adjust_cost_budget_v1:adjust_cost_budget_v1(), term()) ->
    {ok, [cost_budget_adjusted_v1:cost_budget_adjusted_v1()]} | {error, term()}.
handle(Cmd, _State) ->
    VentureId = adjust_cost_budget_v1:get_venture_id(Cmd),
    NewBudgetUsd = adjust_cost_budget_v1:get_new_budget_usd(Cmd),
    case validate_command(VentureId, NewBudgetUsd) of
        ok ->
            Event = create_event(Cmd),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Dispatch command via evoq (persists to ReckonDB)
-spec dispatch(adjust_cost_budget_v1:adjust_cost_budget_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = adjust_cost_budget_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = adjust_cost_budget,
        aggregate_type = cost_budget_aggregate,
        aggregate_id = VentureId,
        payload = adjust_cost_budget_v1:to_map(Cmd),
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

validate_command(VentureId, NewBudgetUsd) when
    is_binary(VentureId), byte_size(VentureId) > 0,
    is_number(NewBudgetUsd), NewBudgetUsd > 0 ->
    ok;
validate_command(VentureId, _NewBudgetUsd) when
    not is_binary(VentureId); byte_size(VentureId) =:= 0 ->
    {error, invalid_venture_id};
validate_command(_VentureId, _NewBudgetUsd) ->
    {error, invalid_new_budget_usd}.

create_event(Cmd) ->
    cost_budget_adjusted_v1:new(#{
        venture_id     => adjust_cost_budget_v1:get_venture_id(Cmd),
        new_budget_usd => adjust_cost_budget_v1:get_new_budget_usd(Cmd)
    }).
