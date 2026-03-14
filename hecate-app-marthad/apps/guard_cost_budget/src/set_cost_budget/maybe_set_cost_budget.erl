%%% @doc maybe_set_cost_budget handler
%%% Business logic for configuring a cost budget.
%%% Validates venture_id is non-empty and budget_usd is positive.
-module(maybe_set_cost_budget).

-include_lib("evoq/include/evoq.hrl").

-export([handle/1, handle/2, dispatch/1]).


%% @doc Handle set_cost_budget_v1 command (business logic only)
-spec handle(set_cost_budget_v1:set_cost_budget_v1()) ->
    {ok, [cost_budget_set_v1:cost_budget_set_v1()]} | {error, term()}.
handle(Cmd) ->
    handle(Cmd, undefined).

%% @doc Handle with state (for aggregate pattern)
-spec handle(set_cost_budget_v1:set_cost_budget_v1(), term()) ->
    {ok, [cost_budget_set_v1:cost_budget_set_v1()]} | {error, term()}.
handle(Cmd, _State) ->
    VentureId = set_cost_budget_v1:get_venture_id(Cmd),
    BudgetUsd = set_cost_budget_v1:get_budget_usd(Cmd),
    case validate_command(VentureId, BudgetUsd) of
        ok ->
            Event = create_event(Cmd),
            {ok, [Event]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Dispatch command via evoq (persists to ReckonDB)
-spec dispatch(set_cost_budget_v1:set_cost_budget_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = set_cost_budget_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = set_cost_budget,
        aggregate_type = cost_budget_aggregate,
        aggregate_id = VentureId,
        payload = set_cost_budget_v1:to_map(Cmd),
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

validate_command(VentureId, BudgetUsd) when
    is_binary(VentureId), byte_size(VentureId) > 0,
    is_number(BudgetUsd), BudgetUsd > 0 ->
    ok;
validate_command(VentureId, _BudgetUsd) when
    not is_binary(VentureId); byte_size(VentureId) =:= 0 ->
    {error, invalid_venture_id};
validate_command(_VentureId, _BudgetUsd) ->
    {error, invalid_budget_usd}.

create_event(Cmd) ->
    cost_budget_set_v1:new(#{
        venture_id   => set_cost_budget_v1:get_venture_id(Cmd),
        budget_usd   => set_cost_budget_v1:get_budget_usd(Cmd),
        warning_pct  => set_cost_budget_v1:get_warning_pct(Cmd),
        model_policy => set_cost_budget_v1:get_model_policy(Cmd)
    }).
