%%% @doc maybe_record_spending handler
%%% Business logic for recording spending against a cost budget.
%%% Requires aggregate state to compute new total and check thresholds.
%%% May emit additional events:
%%%   - cost_budget_warning_v1 if warning threshold crossed
%%%   - cost_budget_breached_v1 if budget exceeded
-module(maybe_record_spending).

-include_lib("evoq/include/evoq.hrl").
-include("cost_budget_status.hrl").
-include("cost_budget_state.hrl").

-export([handle/1, handle/2, dispatch/1]).


%% @doc Handle without state (not supported for this command)
-spec handle(record_spending_v1:record_spending_v1()) ->
    {ok, list()} | {error, term()}.
handle(_Cmd) ->
    {error, state_required}.

%% @doc Handle with aggregate state
-spec handle(record_spending_v1:record_spending_v1(), #cost_budget_state{}) ->
    {ok, list()} | {error, term()}.
handle(Cmd, #cost_budget_state{} = State) ->
    VentureId = record_spending_v1:get_venture_id(Cmd),
    AmountUsd = record_spending_v1:get_amount_usd(Cmd),
    case validate_command(VentureId, AmountUsd) of
        ok ->
            Events = create_events(Cmd, State),
            {ok, Events};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Dispatch command via evoq (persists to ReckonDB)
-spec dispatch(record_spending_v1:record_spending_v1()) ->
    {ok, non_neg_integer(), [map()]} | {error, term()}.
dispatch(Cmd) ->
    VentureId = record_spending_v1:get_venture_id(Cmd),
    Timestamp = erlang:system_time(millisecond),

    EvoqCmd = #evoq_command{
        command_type = record_spending,
        aggregate_type = cost_budget_aggregate,
        aggregate_id = VentureId,
        payload = record_spending_v1:to_map(Cmd),
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

validate_command(VentureId, AmountUsd) when
    is_binary(VentureId), byte_size(VentureId) > 0,
    is_number(AmountUsd), AmountUsd > 0 ->
    ok;
validate_command(VentureId, _AmountUsd) when
    not is_binary(VentureId); byte_size(VentureId) =:= 0 ->
    {error, invalid_venture_id};
validate_command(_VentureId, _AmountUsd) ->
    {error, invalid_amount_usd}.

create_events(Cmd, #cost_budget_state{spent_usd = SpentUsd, budget_usd = BudgetUsd,
                                       warning_pct = WarningPct, status = Status} = _State) ->
    VentureId = record_spending_v1:get_venture_id(Cmd),
    AmountUsd = record_spending_v1:get_amount_usd(Cmd),
    NewTotal = SpentUsd + AmountUsd,

    SpendingEvent = spending_recorded_v1:new(#{
        venture_id    => VentureId,
        amount_usd    => AmountUsd,
        new_total_usd => NewTotal,
        model         => record_spending_v1:get_model(Cmd),
        session_id    => record_spending_v1:get_session_id(Cmd)
    }),

    WarningThreshold = BudgetUsd * WarningPct,
    AlreadyWarned = evoq_bit_flags:has(Status, ?CB_WARNING),

    %% Check if we crossed the warning threshold (and haven't warned yet)
    WarningEvents = case NewTotal >= WarningThreshold andalso not AlreadyWarned andalso NewTotal < BudgetUsd of
        true ->
            [cost_budget_warning_v1:new(#{
                venture_id  => VentureId,
                spent_usd   => NewTotal,
                budget_usd  => BudgetUsd,
                warning_pct => WarningPct
            })];
        false ->
            []
    end,

    %% Check if we breached the budget
    BreachEvents = case NewTotal >= BudgetUsd of
        true ->
            [cost_budget_breached_v1:new(#{
                venture_id => VentureId,
                spent_usd  => NewTotal,
                budget_usd => BudgetUsd
            })];
        false ->
            []
    end,

    [SpendingEvent] ++ WarningEvents ++ BreachEvents.
