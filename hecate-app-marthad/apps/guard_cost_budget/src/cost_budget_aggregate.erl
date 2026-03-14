%%% @doc Cost budget aggregate.
%%%
%%% Stream: cost-budget-{venture_id}
%%% Store: cost_budget_store
%%%
%%% Lifecycle:
%%%   1. set_cost_budget (birth - configures budget limit and warning threshold)
%%%   2. record_spending (accumulates spending, may emit warning/breach)
%%%   3. breach_cost_budget (spending exceeded budget)
%%%   4. adjust_cost_budget (raise or lower budget limit)
%%% @end
-module(cost_budget_aggregate).

-behaviour(evoq_aggregate).

-include("cost_budget_status.hrl").
-include("cost_budget_state.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([flag_map/0]).

-type state() :: #cost_budget_state{}.
-export_type([state/0]).

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?CB_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(_AggregateId) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #cost_budget_state{status = 0}.

%% --- Execute ---
%% NOTE: evoq calls execute(State, Payload) - State FIRST!

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh aggregate — only set_cost_budget allowed
execute(#cost_budget_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"set_cost_budget">> -> execute_set_cost_budget(Payload);
        <<"record_spending">> -> {error, budget_not_set};
        _ -> {error, unknown_command}
    end;

%% Breached — only adjust (to raise limit and unblock)
execute(#cost_budget_state{status = S}, Payload) when S band ?CB_BREACHED =/= 0 ->
    case get_command_type(Payload) of
        <<"adjust_cost_budget">> -> execute_adjust_cost_budget(Payload);
        _ -> {error, budget_breached}
    end;

%% Active — record spending, breach, or adjust
execute(#cost_budget_state{status = S} = State, Payload) when S band ?CB_ACTIVE =/= 0 ->
    case get_command_type(Payload) of
        <<"record_spending">>    -> execute_record_spending(State, Payload);
        <<"breach_cost_budget">> -> execute_breach_cost_budget(Payload);
        <<"adjust_cost_budget">> -> execute_adjust_cost_budget(Payload);
        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Command handlers ---

execute_set_cost_budget(Payload) ->
    {ok, Cmd} = set_cost_budget_v1:from_map(Payload),
    convert_events(maybe_set_cost_budget:handle(Cmd), fun cost_budget_set_v1:to_map/1).

execute_record_spending(State, Payload) ->
    {ok, Cmd} = record_spending_v1:from_map(Payload),
    convert_events(maybe_record_spending:handle(Cmd, State), fun events_to_maps/1).

execute_breach_cost_budget(Payload) ->
    {ok, Cmd} = breach_cost_budget_v1:from_map(Payload),
    convert_events(maybe_breach_cost_budget:handle(Cmd), fun cost_budget_breached_v1:to_map/1).

execute_adjust_cost_budget(Payload) ->
    {ok, Cmd} = adjust_cost_budget_v1:from_map(Payload),
    convert_events(maybe_adjust_cost_budget:handle(Cmd), fun cost_budget_adjusted_v1:to_map/1).

%% --- Apply ---
%% NOTE: evoq calls apply(State, Event) - State FIRST!

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    apply_event(Event, State).

-spec apply_event(map(), state()) -> state().

apply_event(#{event_type := <<"cost_budget_set_v1">>} = E, S)              -> apply_set(E, S);
apply_event(#{event_type := <<"spending_recorded_v1">>} = E, S)            -> apply_spending(E, S);
apply_event(#{event_type := <<"cost_budget_warning_v1">>}, S)              -> apply_warning(S);
apply_event(#{event_type := <<"cost_budget_breached_v1">>} = E, S)         -> apply_breached(E, S);
apply_event(#{event_type := <<"cost_budget_adjusted_v1">>} = E, S)         -> apply_adjusted(E, S);
%% Unknown — ignore
apply_event(_E, S) -> S.

%% --- Apply helpers ---

apply_set(E, State) ->
    State#cost_budget_state{
        venture_id   = gf(venture_id, E),
        budget_usd   = gf(budget_usd, E),
        warning_pct  = gf(warning_pct, E),
        model_policy = gf(model_policy, E),
        status       = evoq_bit_flags:set(evoq_bit_flags:set(0, ?CB_INITIATED), ?CB_ACTIVE),
        initiated_at = gf(initiated_at, E)
    }.

apply_spending(E, #cost_budget_state{} = State) ->
    State#cost_budget_state{
        spent_usd = gf(new_total_usd, E)
    }.

apply_warning(#cost_budget_state{status = Status} = State) ->
    State#cost_budget_state{
        status = evoq_bit_flags:set(Status, ?CB_WARNING)
    }.

apply_breached(E, #cost_budget_state{status = Status} = State) ->
    State#cost_budget_state{
        status      = evoq_bit_flags:set(Status, ?CB_BREACHED),
        breached_at = gf(breached_at, E)
    }.

apply_adjusted(E, #cost_budget_state{status = Status} = State) ->
    %% Adjusting clears breach and warning flags
    NewStatus0 = evoq_bit_flags:unset(Status, ?CB_BREACHED),
    NewStatus1 = evoq_bit_flags:unset(NewStatus0, ?CB_WARNING),
    State#cost_budget_state{
        budget_usd = gf(new_budget_usd, E),
        status     = NewStatus1
    }.

%% --- Internal ---

get_command_type(#{command_type := T}) when is_binary(T) -> T;
get_command_type(#{command_type := T}) when is_atom(T) -> atom_to_binary(T);
get_command_type(_) -> undefined.

convert_events({ok, Events}, ToMapFn) ->
    {ok, [ToMapFn(E) || E <- Events]};
convert_events({error, _} = Err, _) ->
    Err.

%% Special converter for record_spending which returns mixed event types
events_to_maps(Event) ->
    case element(1, Event) of
        spending_recorded_v1     -> spending_recorded_v1:to_map(Event);
        cost_budget_warning_v1   -> cost_budget_warning_v1:to_map(Event);
        cost_budget_breached_v1  -> cost_budget_breached_v1:to_map(Event)
    end.

gf(Key, Data) -> app_marthad_api_utils:get_field(Key, Data).
