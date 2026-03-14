%%% @doc Cost budget state module — implements evoq_state behaviour.
%%%
%%% Owns the cost_budget_state record, initial state creation, event folding,
%%% and serialization. Extracted from cost_budget_aggregate.
-module(cost_budget_state).

-behaviour(evoq_state).

-include("cost_budget_status.hrl").
-include("cost_budget_state.hrl").

-export([new/1, apply_event/2, to_map/1]).

-type state() :: #cost_budget_state{}.
-export_type([state/0]).

%% --- evoq_state callbacks ---

-spec new(binary()) -> state().
new(_AggregateId) ->
    #cost_budget_state{status = 0}.

-spec apply_event(state(), map()) -> state().

%% Normalize atom event_type to binary (from typed evoq_event modules)
apply_event(S, #{event_type := Type} = E) when is_atom(Type) ->
    apply_event(S, E#{event_type := atom_to_binary(Type, utf8)});

apply_event(S, #{event_type := <<"cost_budget_set_v1">>} = E)       -> apply_set(E, S);
apply_event(S, #{event_type := <<"spending_recorded_v1">>} = E)     -> apply_spending(E, S);
apply_event(S, #{event_type := <<"cost_budget_warning_v1">>})       -> apply_warning(S);
apply_event(S, #{event_type := <<"cost_budget_breached_v1">>} = E)  -> apply_breached(E, S);
apply_event(S, #{event_type := <<"cost_budget_adjusted_v1">>} = E)  -> apply_adjusted(E, S);
%% Unknown — ignore
apply_event(S, _E) -> S.

%% --- to_map ---

-spec to_map(state()) -> map().
to_map(#cost_budget_state{} = S) ->
    #{
        venture_id   => S#cost_budget_state.venture_id,
        status       => S#cost_budget_state.status,
        budget_usd   => S#cost_budget_state.budget_usd,
        spent_usd    => S#cost_budget_state.spent_usd,
        warning_pct  => S#cost_budget_state.warning_pct,
        model_policy => S#cost_budget_state.model_policy,
        breached_at  => S#cost_budget_state.breached_at,
        initiated_at => S#cost_budget_state.initiated_at
    }.

%% --- Apply helpers ---

apply_set(E, State) ->
    State#cost_budget_state{
        venture_id   = get_value(venture_id, E),
        budget_usd   = get_value(budget_usd, E),
        warning_pct  = get_value(warning_pct, E),
        model_policy = get_value(model_policy, E),
        status       = evoq_bit_flags:set(evoq_bit_flags:set(0, ?CB_INITIATED), ?CB_ACTIVE),
        initiated_at = get_value(initiated_at, E)
    }.

apply_spending(E, #cost_budget_state{} = State) ->
    State#cost_budget_state{
        spent_usd = get_value(new_total_usd, E)
    }.

apply_warning(#cost_budget_state{status = Status} = State) ->
    State#cost_budget_state{
        status = evoq_bit_flags:set(Status, ?CB_WARNING)
    }.

apply_breached(E, #cost_budget_state{status = Status} = State) ->
    State#cost_budget_state{
        status      = evoq_bit_flags:set(Status, ?CB_BREACHED),
        breached_at = get_value(breached_at, E)
    }.

apply_adjusted(E, #cost_budget_state{status = Status} = State) ->
    NewStatus0 = evoq_bit_flags:unset(Status, ?CB_BREACHED),
    NewStatus1 = evoq_bit_flags:unset(NewStatus0, ?CB_WARNING),
    State#cost_budget_state{
        budget_usd = get_value(new_budget_usd, E),
        status     = NewStatus1
    }.

%% --- Internal ---

get_value(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
