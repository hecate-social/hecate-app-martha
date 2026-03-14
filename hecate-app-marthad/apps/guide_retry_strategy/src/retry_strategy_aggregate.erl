%%% @doc Retry strategy aggregate — manages retry lifecycle per session.
%%%
%%% Stream: retry-{session_id}
%%% Store: retry_strategy_store
%%%
%%% Lifecycle:
%%%   [fresh] -> initiate_retry -> INITIATED
%%%   INITIATED -> attempt_retry -> RETRYING (increments attempt_count)
%%%   RETRYING -> attempt_retry -> RETRYING (up to max_attempts)
%%%   RETRYING -> succeed_retry -> SUCCEEDED
%%%   RETRYING -> exhaust_retry -> EXHAUSTED (when max_attempts reached)
%%%   INITIATED -> exhaust_retry -> EXHAUSTED
-module(retry_strategy_aggregate).
-behaviour(evoq_aggregate).

-include("retry_status.hrl").
-include("retry_state.hrl").

-export([init/1, execute/2, apply/2]).
-export([initial_state/0, apply_event/2]).
-export([flag_map/0]).

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?RS_FLAG_MAP.

init(_AggregateId) ->
    {ok, initial_state()}.

initial_state() ->
    #retry_state{}.

%% --- Execute ---

execute(#retry_state{status = 0}, Payload) ->
    case gf(command_type, Payload) of
        <<"initiate_retry">> ->
            maybe_initiate_retry:handle(Payload);
        _ ->
            {error, retry_not_initiated}
    end;

execute(#retry_state{status = S}, _Payload) when S band ?RS_EXHAUSTED =/= 0 ->
    {error, retry_exhausted};

execute(#retry_state{status = S}, _Payload) when S band ?RS_SUCCEEDED =/= 0 ->
    {error, retry_already_succeeded};

execute(#retry_state{status = S} = State, Payload) when S band ?RS_INITIATED =/= 0;
                                                          S band ?RS_RETRYING =/= 0 ->
    case gf(command_type, Payload) of
        <<"attempt_retry">> ->
            maybe_attempt_retry:handle(Payload, State);
        <<"exhaust_retry">> ->
            maybe_exhaust_retry:handle(Payload);
        <<"succeed_retry">> ->
            maybe_succeed_retry:handle(Payload);
        _ ->
            {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Apply ---

apply(State, Event) ->
    apply_event(Event, State).

apply_event(#{event_type := <<"retry_initiated_v1">>} = E, S) ->
    apply_initiated(E, S);
apply_event(#{event_type := <<"retry_attempted_v1">>} = E, S) ->
    apply_attempted(E, S);
apply_event(#{event_type := <<"retry_exhausted_v1">>} = E, S) ->
    apply_exhausted(E, S);
apply_event(#{event_type := <<"retry_succeeded_v1">>} = E, S) ->
    apply_succeeded(E, S);
apply_event(_E, S) -> S.

%% --- Apply helpers ---

apply_initiated(E, State) ->
    State#retry_state{
        session_id = gf(session_id, E),
        venture_id = gf(venture_id, E),
        agent_role = gf(agent_role, E),
        status = evoq_bit_flags:set(0, ?RS_INITIATED),
        max_attempts = gf(max_attempts, E, 3),
        last_failure = gf(failure_reason, E),
        initiated_at = gf(initiated_at, E, 0)
    }.

apply_attempted(E, #retry_state{status = Status, adjustments = Adj} = State) ->
    Adjustment = gf(adjustment, E, #{}),
    State#retry_state{
        status = evoq_bit_flags:set(Status, ?RS_RETRYING),
        attempt_count = gf(attempt_number, E, State#retry_state.attempt_count + 1),
        adjustments = Adj ++ [Adjustment],
        last_attempt_at = gf(attempted_at, E, 0)
    }.

apply_exhausted(_E, #retry_state{status = Status} = State) ->
    State#retry_state{
        status = evoq_bit_flags:set(Status, ?RS_EXHAUSTED)
    }.

apply_succeeded(_E, #retry_state{status = Status} = State) ->
    State#retry_state{
        status = evoq_bit_flags:set(Status, ?RS_SUCCEEDED)
    }.

%% --- Internal ---

gf(Key, Map) -> app_marthad_api_utils:get_field(Key, Map).
gf(Key, Map, Default) ->
    case app_marthad_api_utils:get_field(Key, Map) of
        undefined -> Default;
        V -> V
    end.
