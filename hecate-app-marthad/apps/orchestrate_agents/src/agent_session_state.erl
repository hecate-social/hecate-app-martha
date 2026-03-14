%%% @doc Agent session state module — implements evoq_state behaviour.
%%%
%%% Owns the agent_session_state record, initial state creation, event folding,
%%% and serialization. Extracted from agent_orchestration_aggregate to separate
%%% state concerns from command validation.
%%% @end
-module(agent_session_state).

-behaviour(evoq_state).

-include("agent_orchestration_status.hrl").
-include("agent_session_state.hrl").

-export([new/1, apply_event/2, to_map/1]).

-type state() :: #agent_session_state{}.
-export_type([state/0]).

%% --- evoq_state callbacks ---

-spec new(binary()) -> state().
new(_AggregateId) ->
    #agent_session_state{}.

-spec apply_event(state(), map()) -> state().

%% Normalize atom event_type to binary (from typed evoq_event modules)
apply_event(S, #{event_type := Type} = E) when is_atom(Type) ->
    apply_event(S, E#{event_type := atom_to_binary(Type, utf8)});

%% ── Visionary ──
apply_event(S, #{event_type := <<"visionary_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"visionary_completed_v1">>} = E)       -> apply_completed(E, S);
apply_event(S, #{event_type := <<"visionary_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Vision gate ──
apply_event(S, #{event_type := <<"vision_gate_escalated_v1">>} = E)       -> apply_gate_escalated(E, S);
apply_event(S, #{event_type := <<"vision_gate_passed_v1">>} = E)         -> apply_gate_passed(E, S);
apply_event(S, #{event_type := <<"vision_gate_rejected_v1">>} = E)       -> apply_gate_rejected(E, S);

%% ── Explorer ──
apply_event(S, #{event_type := <<"explorer_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"explorer_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"explorer_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Boundary gate ──
apply_event(S, #{event_type := <<"boundary_gate_escalated_v1">>} = E)      -> apply_gate_escalated(E, S);
apply_event(S, #{event_type := <<"boundary_gate_passed_v1">>} = E)        -> apply_gate_passed(E, S);
apply_event(S, #{event_type := <<"boundary_gate_rejected_v1">>} = E)      -> apply_gate_rejected(E, S);

%% ── Stormer ──
apply_event(S, #{event_type := <<"stormer_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"stormer_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"stormer_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Design gate ──
apply_event(S, #{event_type := <<"design_gate_escalated_v1">>} = E)      -> apply_gate_escalated(E, S);
apply_event(S, #{event_type := <<"design_gate_passed_v1">>} = E)        -> apply_gate_passed(E, S);
apply_event(S, #{event_type := <<"design_gate_rejected_v1">>} = E)      -> apply_gate_rejected(E, S);

%% ── Reviewer ──
apply_event(S, #{event_type := <<"reviewer_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"reviewer_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"reviewer_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Review gate ──
apply_event(S, #{event_type := <<"review_gate_escalated_v1">>} = E)      -> apply_gate_escalated(E, S);
apply_event(S, #{event_type := <<"review_gate_passed_v1">>} = E)        -> apply_gate_passed(E, S);
apply_event(S, #{event_type := <<"review_gate_rejected_v1">>} = E)      -> apply_gate_rejected(E, S);

%% ── Architect ──
apply_event(S, #{event_type := <<"architect_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"architect_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"architect_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Erlang Coder ──
apply_event(S, #{event_type := <<"erlang_coder_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"erlang_coder_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"erlang_coder_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Svelte Coder ──
apply_event(S, #{event_type := <<"svelte_coder_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"svelte_coder_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"svelte_coder_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── SQL Coder ──
apply_event(S, #{event_type := <<"sql_coder_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"sql_coder_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"sql_coder_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Tester ──
apply_event(S, #{event_type := <<"tester_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"tester_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"tester_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Delivery Manager ──
apply_event(S, #{event_type := <<"delivery_manager_initiated_v1">>} = E)       -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"delivery_manager_completed_v1">>} = E)      -> apply_completed(E, S);
apply_event(S, #{event_type := <<"delivery_manager_failed_v1">>} = E)         -> apply_failed(E, S);

%% ── Coordinator (conversational) ──
apply_event(S, #{event_type := <<"coordinator_initiated_v1">>} = E)              -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"coordinator_turn_completed_v1">>} = E)         -> apply_turn_completed(E, S);
apply_event(S, #{event_type := <<"coordinator_input_received_v1">>} = E)         -> apply_input_received(E, S);
apply_event(S, #{event_type := <<"coordinator_completed_v1">>} = E)              -> apply_completed(E, S);
apply_event(S, #{event_type := <<"coordinator_failed_v1">>} = E)                 -> apply_failed(E, S);

%% ── Mentor (conversational) ──
apply_event(S, #{event_type := <<"mentor_initiated_v1">>} = E)              -> apply_initiated(E, S);
apply_event(S, #{event_type := <<"mentor_turn_completed_v1">>} = E)         -> apply_turn_completed(E, S);
apply_event(S, #{event_type := <<"mentor_input_received_v1">>} = E)         -> apply_input_received(E, S);
apply_event(S, #{event_type := <<"mentor_completed_v1">>} = E)              -> apply_completed(E, S);
apply_event(S, #{event_type := <<"mentor_failed_v1">>} = E)                 -> apply_failed(E, S);

%% ── Archive (role-agnostic) ──
apply_event(S, #{event_type := <<"agent_session_archived_v1">>} = E)      -> apply_archived(E, S);

%% Unknown — ignore
apply_event(S, _E) -> S.

%% --- Apply helpers (shared — lifecycle state changes are role-agnostic) ---

apply_initiated(E, State) ->
    State#agent_session_state{
        session_id = gf(session_id, E),
        agent_role = gf(agent_role, E),
        venture_id = gf(venture_id, E),
        division_id = gf(division_id, E),
        tier = gf(tier, E),
        model = gf(model, E),
        status = evoq_bit_flags:set(0, ?AO_INITIATED),
        initiated_at = gf(initiated_at, E),
        initiated_by = gf(initiated_by, E)
    }.

apply_completed(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_COMPLETED),
        completed_at = gf(completed_at, E),
        tokens_in = State#agent_session_state.tokens_in + gf(tokens_in, E, 0),
        tokens_out = State#agent_session_state.tokens_out + gf(tokens_out, E, 0),
        notation_output = gf(notation_output, E),
        parsed_terms = gf(parsed_terms, E, [])
    }.

apply_turn_completed(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_AWAITING_INPUT),
        turn_count = gf(turn_number, E, State#agent_session_state.turn_count + 1),
        last_agent_output = gf(agent_output, E),
        tokens_in = State#agent_session_state.tokens_in + gf(tokens_in, E, 0),
        tokens_out = State#agent_session_state.tokens_out + gf(tokens_out, E, 0)
    }.

apply_input_received(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:unset(Status, ?AO_AWAITING_INPUT),
        last_input = gf(input_content, E),
        last_input_by = gf(input_by, E)
    }.

apply_failed(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_FAILED),
        failed_at = gf(failed_at, E),
        error_reason = gf(error_reason, E)
    }.

apply_gate_escalated(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_GATE_PENDING),
        gate_name = gf(gate_name, E),
        escalated_at = gf(escalated_at, E)
    }.

apply_gate_passed(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_GATE_PASSED),
        gate_verdict = <<"passed">>,
        gate_decided_by = gf(passed_by, E),
        gate_decided_at = gf(passed_at, E)
    }.

apply_gate_rejected(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_GATE_REJECTED),
        gate_verdict = <<"rejected">>,
        gate_decided_by = gf(rejected_by, E),
        gate_decided_at = gf(rejected_at, E),
        rejection_reason = gf(rejection_reason, E)
    }.

apply_archived(E, #agent_session_state{status = Status} = State) ->
    State#agent_session_state{
        status = evoq_bit_flags:set(Status, ?AO_ARCHIVED),
        archived_at = gf(archived_at, E)
    }.

%% --- Serialization ---

-spec to_map(state()) -> map().
to_map(#agent_session_state{} = S) ->
    #{
        session_id       => S#agent_session_state.session_id,
        agent_role       => S#agent_session_state.agent_role,
        venture_id       => S#agent_session_state.venture_id,
        division_id      => S#agent_session_state.division_id,
        kanban_item_id   => S#agent_session_state.kanban_item_id,
        tier             => S#agent_session_state.tier,
        model            => S#agent_session_state.model,
        status           => S#agent_session_state.status,
        initiated_at     => S#agent_session_state.initiated_at,
        initiated_by     => S#agent_session_state.initiated_by,
        completed_at     => S#agent_session_state.completed_at,
        failed_at        => S#agent_session_state.failed_at,
        archived_at      => S#agent_session_state.archived_at,
        tokens_in        => S#agent_session_state.tokens_in,
        tokens_out       => S#agent_session_state.tokens_out,
        notation_output  => S#agent_session_state.notation_output,
        parsed_terms     => S#agent_session_state.parsed_terms,
        gate_name        => S#agent_session_state.gate_name,
        gate_verdict     => S#agent_session_state.gate_verdict,
        gate_decided_by  => S#agent_session_state.gate_decided_by,
        gate_decided_at  => S#agent_session_state.gate_decided_at,
        escalated_at     => S#agent_session_state.escalated_at,
        rejection_reason => S#agent_session_state.rejection_reason,
        error_reason     => S#agent_session_state.error_reason,
        turn_count       => S#agent_session_state.turn_count,
        last_agent_output => S#agent_session_state.last_agent_output,
        last_input       => S#agent_session_state.last_input,
        last_input_by    => S#agent_session_state.last_input_by
    }.

%% --- Internal ---

gf(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.

gf(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
