%%% @doc Division team aggregate — manages team lifecycle for a division.
%%%
%%% Stream: division-team-{division_id}
%%% Store: orchestration_store
%%%
%%% Lifecycle:
%%%   [fresh] -> form_team -> FORMED
%%%   FORMED -> assign_agent_to_team -> FORMED (accumulates members)
%%%   FORMED -> activate_team -> ACTIVE
%%%   ACTIVE -> disband_team -> DISBANDED
%%%   any (except DISBANDED) -> disband_team -> DISBANDED
-module(division_team_aggregate).

-behaviour(evoq_aggregate).

-include("division_team_status.hrl").
-include("division_team_state.hrl").

-export([init/1, execute/2, apply/2]).
-export([state_module/0, flag_map/0]).

-type state() :: #division_team_state{}.
-export_type([state/0]).

-spec state_module() -> module().
state_module() -> division_team_state.

-spec flag_map() -> evoq_bit_flags:flag_map().
flag_map() -> ?DT_FLAG_MAP.

%% --- Callbacks ---

-spec init(binary()) -> {ok, state()}.
init(AggregateId) ->
    {ok, division_team_state:new(AggregateId)}.

%% --- Execute ---

-spec execute(state(), map()) -> {ok, [map()]} | {error, term()}.

%% Fresh — only form_team allowed
execute(#division_team_state{status = 0}, Payload) ->
    case get_command_type(Payload) of
        <<"form_team">> -> execute_form_team(Payload);
        _ -> {error, team_not_formed}
    end;

%% Disbanded — nothing allowed
execute(#division_team_state{status = S}, _Payload) when S band ?DT_DISBANDED =/= 0 ->
    {error, team_disbanded};

%% Active — accept assign or disband
execute(#division_team_state{status = S} = State, Payload) when S band ?DT_ACTIVE =/= 0 ->
    case get_command_type(Payload) of
        <<"assign_agent_to_team">> -> execute_assign(Payload, State);
        <<"disband_team">>         -> execute_disband(Payload, State);
        _ -> {error, unknown_command}
    end;

%% Formed (not active) — accept assign, activate, or disband
execute(#division_team_state{status = S} = State, Payload) when S band ?DT_FORMED =/= 0 ->
    case get_command_type(Payload) of
        <<"assign_agent_to_team">> -> execute_assign(Payload, State);
        <<"activate_team">>        -> execute_activate(Payload, State);
        <<"disband_team">>         -> execute_disband(Payload, State);
        _ -> {error, unknown_command}
    end;

execute(_State, _Payload) ->
    {error, unknown_command}.

%% --- Command handlers ---

execute_form_team(Payload) ->
    {ok, Cmd} = form_team_v1:from_map(Payload),
    convert_events(
        maybe_form_team:handle(Cmd),
        fun team_formed_v1:to_map/1).

execute_assign(Payload, State) ->
    {ok, Cmd} = assign_agent_to_team_v1:from_map(Payload),
    convert_events(
        maybe_assign_agent_to_team:handle(Cmd, State),
        fun agent_assigned_to_team_v1:to_map/1).

execute_activate(Payload, State) ->
    {ok, Cmd} = activate_team_v1:from_map(Payload),
    convert_events(
        maybe_activate_team:handle(Cmd, State),
        fun team_activated_v1:to_map/1).

execute_disband(Payload, _State) ->
    {ok, Cmd} = disband_team_v1:from_map(Payload),
    convert_events(
        maybe_disband_team:handle(Cmd),
        fun team_disbanded_v1:to_map/1).

%% --- Apply ---

-spec apply(state(), map()) -> state().
apply(State, Event) ->
    division_team_state:apply_event(State, Event).

%% --- Internal ---

get_command_type(#{command_type := T}) when is_binary(T) -> T;
get_command_type(#{command_type := T}) when is_atom(T) -> atom_to_binary(T);
get_command_type(_) -> undefined.

convert_events({ok, Events}, ToMapFn) ->
    {ok, [ToMapFn(E) || E <- Events]};
convert_events({error, _} = Err, _) ->
    Err.
