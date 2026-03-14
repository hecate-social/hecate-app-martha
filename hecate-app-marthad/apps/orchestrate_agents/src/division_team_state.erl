%%% @doc Division team state module — implements evoq_state behaviour.
%%%
%%% Owns the division_team_state record, initial state creation, event folding,
%%% and serialization. Extracted from division_team_aggregate.
-module(division_team_state).

-behaviour(evoq_state).

-include("division_team_status.hrl").
-include("division_team_state.hrl").

-export([new/1, apply_event/2, to_map/1]).

-type state() :: #division_team_state{}.
-export_type([state/0]).

%% --- evoq_state callbacks ---

-spec new(binary()) -> state().
new(_AggregateId) ->
    #division_team_state{}.

-spec apply_event(state(), map()) -> state().

%% Normalize atom event_type to binary (from typed evoq_event modules)
apply_event(S, #{event_type := Type} = E) when is_atom(Type) ->
    apply_event(S, E#{event_type := atom_to_binary(Type, utf8)});

apply_event(S, #{event_type := <<"team_formed_v1">>} = E)                -> apply_formed(E, S);
apply_event(S, #{event_type := <<"agent_assigned_to_team_v1">>} = E)     -> apply_assigned(E, S);
apply_event(S, #{event_type := <<"team_activated_v1">>} = E)             -> apply_activated(E, S);
apply_event(S, #{event_type := <<"team_disbanded_v1">>} = E)             -> apply_disbanded(E, S);
%% Unknown — ignore
apply_event(S, _E) -> S.

%% --- to_map ---

-spec to_map(state()) -> map().
to_map(#division_team_state{} = S) ->
    #{
        division_id   => S#division_team_state.division_id,
        venture_id    => S#division_team_state.venture_id,
        status        => S#division_team_state.status,
        planned_roles => S#division_team_state.planned_roles,
        members       => S#division_team_state.members,
        formed_at     => S#division_team_state.formed_at,
        formed_by     => S#division_team_state.formed_by,
        activated_at  => S#division_team_state.activated_at,
        disbanded_at  => S#division_team_state.disbanded_at
    }.

%% --- Apply helpers ---

apply_formed(E, _State) ->
    #division_team_state{
        division_id = get_value(division_id, E),
        venture_id = get_value(venture_id, E),
        status = evoq_bit_flags:set(0, ?DT_FORMED),
        planned_roles = get_value(planned_roles, E, []),
        formed_at = get_value(formed_at, E),
        formed_by = get_value(formed_by, E)
    }.

apply_assigned(E, #division_team_state{members = Members} = State) ->
    NewMember = #{
        agent_role => get_value(agent_role, E),
        session_id => get_value(session_id, E)
    },
    State#division_team_state{
        members = Members ++ [NewMember]
    }.

apply_activated(E, #division_team_state{status = Status} = State) ->
    State#division_team_state{
        status = evoq_bit_flags:set(Status, ?DT_ACTIVE),
        activated_at = get_value(activated_at, E)
    }.

apply_disbanded(E, #division_team_state{status = Status} = State) ->
    State#division_team_state{
        status = evoq_bit_flags:set(Status, ?DT_DISBANDED),
        disbanded_at = get_value(disbanded_at, E)
    }.

%% --- Internal ---

get_value(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.

get_value(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
