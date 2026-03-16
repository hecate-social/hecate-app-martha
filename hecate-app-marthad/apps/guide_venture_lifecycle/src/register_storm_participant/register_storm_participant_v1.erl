%%% @doc register_storm_participant_v1 command
%%% Registers a participant for a Big Picture Event Storming session.
-module(register_storm_participant_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1, command_type/0]).
-export([get_venture_id/1, get_participant_id/1, get_role/1, get_custom_instructions/1]).

-record(register_storm_participant_v1, {
    venture_id          :: binary(),
    participant_id      :: binary(),
    role                :: binary(),
    custom_instructions :: binary() | undefined
}).

-export_type([register_storm_participant_v1/0]).
-opaque register_storm_participant_v1() :: #register_storm_participant_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec command_type() -> atom().
command_type() -> register_storm_participant_v1.

-spec new(map()) -> {ok, register_storm_participant_v1()} | {error, term()}.
new(#{venture_id := VentureId, participant_id := ParticipantId, role := Role} = Params) ->
    Cmd = #register_storm_participant_v1{
        venture_id = VentureId,
        participant_id = ParticipantId,
        role = Role,
        custom_instructions = maps:get(custom_instructions, Params, undefined)
    },
    case validate(Cmd) of
        ok -> {ok, Cmd};
        {error, _} = Err -> Err
    end;
new(_) ->
    {error, missing_required_fields}.

-spec validate(register_storm_participant_v1()) -> ok | {error, term()}.
validate(#register_storm_participant_v1{venture_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, venture_id}};
validate(#register_storm_participant_v1{participant_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, participant_id}};
validate(#register_storm_participant_v1{role = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, role}};
validate(_) -> ok.

-spec to_map(register_storm_participant_v1()) -> map().
to_map(#register_storm_participant_v1{} = C) ->
    #{
        command_type => register_storm_participant_v1,
        venture_id => C#register_storm_participant_v1.venture_id,
        participant_id => C#register_storm_participant_v1.participant_id,
        role => C#register_storm_participant_v1.role,
        custom_instructions => C#register_storm_participant_v1.custom_instructions
    }.

-spec from_map(map()) -> {ok, register_storm_participant_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    ParticipantId = case get_value(participant_id, Map) of
        undefined -> evoq_id:generate();
        V0 -> V0
    end,
    Role = get_value(role, Map),
    CustomInstructions = get_value(custom_instructions, Map),
    case {VentureId, Role} of
        {undefined, _} -> {error, missing_required_fields};
        {_, undefined} -> {error, missing_required_fields};
        _ -> new(#{
            venture_id => VentureId,
            participant_id => ParticipantId,
            role => Role,
            custom_instructions => CustomInstructions
        })
    end.

-spec get_venture_id(register_storm_participant_v1()) -> binary().
get_venture_id(#register_storm_participant_v1{venture_id = V}) -> V.

-spec get_participant_id(register_storm_participant_v1()) -> binary().
get_participant_id(#register_storm_participant_v1{participant_id = V}) -> V.

-spec get_role(register_storm_participant_v1()) -> binary().
get_role(#register_storm_participant_v1{role = V}) -> V.

-spec get_custom_instructions(register_storm_participant_v1()) -> binary() | undefined.
get_custom_instructions(#register_storm_participant_v1{custom_instructions = V}) -> V.

%% Internal
get_value(Key, Map) -> get_value(Key, Map, undefined).

get_value(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            case maps:find(BinKey, Map) of
                {ok, V} -> V;
                error -> Default
            end
    end.
