%%% @doc unregister_storm_participant_v1 command
%%% Removes a participant from the storm meditation roster.
-module(unregister_storm_participant_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1, command_type/0]).
-export([get_venture_id/1, get_participant_id/1]).

-record(unregister_storm_participant_v1, {
    venture_id     :: binary(),
    participant_id :: binary()
}).

-export_type([unregister_storm_participant_v1/0]).
-opaque unregister_storm_participant_v1() :: #unregister_storm_participant_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec command_type() -> atom().
command_type() -> unregister_storm_participant_v1.

-spec new(map()) -> {ok, unregister_storm_participant_v1()} | {error, term()}.
new(#{venture_id := VentureId, participant_id := ParticipantId}) ->
    Cmd = #unregister_storm_participant_v1{
        venture_id = VentureId,
        participant_id = ParticipantId
    },
    case validate(Cmd) of
        ok -> {ok, Cmd};
        {error, _} = Err -> Err
    end;
new(_) ->
    {error, missing_required_fields}.

-spec validate(unregister_storm_participant_v1()) -> ok | {error, term()}.
validate(#unregister_storm_participant_v1{venture_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, venture_id}};
validate(#unregister_storm_participant_v1{participant_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, participant_id}};
validate(_) -> ok.

-spec to_map(unregister_storm_participant_v1()) -> map().
to_map(#unregister_storm_participant_v1{venture_id = V, participant_id = P}) ->
    #{
        command_type => unregister_storm_participant_v1,
        venture_id => V,
        participant_id => P
    }.

-spec from_map(map()) -> {ok, unregister_storm_participant_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    ParticipantId = get_value(participant_id, Map),
    case {VentureId, ParticipantId} of
        {undefined, _} -> {error, missing_required_fields};
        {_, undefined} -> {error, missing_required_fields};
        _ -> new(#{venture_id => VentureId, participant_id => ParticipantId})
    end.

-spec get_venture_id(unregister_storm_participant_v1()) -> binary().
get_venture_id(#unregister_storm_participant_v1{venture_id = V}) -> V.

-spec get_participant_id(unregister_storm_participant_v1()) -> binary().
get_participant_id(#unregister_storm_participant_v1{participant_id = V}) -> V.

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
