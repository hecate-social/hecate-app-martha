%%% @doc storm_participant_unregistered_v1 event
%%% Emitted when a participant is removed from the storm roster.
-module(storm_participant_unregistered_v1).

-behaviour(evoq_event).

-export([new/1, from_map/1, to_map/1, event_type/0]).
-export([get_venture_id/1, get_participant_id/1, get_unregistered_at/1]).

-record(storm_participant_unregistered_v1, {
    venture_id      :: binary(),
    participant_id  :: binary(),
    unregistered_at :: non_neg_integer()
}).

-export_type([storm_participant_unregistered_v1/0]).
-opaque storm_participant_unregistered_v1() :: #storm_participant_unregistered_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec event_type() -> atom().
event_type() -> storm_participant_unregistered_v1.

-spec new(map()) -> storm_participant_unregistered_v1().
new(#{venture_id := VentureId, participant_id := ParticipantId} = Params) ->
    #storm_participant_unregistered_v1{
        venture_id = VentureId,
        participant_id = ParticipantId,
        unregistered_at = maps:get(unregistered_at, Params, erlang:system_time(millisecond))
    }.

-spec to_map(storm_participant_unregistered_v1()) -> map().
to_map(#storm_participant_unregistered_v1{} = E) ->
    #{
        event_type => storm_participant_unregistered_v1,
        venture_id => E#storm_participant_unregistered_v1.venture_id,
        participant_id => E#storm_participant_unregistered_v1.participant_id,
        unregistered_at => E#storm_participant_unregistered_v1.unregistered_at
    }.

-spec from_map(map()) -> {ok, storm_participant_unregistered_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #storm_participant_unregistered_v1{
                venture_id = VentureId,
                participant_id = get_value(participant_id, Map),
                unregistered_at = get_value(unregistered_at, Map, erlang:system_time(millisecond))
            }}
    end.

-spec get_venture_id(storm_participant_unregistered_v1()) -> binary().
get_venture_id(#storm_participant_unregistered_v1{venture_id = V}) -> V.

-spec get_participant_id(storm_participant_unregistered_v1()) -> binary().
get_participant_id(#storm_participant_unregistered_v1{participant_id = V}) -> V.

-spec get_unregistered_at(storm_participant_unregistered_v1()) -> non_neg_integer().
get_unregistered_at(#storm_participant_unregistered_v1{unregistered_at = V}) -> V.

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
