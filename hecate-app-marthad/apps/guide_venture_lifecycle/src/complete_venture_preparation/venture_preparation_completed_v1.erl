%%% @doc venture_preparation_completed_v1 event
%%% Emitted when venture knowledge preparation is complete.
%%% All research briefs have been contributed; the storm can begin.
-module(venture_preparation_completed_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_venture_id/1, get_completed_at/1]).

-record(venture_preparation_completed_v1, {
    venture_id   :: binary(),
    completed_at :: integer()
}).

-export_type([venture_preparation_completed_v1/0]).
-opaque venture_preparation_completed_v1() :: #venture_preparation_completed_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> venture_preparation_completed_v1().
new(#{venture_id := VentureId}) ->
    #venture_preparation_completed_v1{
        venture_id = VentureId,
        completed_at = erlang:system_time(millisecond)
    }.

-spec to_map(venture_preparation_completed_v1()) -> map().
to_map(#venture_preparation_completed_v1{} = E) ->
    #{
        event_type => <<"venture_preparation_completed_v1">>,
        venture_id => E#venture_preparation_completed_v1.venture_id,
        completed_at => E#venture_preparation_completed_v1.completed_at
    }.

-spec from_map(map()) -> {ok, venture_preparation_completed_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #venture_preparation_completed_v1{
                venture_id = VentureId,
                completed_at = get_value(completed_at, Map,
                    erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_venture_id(venture_preparation_completed_v1()) -> binary().
get_venture_id(#venture_preparation_completed_v1{venture_id = V}) -> V.

-spec get_completed_at(venture_preparation_completed_v1()) -> integer().
get_completed_at(#venture_preparation_completed_v1{completed_at = V}) -> V.

%% Internal
get_value(Key, Map) ->
    get_value(Key, Map, undefined).

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
