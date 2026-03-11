%%% @doc storming_archived_v1 event
%%% Emitted when a storming session is archived.
-module(storming_archived_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_division_id/1, get_archived_at/1]).

-record(storming_archived_v1, {
    division_id :: binary(),
    archived_at :: integer()
}).

-export_type([storming_archived_v1/0]).
-opaque storming_archived_v1() :: #storming_archived_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> storming_archived_v1().
new(#{division_id := DivisionId}) ->
    #storming_archived_v1{
        division_id = DivisionId,
        archived_at = erlang:system_time(millisecond)
    }.

-spec to_map(storming_archived_v1()) -> map().
to_map(#storming_archived_v1{} = E) ->
    #{
        <<"event_type">> => <<"storming_archived_v1">>,
        <<"division_id">> => E#storming_archived_v1.division_id,
        <<"archived_at">> => E#storming_archived_v1.archived_at
    }.

-spec from_map(map()) -> {ok, storming_archived_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    case DivisionId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #storming_archived_v1{
                division_id = DivisionId,
                archived_at = get_value(archived_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_division_id(storming_archived_v1()) -> binary().
get_division_id(#storming_archived_v1{division_id = V}) -> V.

-spec get_archived_at(storming_archived_v1()) -> integer().
get_archived_at(#storming_archived_v1{archived_at = V}) -> V.

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
