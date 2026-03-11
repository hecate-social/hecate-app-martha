%%% @doc kanban_item_completed_v1 event
-module(kanban_item_completed_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_division_id/1, get_item_id/1, get_completed_at/1]).

-record(kanban_item_completed_v1, {
    division_id  :: binary(),
    item_id      :: binary(),
    completed_at :: integer()
}).

-export_type([kanban_item_completed_v1/0]).
-opaque kanban_item_completed_v1() :: #kanban_item_completed_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> kanban_item_completed_v1().
new(#{division_id := DivisionId, item_id := ItemId}) ->
    #kanban_item_completed_v1{
        division_id = DivisionId,
        item_id = ItemId,
        completed_at = erlang:system_time(millisecond)
    }.

-spec to_map(kanban_item_completed_v1()) -> map().
to_map(#kanban_item_completed_v1{} = E) ->
    #{
        <<"event_type">> => <<"kanban_item_completed_v1">>,
        <<"division_id">> => E#kanban_item_completed_v1.division_id,
        <<"item_id">> => E#kanban_item_completed_v1.item_id,
        <<"completed_at">> => E#kanban_item_completed_v1.completed_at
    }.

-spec from_map(map()) -> {ok, kanban_item_completed_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    ItemId = get_value(item_id, Map),
    case {DivisionId, ItemId} of
        {undefined, _} -> {error, invalid_event};
        {_, undefined} -> {error, invalid_event};
        _ ->
            {ok, #kanban_item_completed_v1{
                division_id = DivisionId,
                item_id = ItemId,
                completed_at = get_value(completed_at, Map, erlang:system_time(millisecond))
            }}
    end.

-spec get_division_id(kanban_item_completed_v1()) -> binary().
get_division_id(#kanban_item_completed_v1{division_id = V}) -> V.
-spec get_item_id(kanban_item_completed_v1()) -> binary().
get_item_id(#kanban_item_completed_v1{item_id = V}) -> V.
-spec get_completed_at(kanban_item_completed_v1()) -> integer().
get_completed_at(#kanban_item_completed_v1{completed_at = V}) -> V.

%% Internal
get_value(Key, Map) -> get_value(Key, Map, undefined).
get_value(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> case maps:find(BinKey, Map) of {ok, V} -> V; error -> Default end
    end.
