%%% @doc kanban_item_submitted_v1 event
%%% Emitted when a new item is submitted to the kanban board.
-module(kanban_item_submitted_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_division_id/1, get_item_id/1, get_title/1,
         get_description/1, get_item_type/1, get_submitted_by/1,
         get_submitted_at/1]).

-record(kanban_item_submitted_v1, {
    division_id  :: binary(),
    item_id      :: binary(),
    title        :: binary(),
    description  :: binary() | undefined,
    item_type    :: binary() | undefined,
    submitted_by :: binary() | undefined,
    submitted_at :: integer()
}).

-export_type([kanban_item_submitted_v1/0]).
-opaque kanban_item_submitted_v1() :: #kanban_item_submitted_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> kanban_item_submitted_v1().
new(#{division_id := DivisionId, item_id := ItemId, title := Title} = Params) ->
    #kanban_item_submitted_v1{
        division_id = DivisionId,
        item_id = ItemId,
        title = Title,
        description = maps:get(description, Params, undefined),
        item_type = maps:get(item_type, Params, undefined),
        submitted_by = maps:get(submitted_by, Params, undefined),
        submitted_at = erlang:system_time(millisecond)
    }.

-spec to_map(kanban_item_submitted_v1()) -> map().
to_map(#kanban_item_submitted_v1{} = E) ->
    #{
        <<"event_type">> => <<"kanban_item_submitted_v1">>,
        <<"division_id">> => E#kanban_item_submitted_v1.division_id,
        <<"item_id">> => E#kanban_item_submitted_v1.item_id,
        <<"title">> => E#kanban_item_submitted_v1.title,
        <<"description">> => E#kanban_item_submitted_v1.description,
        <<"item_type">> => E#kanban_item_submitted_v1.item_type,
        <<"submitted_by">> => E#kanban_item_submitted_v1.submitted_by,
        <<"submitted_at">> => E#kanban_item_submitted_v1.submitted_at
    }.

-spec from_map(map()) -> {ok, kanban_item_submitted_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    ItemId = get_value(item_id, Map),
    Title = get_value(title, Map),
    case {DivisionId, ItemId, Title} of
        {undefined, _, _} -> {error, invalid_event};
        {_, undefined, _} -> {error, invalid_event};
        {_, _, undefined} -> {error, invalid_event};
        _ ->
            {ok, #kanban_item_submitted_v1{
                division_id = DivisionId,
                item_id = ItemId,
                title = Title,
                description = get_value(description, Map, undefined),
                item_type = get_value(item_type, Map, undefined),
                submitted_by = get_value(submitted_by, Map, undefined),
                submitted_at = get_value(submitted_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_division_id(kanban_item_submitted_v1()) -> binary().
get_division_id(#kanban_item_submitted_v1{division_id = V}) -> V.

-spec get_item_id(kanban_item_submitted_v1()) -> binary().
get_item_id(#kanban_item_submitted_v1{item_id = V}) -> V.

-spec get_title(kanban_item_submitted_v1()) -> binary().
get_title(#kanban_item_submitted_v1{title = V}) -> V.

-spec get_description(kanban_item_submitted_v1()) -> binary() | undefined.
get_description(#kanban_item_submitted_v1{description = V}) -> V.

-spec get_item_type(kanban_item_submitted_v1()) -> binary() | undefined.
get_item_type(#kanban_item_submitted_v1{item_type = V}) -> V.

-spec get_submitted_by(kanban_item_submitted_v1()) -> binary() | undefined.
get_submitted_by(#kanban_item_submitted_v1{submitted_by = V}) -> V.

-spec get_submitted_at(kanban_item_submitted_v1()) -> integer().
get_submitted_at(#kanban_item_submitted_v1{submitted_at = V}) -> V.

%% Internal helper to get value with atom or binary key
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
