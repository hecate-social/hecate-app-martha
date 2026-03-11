%%% @doc pick_kanban_item_v1 command
%%% Picks (claims) a kanban item for work.
-module(pick_kanban_item_v1).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([get_division_id/1, get_item_id/1, get_picked_by/1]).

-record(pick_kanban_item_v1, {
    division_id :: binary(),
    item_id     :: binary(),
    picked_by   :: binary() | undefined
}).

-export_type([pick_kanban_item_v1/0]).
-opaque pick_kanban_item_v1() :: #pick_kanban_item_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, pick_kanban_item_v1()} | {error, term()}.
new(#{division_id := DivisionId, item_id := ItemId} = Params) ->
    {ok, #pick_kanban_item_v1{
        division_id = DivisionId,
        item_id = ItemId,
        picked_by = maps:get(picked_by, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(pick_kanban_item_v1()) -> {ok, pick_kanban_item_v1()} | {error, term()}.
validate(#pick_kanban_item_v1{division_id = D}) when not is_binary(D); byte_size(D) =:= 0 ->
    {error, invalid_division_id};
validate(#pick_kanban_item_v1{item_id = I}) when not is_binary(I); byte_size(I) =:= 0 ->
    {error, invalid_item_id};
validate(#pick_kanban_item_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(pick_kanban_item_v1()) -> map().
to_map(#pick_kanban_item_v1{} = Cmd) ->
    #{
        <<"command_type">> => <<"pick_kanban_item">>,
        <<"division_id">> => Cmd#pick_kanban_item_v1.division_id,
        <<"item_id">> => Cmd#pick_kanban_item_v1.item_id,
        <<"picked_by">> => Cmd#pick_kanban_item_v1.picked_by
    }.

-spec from_map(map()) -> {ok, pick_kanban_item_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    ItemId = get_value(item_id, Map),
    case {DivisionId, ItemId} of
        {undefined, _} -> {error, missing_required_fields};
        {_, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #pick_kanban_item_v1{
                division_id = DivisionId,
                item_id = ItemId,
                picked_by = get_value(picked_by, Map, undefined)
            }}
    end.

-spec get_division_id(pick_kanban_item_v1()) -> binary().
get_division_id(#pick_kanban_item_v1{division_id = V}) -> V.

-spec get_item_id(pick_kanban_item_v1()) -> binary().
get_item_id(#pick_kanban_item_v1{item_id = V}) -> V.

-spec get_picked_by(pick_kanban_item_v1()) -> binary() | undefined.
get_picked_by(#pick_kanban_item_v1{picked_by = V}) -> V.

%% Internal
get_value(Key, Map) -> get_value(Key, Map, undefined).
get_value(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> case maps:find(BinKey, Map) of {ok, V} -> V; error -> Default end
    end.
