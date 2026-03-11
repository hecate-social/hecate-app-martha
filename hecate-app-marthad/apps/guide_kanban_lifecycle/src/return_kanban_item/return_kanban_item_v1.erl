%%% @doc return_kanban_item_v1 command
-module(return_kanban_item_v1).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([get_division_id/1, get_item_id/1, get_reason/1]).

-record(return_kanban_item_v1, {
    division_id :: binary(),
    item_id     :: binary(),
    reason      :: binary() | undefined
}).

-export_type([return_kanban_item_v1/0]).
-opaque return_kanban_item_v1() :: #return_kanban_item_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, return_kanban_item_v1()} | {error, term()}.
new(#{division_id := DivisionId, item_id := ItemId} = Params) ->
    {ok, #return_kanban_item_v1{
        division_id = DivisionId,
        item_id = ItemId,
        reason = maps:get(reason, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(return_kanban_item_v1()) -> {ok, return_kanban_item_v1()} | {error, term()}.
validate(#return_kanban_item_v1{division_id = D}) when not is_binary(D); byte_size(D) =:= 0 ->
    {error, invalid_division_id};
validate(#return_kanban_item_v1{item_id = I}) when not is_binary(I); byte_size(I) =:= 0 ->
    {error, invalid_item_id};
validate(#return_kanban_item_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(return_kanban_item_v1()) -> map().
to_map(#return_kanban_item_v1{} = Cmd) ->
    #{
        <<"command_type">> => <<"return_kanban_item">>,
        <<"division_id">> => Cmd#return_kanban_item_v1.division_id,
        <<"item_id">> => Cmd#return_kanban_item_v1.item_id,
        <<"reason">> => Cmd#return_kanban_item_v1.reason
    }.

-spec from_map(map()) -> {ok, return_kanban_item_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    ItemId = get_value(item_id, Map),
    case {DivisionId, ItemId} of
        {undefined, _} -> {error, missing_required_fields};
        {_, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #return_kanban_item_v1{
                division_id = DivisionId,
                item_id = ItemId,
                reason = get_value(reason, Map, undefined)
            }}
    end.

-spec get_division_id(return_kanban_item_v1()) -> binary().
get_division_id(#return_kanban_item_v1{division_id = V}) -> V.
-spec get_item_id(return_kanban_item_v1()) -> binary().
get_item_id(#return_kanban_item_v1{item_id = V}) -> V.
-spec get_reason(return_kanban_item_v1()) -> binary() | undefined.
get_reason(#return_kanban_item_v1{reason = V}) -> V.

%% Internal
get_value(Key, Map) -> get_value(Key, Map, undefined).
get_value(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> case maps:find(BinKey, Map) of {ok, V} -> V; error -> Default end
    end.
