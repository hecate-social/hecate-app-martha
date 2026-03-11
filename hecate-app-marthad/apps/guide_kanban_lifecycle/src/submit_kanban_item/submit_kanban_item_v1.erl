%%% @doc submit_kanban_item_v1 command
%%% Submits a new item to the kanban board.
-module(submit_kanban_item_v1).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([get_division_id/1, get_item_id/1, get_title/1,
         get_description/1, get_item_type/1, get_submitted_by/1]).

-record(submit_kanban_item_v1, {
    division_id  :: binary(),
    item_id      :: binary(),
    title        :: binary(),
    description  :: binary() | undefined,
    item_type    :: binary() | undefined,
    submitted_by :: binary() | undefined
}).

-export_type([submit_kanban_item_v1/0]).
-opaque submit_kanban_item_v1() :: #submit_kanban_item_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, submit_kanban_item_v1()} | {error, term()}.
new(#{division_id := DivisionId, item_id := ItemId, title := Title} = Params) ->
    {ok, #submit_kanban_item_v1{
        division_id = DivisionId,
        item_id = ItemId,
        title = Title,
        description = maps:get(description, Params, undefined),
        item_type = maps:get(item_type, Params, undefined),
        submitted_by = maps:get(submitted_by, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(submit_kanban_item_v1()) -> {ok, submit_kanban_item_v1()} | {error, term()}.
validate(#submit_kanban_item_v1{division_id = D}) when not is_binary(D); byte_size(D) =:= 0 ->
    {error, invalid_division_id};
validate(#submit_kanban_item_v1{item_id = I}) when not is_binary(I); byte_size(I) =:= 0 ->
    {error, invalid_item_id};
validate(#submit_kanban_item_v1{title = T}) when not is_binary(T); byte_size(T) =:= 0 ->
    {error, invalid_title};
validate(#submit_kanban_item_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(submit_kanban_item_v1()) -> map().
to_map(#submit_kanban_item_v1{} = Cmd) ->
    #{
        <<"command_type">> => <<"submit_kanban_item">>,
        <<"division_id">> => Cmd#submit_kanban_item_v1.division_id,
        <<"item_id">> => Cmd#submit_kanban_item_v1.item_id,
        <<"title">> => Cmd#submit_kanban_item_v1.title,
        <<"description">> => Cmd#submit_kanban_item_v1.description,
        <<"item_type">> => Cmd#submit_kanban_item_v1.item_type,
        <<"submitted_by">> => Cmd#submit_kanban_item_v1.submitted_by
    }.

-spec from_map(map()) -> {ok, submit_kanban_item_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    ItemId = get_value(item_id, Map),
    Title = get_value(title, Map),
    case {DivisionId, ItemId, Title} of
        {undefined, _, _} -> {error, missing_required_fields};
        {_, undefined, _} -> {error, missing_required_fields};
        {_, _, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #submit_kanban_item_v1{
                division_id = DivisionId,
                item_id = ItemId,
                title = Title,
                description = get_value(description, Map, undefined),
                item_type = get_value(item_type, Map, undefined),
                submitted_by = get_value(submitted_by, Map, undefined)
            }}
    end.

%% Accessors
-spec get_division_id(submit_kanban_item_v1()) -> binary().
get_division_id(#submit_kanban_item_v1{division_id = V}) -> V.

-spec get_item_id(submit_kanban_item_v1()) -> binary().
get_item_id(#submit_kanban_item_v1{item_id = V}) -> V.

-spec get_title(submit_kanban_item_v1()) -> binary().
get_title(#submit_kanban_item_v1{title = V}) -> V.

-spec get_description(submit_kanban_item_v1()) -> binary() | undefined.
get_description(#submit_kanban_item_v1{description = V}) -> V.

-spec get_item_type(submit_kanban_item_v1()) -> binary() | undefined.
get_item_type(#submit_kanban_item_v1{item_type = V}) -> V.

-spec get_submitted_by(submit_kanban_item_v1()) -> binary() | undefined.
get_submitted_by(#submit_kanban_item_v1{submitted_by = V}) -> V.

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
