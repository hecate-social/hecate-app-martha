%%% @doc Projection: kanban_item_picked_v1 -> kanban_items table (update)
-module(kanban_item_picked_v1_to_sqlite_kanban_items).

-export([project/1]).

project(Event) ->
    ItemId = get(item_id, Event),
    PickedBy = get(picked_by, Event),
    PickedAt = get(picked_at, Event),
    logger:info("[PROJECTION] ~s: projecting ~s", [?MODULE, ItemId]),
    Sql = "UPDATE kanban_items SET status_text = ?1, picked_by = ?2, picked_at = ?3 "
          "WHERE item_id = ?4",
    project_division_kanbans_store:execute(Sql, [
        <<"in_progress">>, PickedBy, PickedAt, ItemId
    ]).

get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
