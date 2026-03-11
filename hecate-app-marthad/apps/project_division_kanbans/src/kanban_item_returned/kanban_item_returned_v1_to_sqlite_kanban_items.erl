%%% @doc Projection: kanban_item_returned_v1 -> kanban_items table (update)
-module(kanban_item_returned_v1_to_sqlite_kanban_items).

-export([project/1]).

project(Event) ->
    ItemId = get(item_id, Event),
    ReturnReason = get(return_reason, Event),
    logger:info("[PROJECTION] ~s: projecting ~s", [?MODULE, ItemId]),
    Sql = "UPDATE kanban_items SET status_text = ?1, picked_by = NULL, "
          "picked_at = NULL, return_reason = ?2 WHERE item_id = ?3",
    project_division_kanbans_store:execute(Sql, [
        <<"ready">>, ReturnReason, ItemId
    ]).

get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
