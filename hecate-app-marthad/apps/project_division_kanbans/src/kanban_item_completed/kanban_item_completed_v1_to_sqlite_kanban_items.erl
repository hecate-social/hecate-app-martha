%%% @doc Projection: kanban_item_completed_v1 -> kanban_items table (update)
-module(kanban_item_completed_v1_to_sqlite_kanban_items).

-export([project/1]).

project(Event) ->
    ItemId = get(item_id, Event),
    CompletedAt = get(completed_at, Event),
    logger:info("[PROJECTION] ~s: projecting ~s", [?MODULE, ItemId]),
    Sql = "UPDATE kanban_items SET status_text = ?1, completed_at = ?2 "
          "WHERE item_id = ?3",
    project_division_kanbans_store:execute(Sql, [
        <<"done">>, CompletedAt, ItemId
    ]).

get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
