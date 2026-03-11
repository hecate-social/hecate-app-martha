%%% @doc Projection: kanban_item_submitted_v1 -> kanban_items table
-module(kanban_item_submitted_v1_to_sqlite_kanban_items).

-export([project/1]).

project(Event) ->
    ItemId = get(item_id, Event),
    DivisionId = get(division_id, Event),
    Title = get(title, Event),
    Description = get(description, Event),
    ItemType = get(item_type, Event),
    SubmittedBy = get(submitted_by, Event),
    SubmittedAt = get(submitted_at, Event),
    logger:info("[PROJECTION] ~s: projecting ~s", [?MODULE, ItemId]),
    Sql = "INSERT OR REPLACE INTO kanban_items "
          "(item_id, division_id, title, description, item_type, status_text, "
          "submitted_by, submitted_at) "
          "VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
    project_division_kanbans_store:execute(Sql, [
        ItemId, DivisionId, Title, Description, ItemType,
        <<"ready">>, SubmittedBy, SubmittedAt
    ]).

get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
