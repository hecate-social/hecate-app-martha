-module(get_kanban_items_by_division).
-export([get/1, get/2]).

-spec get(binary()) -> {ok, [map()]} | {error, term()}.
get(DivisionId) ->
    get(DivisionId, #{}).

-spec get(binary(), map()) -> {ok, [map()]} | {error, term()}.
get(DivisionId, Filters) ->
    StatusFilter = maps:get(status, Filters, undefined),
    {Sql, Params} = case StatusFilter of
        undefined ->
            {"SELECT item_id, division_id, title, description, item_type, status_text, "
             "submitted_by, submitted_at, picked_by, picked_at, completed_at, return_reason "
             "FROM kanban_items WHERE division_id = ?1 "
             "ORDER BY submitted_at ASC",
             [DivisionId]};
        _ ->
            {"SELECT item_id, division_id, title, description, item_type, status_text, "
             "submitted_by, submitted_at, picked_by, picked_at, completed_at, return_reason "
             "FROM kanban_items WHERE division_id = ?1 AND status_text = ?2 "
             "ORDER BY submitted_at ASC",
             [DivisionId, StatusFilter]}
    end,
    case project_division_kanbans_store:query(Sql, Params) of
        {ok, Rows} ->
            {ok, [item_to_map(R) || R <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

item_to_map([ItemId, DivId, Title, Desc, IType, StatusText,
             SubmBy, SubmAt, PickBy, PickAt, CompAt, RetReason]) ->
    #{
        item_id => ItemId,
        division_id => DivId,
        title => Title,
        description => Desc,
        item_type => IType,
        status_text => StatusText,
        submitted_by => SubmBy,
        submitted_at => SubmAt,
        picked_by => PickBy,
        picked_at => PickAt,
        completed_at => CompAt,
        return_reason => RetReason
    }.
