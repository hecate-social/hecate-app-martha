-module(get_kanban_by_division).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found | term()}.
get(DivisionId) ->
    case get_board(DivisionId) of
        {ok, Board} ->
            Items = get_items(DivisionId),
            {ok, Board#{items => Items}};
        Error ->
            Error
    end.

get_board(DivisionId) ->
    Sql = "SELECT division_id, venture_id, context_name, status, status_label, "
          "initiated_at, initiated_by "
          "FROM division_kanbans WHERE division_id = ?1",
    case project_division_kanbans_store:query(Sql, [DivisionId]) of
        {ok, [[DId, VId, CName, Status, SLabel, IAt, IBy]]} ->
            {ok, #{
                division_id => DId,
                venture_id => VId,
                context_name => CName,
                status => Status,
                status_label => SLabel,
                initiated_at => IAt,
                initiated_by => IBy
            }};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

get_items(DivisionId) ->
    Sql = "SELECT item_id, division_id, title, description, item_type, status_text, "
          "submitted_by, submitted_at, picked_by, picked_at, completed_at, return_reason "
          "FROM kanban_items WHERE division_id = ?1 "
          "ORDER BY submitted_at ASC",
    case project_division_kanbans_store:query(Sql, [DivisionId]) of
        {ok, Rows} ->
            [item_to_map(R) || R <- Rows];
        _ -> []
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
