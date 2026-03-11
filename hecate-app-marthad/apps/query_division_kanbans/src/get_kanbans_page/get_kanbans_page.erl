-module(get_kanbans_page).
-export([get/1]).

-spec get(map()) -> {ok, [map()]} | {error, term()}.
get(Filters) ->
    Limit = maps:get(limit, Filters, 50),
    Offset = maps:get(offset, Filters, 0),
    Sql = "SELECT division_id, venture_id, context_name, status, status_label, "
          "initiated_at, initiated_by "
          "FROM division_kanbans "
          "ORDER BY initiated_at DESC "
          "LIMIT ?1 OFFSET ?2",
    case project_division_kanbans_store:query(Sql, [Limit, Offset]) of
        {ok, Rows} ->
            {ok, [row_to_map(R) || R <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

row_to_map([DivisionId, VentureId, ContextName, Status, StatusLabel,
            InitiatedAt, InitiatedBy]) ->
    #{
        division_id => DivisionId,
        venture_id => VentureId,
        context_name => ContextName,
        status => Status,
        status_label => StatusLabel,
        initiated_at => InitiatedAt,
        initiated_by => InitiatedBy
    }.
