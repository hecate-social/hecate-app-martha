-module(get_stormings_by_venture).
-export([get/1]).

-spec get(binary()) -> {ok, [map()]} | {error, term()}.
get(VentureId) ->
    Sql = "SELECT division_id, venture_id, context_name, status, status_label, "
          "initiated_at, initiated_by "
          "FROM division_stormings WHERE venture_id = ?1 "
          "ORDER BY initiated_at DESC",
    case project_division_stormings_store:query(Sql, [VentureId]) of
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
