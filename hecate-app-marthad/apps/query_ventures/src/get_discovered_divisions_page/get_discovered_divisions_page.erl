%%% @doc Query: list discovered divisions for a venture with pagination.
-module(get_discovered_divisions_page).

-export([get/1]).

-spec get(map()) -> {ok, [map()]} | {error, term()}.
get(Filters) ->
    VentureId = maps:get(venture_id, Filters),
    Limit = maps:get(limit, Filters, 50),
    Offset = maps:get(offset, Filters, 0),
    case project_ventures_store:list_divisions_by_venture(VentureId) of
        {ok, All} ->
            Paginated = paginate(All, Offset, Limit),
            {ok, Paginated};
        {error, Reason} ->
            {error, Reason}
    end.

paginate(List, Offset, Limit) ->
    lists:sublist(safe_drop(List, Offset), Limit).

safe_drop(List, 0) -> List;
safe_drop([], _) -> [];
safe_drop([_ | T], N) -> safe_drop(T, N - 1).
