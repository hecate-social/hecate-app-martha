%%% @doc Query: list ventures with pagination.
-module(get_ventures_page).

-export([get/1]).

-spec get(map()) -> {ok, [map()]} | {error, term()}.
get(Filters) ->
    Limit = maps:get(limit, Filters, 20),
    Offset = maps:get(offset, Filters, 0),
    case project_ventures_store:list_ventures() of
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
