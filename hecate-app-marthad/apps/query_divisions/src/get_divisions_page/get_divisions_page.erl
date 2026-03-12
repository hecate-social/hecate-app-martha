-module(get_divisions_page).
-export([get/2]).

-spec get(non_neg_integer(), non_neg_integer()) -> {ok, [map()]}.
get(Limit, Offset) ->
    {ok, All} = project_divisions_store:list_divisions(),
    Page = lists:sublist(safe_drop(Offset, All), max(1, Limit)),
    {ok, Page}.

safe_drop(0, List) -> List;
safe_drop(N, List) when N >= length(List) -> [];
safe_drop(N, List) -> lists:nthtail(N, List).
