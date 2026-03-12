-module(get_division_planning).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found}.
get(DivisionId) ->
    case project_divisions_store:get_division(DivisionId) of
        {ok, Division} ->
            {ok, #{
                division_id => maps:get(division_id, Division),
                planning_status => maps:get(planning_status, Division, 0),
                planning_status_label => maps:get(planning_status_label, Division, <<>>),
                planning_opened_at => maps:get(planning_opened_at, Division, undefined)
            }};
        Error -> Error
    end.
