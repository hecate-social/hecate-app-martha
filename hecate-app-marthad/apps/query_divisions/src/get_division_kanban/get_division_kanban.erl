-module(get_division_kanban).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found}.
get(DivisionId) ->
    case project_divisions_store:get_division(DivisionId) of
        {ok, Division} ->
            {ok, Cards} = project_divisions_store:list_kanban_cards(DivisionId),
            {ok, #{
                division_id => maps:get(division_id, Division),
                kanban_status => maps:get(kanban_status, Division, 0),
                kanban_status_label => maps:get(kanban_status_label, Division, <<>>),
                cards => Cards
            }};
        Error -> Error
    end.
