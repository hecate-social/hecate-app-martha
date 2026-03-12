-module(get_division_storming).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found}.
get(DivisionId) ->
    case project_divisions_store:get_division(DivisionId) of
        {ok, Division} ->
            {ok, Aggs} = project_divisions_store:list_designed_aggregates(DivisionId),
            {ok, Evts} = project_divisions_store:list_designed_events(DivisionId),
            {ok, Desks} = project_divisions_store:list_planned_desks(DivisionId),
            {ok, Deps} = project_divisions_store:list_planned_dependencies(DivisionId),
            {ok, #{
                division_id => maps:get(division_id, Division),
                storming_status => maps:get(storming_status, Division, 0),
                storming_status_label => maps:get(storming_status_label, Division, <<>>),
                designed_aggregates => Aggs,
                designed_events => Evts,
                planned_desks => Desks,
                planned_dependencies => Deps
            }};
        Error -> Error
    end.
