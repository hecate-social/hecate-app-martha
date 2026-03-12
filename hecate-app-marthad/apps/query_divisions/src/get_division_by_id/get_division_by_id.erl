-module(get_division_by_id).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found}.
get(DivisionId) ->
    case project_divisions_store:get_division(DivisionId) of
        {ok, Division} -> {ok, enrich(DivisionId, Division)};
        Error          -> Error
    end.

enrich(Id, Division) ->
    {ok, Aggs}     = project_divisions_store:list_designed_aggregates(Id),
    {ok, Evts}     = project_divisions_store:list_designed_events(Id),
    {ok, Desks}    = project_divisions_store:list_planned_desks(Id),
    {ok, Deps}     = project_divisions_store:list_planned_dependencies(Id),
    {ok, Cards}    = project_divisions_store:list_kanban_cards(Id),
    {ok, Modules}  = project_divisions_store:list_generated_modules(Id),
    {ok, Tests}    = project_divisions_store:list_generated_tests(Id),
    {ok, Suites}   = project_divisions_store:list_test_suites(Id),
    {ok, Results}  = project_divisions_store:list_test_results(Id),
    {ok, Releases} = project_divisions_store:list_releases(Id),
    {ok, Stages}   = project_divisions_store:list_delivery_stages(Id),
    Division#{
        designed_aggregates  => Aggs,
        designed_events      => Evts,
        planned_desks        => Desks,
        planned_dependencies => Deps,
        kanban_cards         => Cards,
        generated_modules    => Modules,
        generated_tests      => Tests,
        test_suites          => Suites,
        test_results         => Results,
        releases             => Releases,
        delivery_stages      => Stages
    }.
