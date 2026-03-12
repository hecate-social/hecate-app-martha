-module(get_division_crafting).
-export([get/1]).

-spec get(binary()) -> {ok, map()} | {error, not_found}.
get(DivisionId) ->
    case project_divisions_store:get_division(DivisionId) of
        {ok, Division} ->
            {ok, Modules} = project_divisions_store:list_generated_modules(DivisionId),
            {ok, Tests} = project_divisions_store:list_generated_tests(DivisionId),
            {ok, Suites} = project_divisions_store:list_test_suites(DivisionId),
            {ok, Results} = project_divisions_store:list_test_results(DivisionId),
            {ok, Releases} = project_divisions_store:list_releases(DivisionId),
            {ok, Stages} = project_divisions_store:list_delivery_stages(DivisionId),
            {ok, #{
                division_id => maps:get(division_id, Division),
                crafting_status => maps:get(crafting_status, Division, 0),
                crafting_status_label => maps:get(crafting_status_label, Division, <<>>),
                crafting_opened_at => maps:get(crafting_opened_at, Division, undefined),
                generated_modules => Modules,
                generated_tests => Tests,
                test_suites => Suites,
                test_results => Results,
                releases => Releases,
                delivery_stages => Stages
            }};
        Error -> Error
    end.
