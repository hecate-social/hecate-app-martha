-module(project_divisions_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Projections = [
        division_lifecycle_to_divisions,
        storming_lifecycle_to_designed_aggregates,
        storming_lifecycle_to_designed_events,
        storming_lifecycle_to_planned_desks,
        storming_lifecycle_to_planned_deps,
        kanban_lifecycle_to_kanban_cards,
        crafting_lifecycle_to_generated_modules,
        crafting_lifecycle_to_generated_tests,
        crafting_lifecycle_to_test_suites,
        crafting_lifecycle_to_test_results,
        crafting_lifecycle_to_releases,
        crafting_lifecycle_to_delivery_stages
    ],
    ProjectionChildren = [#{
        id => Mod,
        start => {evoq_projection, start_link, [Mod, #{}]},
        restart => permanent,
        type => worker
    } || Mod <- Projections],
    Children = [
        #{id => project_divisions_store,
          start => {project_divisions_store, start_link, []},
          restart => permanent,
          type => worker}
        | ProjectionChildren
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
