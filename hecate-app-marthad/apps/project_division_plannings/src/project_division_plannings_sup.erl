%%% @doc Top-level supervisor for project_division_plannings.
%%%
%%% Content projections (aggregate_designed, event_designed, desk_planned,
%%% dependency_planned) have moved to project_division_stormings.
-module(project_division_plannings_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% SQLite connection worker (must start first)
        #{
            id => project_division_plannings_store,
            start => {project_division_plannings_store, start_link, []},
            restart => permanent,
            type => worker
        },
        %% Projection: planning_initiated_v1 -> division_plannings table
        #{
            id => planning_initiated_v1_to_division_plannings_sup,
            start => {planning_initiated_v1_to_division_plannings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: planning_archived_v1 -> division_plannings table
        #{
            id => planning_archived_v1_to_division_plannings_sup,
            start => {planning_archived_v1_to_division_plannings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: planning_opened_v1 -> division_plannings table
        #{
            id => planning_opened_v1_to_division_plannings_sup,
            start => {planning_opened_v1_to_division_plannings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: planning_shelved_v1 -> division_plannings table
        #{
            id => planning_shelved_v1_to_division_plannings_sup,
            start => {planning_shelved_v1_to_division_plannings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: planning_resumed_v1 -> division_plannings table
        #{
            id => planning_resumed_v1_to_division_plannings_sup,
            start => {planning_resumed_v1_to_division_plannings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: planning_submitted_v1 -> division_plannings table
        #{
            id => planning_submitted_v1_to_division_plannings_sup,
            start => {planning_submitted_v1_to_division_plannings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
