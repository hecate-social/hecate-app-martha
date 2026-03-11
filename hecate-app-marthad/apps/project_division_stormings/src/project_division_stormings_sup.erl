%%% @doc Top-level supervisor for project_division_stormings.
-module(project_division_stormings_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% SQLite connection worker (must start first)
        #{
            id => project_division_stormings_store,
            start => {project_division_stormings_store, start_link, []},
            restart => permanent,
            type => worker
        },
        %% Projection: storming_initiated_v1 -> division_stormings table
        #{
            id => storming_initiated_v1_to_division_stormings_sup,
            start => {storming_initiated_v1_to_division_stormings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: storming_archived_v1 -> division_stormings table
        #{
            id => storming_archived_v1_to_division_stormings_sup,
            start => {storming_archived_v1_to_division_stormings_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: aggregate_designed_v1 -> designed_aggregates table
        #{
            id => aggregate_designed_v1_to_designed_aggregates_sup,
            start => {aggregate_designed_v1_to_designed_aggregates_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: event_designed_v1 -> designed_events table
        #{
            id => event_designed_v1_to_designed_events_sup,
            start => {event_designed_v1_to_designed_events_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: desk_planned_v1 -> planned_desks table
        #{
            id => desk_planned_v1_to_planned_desks_sup,
            start => {desk_planned_v1_to_planned_desks_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: dependency_planned_v1 -> planned_dependencies table
        #{
            id => dependency_planned_v1_to_planned_dependencies_sup,
            start => {dependency_planned_v1_to_planned_dependencies_sup, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
