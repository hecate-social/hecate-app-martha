%%% @doc Top-level supervisor for project_division_kanbans.
-module(project_division_kanbans_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% SQLite connection worker (must start first)
        #{
            id => project_division_kanbans_store,
            start => {project_division_kanbans_store, start_link, []},
            restart => permanent,
            type => worker
        },
        %% Projection: kanban_initiated_v1 -> division_kanbans table
        #{
            id => kanban_initiated_v1_to_division_kanbans_sup,
            start => {kanban_initiated_v1_to_division_kanbans_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: kanban_archived_v1 -> division_kanbans table
        #{
            id => kanban_archived_v1_to_division_kanbans_sup,
            start => {kanban_archived_v1_to_division_kanbans_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: kanban_item_submitted_v1 -> kanban_items table
        #{
            id => kanban_item_submitted_v1_to_kanban_items_sup,
            start => {kanban_item_submitted_v1_to_kanban_items_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: kanban_item_picked_v1 -> kanban_items table
        #{
            id => kanban_item_picked_v1_to_kanban_items_sup,
            start => {kanban_item_picked_v1_to_kanban_items_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: kanban_item_completed_v1 -> kanban_items table
        #{
            id => kanban_item_completed_v1_to_kanban_items_sup,
            start => {kanban_item_completed_v1_to_kanban_items_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        %% Projection: kanban_item_returned_v1 -> kanban_items table
        #{
            id => kanban_item_returned_v1_to_kanban_items_sup,
            start => {kanban_item_returned_v1_to_kanban_items_sup, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
