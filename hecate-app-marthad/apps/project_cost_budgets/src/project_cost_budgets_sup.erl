%%% @doc Top-level supervisor for project_cost_budgets (PRJ).
%%%
%%% Starts the ETS store first (creates table), then merged projection.
%%% @end
-module(project_cost_budgets_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{
            id => project_cost_budgets_store,
            start => {project_cost_budgets_store, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => cost_budget_lifecycle_to_budgets,
            start => {evoq_projection, start_link, [cost_budget_lifecycle_to_budgets, #{}, #{store_id => cost_budget_store}]},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
