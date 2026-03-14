%%% @doc Supervisor for project_retry_strategy.
%%% Starts ETS store and merged projection worker.
-module(project_retry_strategy_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    StoreChild = #{
        id => project_retry_strategy_store,
        start => {project_retry_strategy_store, start_link, []},
        restart => permanent,
        type => worker
    },
    ProjectionChild = #{
        id => retry_lifecycle_to_retries,
        start => {evoq_projection, start_link,
                  [retry_lifecycle_to_retries, #{}, #{store_id => retry_strategy_store}]},
        restart => permanent,
        type => worker
    },
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10},
          [StoreChild, ProjectionChild]}}.
