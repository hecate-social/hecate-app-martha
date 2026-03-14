%%% @doc Top-level supervisor for project_ventures.
%%%
%%% Starts the ETS store first, then 7 merged evoq_projection workers.
%%% @end
-module(project_ventures_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Projections = [
        venture_lifecycle_to_ventures,
        venture_lifecycle_to_discovered_divisions,
        storm_lifecycle_to_storm_sessions,
        storm_lifecycle_to_event_stickies,
        storm_lifecycle_to_event_stacks,
        storm_lifecycle_to_event_clusters,
        storm_lifecycle_to_fact_arrows
    ],
    ProjectionChildren = [#{
        id => Mod,
        start => {evoq_projection, start_link, [Mod, #{}, #{store_id => martha_store}]},
        restart => permanent,
        type => worker
    } || Mod <- Projections],
    Children = [
        %% ETS store must start first (creates named tables)
        #{
            id => project_ventures_store,
            start => {project_ventures_store, start_link, []},
            restart => permanent,
            type => worker
        }
        | ProjectionChildren
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
