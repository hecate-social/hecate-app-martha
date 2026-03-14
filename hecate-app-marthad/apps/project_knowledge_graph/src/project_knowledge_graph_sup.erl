%%% @doc Top-level supervisor for project_knowledge_graph.
%%%
%%% Starts the ETS store first, then the merged projection worker.
%%% @end
-module(project_knowledge_graph_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Projections = [
        knowledge_graph_lifecycle_to_knowledge_graph
    ],
    ProjectionChildren = [#{
        id => Mod,
        start => {evoq_projection, start_link, [Mod, #{}, #{store_id => knowledge_graph_store}]},
        restart => permanent,
        type => worker
    } || Mod <- Projections],
    Children = [
        %% ETS store must start first (creates named tables)
        #{
            id => project_knowledge_graph_store,
            start => {project_knowledge_graph_store, start_link, []},
            restart => permanent,
            type => worker
        }
        | ProjectionChildren
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, Children}}.
