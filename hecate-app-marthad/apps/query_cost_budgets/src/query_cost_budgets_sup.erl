%%% @doc Top-level supervisor for query_cost_budgets.
%%%
%%% Query handlers are stateless Cowboy handlers, not supervised processes.
%%% The ETS store and projections live in project_cost_budgets.
%%% @end
-module(query_cost_budgets_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 10, period => 10}, []}}.
