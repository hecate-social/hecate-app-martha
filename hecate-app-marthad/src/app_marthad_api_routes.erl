%%% @doc API route dispatcher for Martha plugin.
%%%
%%% Cowboy handler for /api/[...] that dispatches to the correct
%%% domain handler based on path matching. Route discovery is
%%% delegated to hecate_plugin_routes.
%%% @end
-module(app_marthad_api_routes).

-export([init/2]).
-export([discover_routes/0]).

%% Martha OTP apps that may contain API handlers.
-define(MARTHA_APPS, [
    hecate_app_marthad,
    martha,
    guide_venture_lifecycle, project_ventures, query_ventures,
    guide_division_lifecycle, project_divisions, query_divisions,
    orchestrate_agents, project_agent_sessions, query_agent_sessions,
    guide_knowledge_graph, project_knowledge_graph, query_knowledge_graph,
    guide_retry_strategy, project_retry_strategy, query_retry_strategy,
    guard_cost_budget, project_cost_budgets, query_cost_budgets
]).

%% @doc Cowboy handler init - dispatches to domain route handlers.
init(Req0, _State) ->
    PathInfo = cowboy_req:path_info(Req0),
    Path = case PathInfo of
        undefined -> cowboy_req:path(Req0);
        Parts -> iolist_to_binary([<<"/">>, lists:join(<<"/">>, Parts)])
    end,
    case find_handler(Path, discover_routes()) of
        {ok, Handler, HandlerState} ->
            Handler:init(Req0, HandlerState);
        nomatch ->
            hecate_plugin_api:not_found(Req0)
    end.

%% @doc Discover all routes from Martha domain apps.
-spec discover_routes() -> [tuple()].
discover_routes() ->
    hecate_plugin_routes:discover_routes(?MARTHA_APPS).

%%% Internal

find_handler(_Path, []) ->
    nomatch;
find_handler(Path, [{Pattern, Handler, HandlerState} | Rest]) ->
    case match_path(Path, Pattern) of
        true -> {ok, Handler, HandlerState};
        false -> find_handler(Path, Rest)
    end;
find_handler(Path, [_ | Rest]) ->
    find_handler(Path, Rest).

match_path(Path, Pattern) when is_binary(Pattern) ->
    Path =:= Pattern;
match_path(Path, Pattern) when is_list(Pattern) ->
    Path =:= list_to_binary(Pattern);
match_path(_, _) ->
    false.
