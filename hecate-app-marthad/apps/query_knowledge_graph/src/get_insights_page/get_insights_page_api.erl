%%% @doc API handler: GET /api/knowledge-graph/:venture_id/insights
-module(get_insights_page_api).
-export([init/2, routes/0]).

routes() -> [{"/api/knowledge-graph/:venture_id/insights", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    VentureId = cowboy_req:binding(venture_id, Req0),
    case project_knowledge_graph_store:get_insights(VentureId) of
        {ok, Insights} ->
            app_marthad_api_utils:json_ok(#{insights => Insights}, Req0)
    end.
