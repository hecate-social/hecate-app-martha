%%% @doc API handler: GET /api/venture
%%% Returns the first non-archived venture (the "active" venture).
-module(get_active_venture_api).

-export([init/2, routes/0]).

routes() -> [{"/api/venture", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    case project_ventures_store:list_ventures_active() of
        {ok, [Venture | _]} ->
            app_marthad_api_utils:json_ok(#{venture => Venture}, Req0);
        {ok, []} ->
            app_marthad_api_utils:json_error(404, <<"No active venture">>, Req0);
        {error, Reason} ->
            app_marthad_api_utils:json_error(500, Reason, Req0)
    end.
