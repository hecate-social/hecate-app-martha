-module(get_kanban_by_division_api).
-export([init/2, routes/0]).

routes() -> [{"/api/kanbans/:division_id", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    DivisionId = cowboy_req:binding(division_id, Req0),
    case get_kanban_by_division:get(DivisionId) of
        {ok, Kanban} ->
            app_marthad_api_utils:json_ok(#{kanban => Kanban}, Req0);
        {error, not_found} ->
            app_marthad_api_utils:json_error(404, <<"Kanban not found">>, Req0);
        {error, Reason} ->
            app_marthad_api_utils:json_error(500, Reason, Req0)
    end.
