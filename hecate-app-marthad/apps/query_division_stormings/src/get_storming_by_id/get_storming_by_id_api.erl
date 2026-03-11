-module(get_storming_by_id_api).
-export([init/2, routes/0]).

routes() -> [{"/api/stormings/:division_id", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    DivisionId = cowboy_req:binding(division_id, Req0),
    case get_storming_by_id:get(DivisionId) of
        {ok, Storming} ->
            app_marthad_api_utils:json_ok(#{storming => Storming}, Req0);
        {error, not_found} ->
            app_marthad_api_utils:json_error(404, <<"Storming not found">>, Req0);
        {error, Reason} ->
            app_marthad_api_utils:json_error(500, Reason, Req0)
    end.
