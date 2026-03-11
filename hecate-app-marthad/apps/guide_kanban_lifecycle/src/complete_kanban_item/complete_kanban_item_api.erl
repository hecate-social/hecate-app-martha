%%% @doc API handler: POST /api/kanbans/:division_id/items/:item_id/complete
-module(complete_kanban_item_api).
-export([init/2, routes/0]).

routes() -> [{"/api/kanbans/:division_id/items/:item_id/complete", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> handle_post(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_post(Req0, _State) ->
    DivisionId = cowboy_req:binding(division_id, Req0),
    ItemId = cowboy_req:binding(item_id, Req0),
    CmdParams = #{division_id => DivisionId, item_id => ItemId},
    case complete_kanban_item_v1:new(CmdParams) of
        {ok, Cmd} -> dispatch(Cmd, Req0);
        {error, Reason} -> app_marthad_api_utils:bad_request(Reason, Req0)
    end.

dispatch(Cmd, Req) ->
    case maybe_complete_kanban_item:dispatch(Cmd) of
        {ok, _Version, EventMaps} ->
            app_marthad_api_utils:json_ok(200, #{
                division_id => complete_kanban_item_v1:get_division_id(Cmd),
                item_id => complete_kanban_item_v1:get_item_id(Cmd),
                status_text => <<"done">>,
                events => EventMaps
            }, Req);
        {error, Reason} ->
            app_marthad_api_utils:bad_request(Reason, Req)
    end.
