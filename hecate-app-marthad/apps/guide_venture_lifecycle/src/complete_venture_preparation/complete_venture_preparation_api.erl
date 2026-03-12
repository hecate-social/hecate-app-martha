%%% @doc API handler: POST /api/ventures/:venture_id/knowledge/complete
%%%
%%% Marks venture knowledge preparation as complete.
-module(complete_venture_preparation_api).

-export([init/2, routes/0]).

routes() -> [{"/api/ventures/:venture_id/knowledge/complete", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> handle_post(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_post(Req0, _State) ->
    VentureId = cowboy_req:binding(venture_id, Req0),
    case VentureId of
        undefined ->
            app_marthad_api_utils:bad_request(<<"venture_id is required">>, Req0);
        _ ->
            do_complete(VentureId, Req0)
    end.

do_complete(VentureId, Req) ->
    CmdParams = #{venture_id => VentureId},
    case complete_venture_preparation_v1:new(CmdParams) of
        {ok, Cmd} -> dispatch(Cmd, VentureId, Req);
        {error, Reason} -> app_marthad_api_utils:bad_request(Reason, Req)
    end.

dispatch(Cmd, VentureId, Req) ->
    case maybe_complete_venture_preparation:dispatch(Cmd) of
        {ok, _Version, EventMaps} ->
            app_marthad_api_utils:json_ok(200, #{
                venture_id => VentureId,
                preparation_completed => true,
                events => EventMaps
            }, Req);
        {error, Reason} ->
            app_marthad_api_utils:bad_request(Reason, Req)
    end.
