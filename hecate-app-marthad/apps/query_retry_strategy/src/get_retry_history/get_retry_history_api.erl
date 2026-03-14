%%% @doc GET /api/retry-strategy/:venture_id — list retry history for a venture.
%%% GET /api/retry-strategy/:venture_id/:session_id — get single retry.
-module(get_retry_history_api).
-behaviour(cowboy_handler).
-export([init/2, routes/0]).

routes() ->
    [{"/api/retry-strategy/:venture_id", ?MODULE, []},
     {"/api/retry-strategy/:venture_id/:session_id", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, State) ->
    VentureId = cowboy_req:binding(venture_id, Req0),
    case cowboy_req:binding(session_id, Req0, undefined) of
        undefined ->
            %% List all retries for venture
            {ok, Retries} = project_retry_strategy_store:list_retries(VentureId),
            Resp = app_marthad_api_utils:json_ok(#{retries => Retries}, Req0),
            {ok, Resp, State};
        SessionId ->
            %% Single retry lookup
            case project_retry_strategy_store:get_retry(SessionId) of
                {ok, Retry} ->
                    Resp = app_marthad_api_utils:json_ok(#{retry => Retry}, Req0),
                    {ok, Resp, State};
                {error, not_found} ->
                    Resp = app_marthad_api_utils:json_error(404, <<"Retry not found">>, Req0),
                    {ok, Resp, State}
            end
    end.
