-module(get_kanban_items_by_division_api).
-export([init/2, routes/0]).

routes() -> [{"/api/kanbans/:division_id/items", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    DivisionId = cowboy_req:binding(division_id, Req0),
    QS = cowboy_req:parse_qs(Req0),
    Filters = build_filters(QS, #{}),
    case get_kanban_items_by_division:get(DivisionId, Filters) of
        {ok, Items} ->
            app_marthad_api_utils:json_ok(#{items => Items}, Req0);
        {error, Reason} ->
            app_marthad_api_utils:json_error(500, Reason, Req0)
    end.

build_filters(QS, Acc) ->
    lists:foldl(fun parse_filter/2, Acc, QS).

parse_filter({<<"status">>, V}, Acc) -> Acc#{status => V};
parse_filter(_, Acc) -> Acc.
