%%% @doc API handler: POST /api/kanbans/:division_id/items
-module(submit_kanban_item_api).

-export([init/2, routes/0]).

routes() -> [{"/api/kanbans/:division_id/items", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> handle_post(Req0, State);
        _ -> app_marthad_api_utils:method_not_allowed(Req0)
    end.

handle_post(Req0, _State) ->
    DivisionId = cowboy_req:binding(division_id, Req0),
    case app_marthad_api_utils:read_json_body(Req0) of
        {ok, Params, Req1} ->
            do_submit(DivisionId, Params, Req1);
        {error, invalid_json, Req1} ->
            app_marthad_api_utils:bad_request(<<"Invalid JSON">>, Req1)
    end.

do_submit(DivisionId, Params, Req) ->
    ItemId = case app_marthad_api_utils:get_field(item_id, Params) of
        undefined -> generate_item_id();
        Id -> Id
    end,
    Title = app_marthad_api_utils:get_field(title, Params),
    Description = app_marthad_api_utils:get_field(description, Params),
    ItemType = app_marthad_api_utils:get_field(item_type, Params),
    SubmittedBy = app_marthad_api_utils:get_field(submitted_by, Params),

    case Title of
        undefined ->
            app_marthad_api_utils:bad_request(<<"title is required">>, Req);
        _ ->
            CmdParams = #{
                division_id => DivisionId,
                item_id => ItemId,
                title => Title,
                description => Description,
                item_type => ItemType,
                submitted_by => SubmittedBy
            },
            case submit_kanban_item_v1:new(CmdParams) of
                {ok, Cmd} -> dispatch(Cmd, Req);
                {error, Reason} -> app_marthad_api_utils:bad_request(Reason, Req)
            end
    end.

dispatch(Cmd, Req) ->
    case maybe_submit_kanban_item:dispatch(Cmd) of
        {ok, _Version, EventMaps} ->
            app_marthad_api_utils:json_ok(201, #{
                division_id => submit_kanban_item_v1:get_division_id(Cmd),
                item_id => submit_kanban_item_v1:get_item_id(Cmd),
                title => submit_kanban_item_v1:get_title(Cmd),
                item_type => submit_kanban_item_v1:get_item_type(Cmd),
                status_text => <<"ready">>,
                events => EventMaps
            }, Req);
        {error, Reason} ->
            app_marthad_api_utils:bad_request(Reason, Req)
    end.

generate_item_id() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = integer_to_binary(rand:uniform(16#FFFFFF), 16),
    <<"item-", Ts/binary, "-", Rand/binary>>.
