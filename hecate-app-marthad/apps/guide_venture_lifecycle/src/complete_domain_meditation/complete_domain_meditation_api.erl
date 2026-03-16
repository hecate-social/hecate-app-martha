%%% @doc API handler: POST /complete_domain_meditation/:venture_id
%%%
%%% Completes the domain meditation phase.
%%% @end
-module(complete_domain_meditation_api).

-export([init/2, routes/0]).

routes() -> [{"/complete_domain_meditation/:venture_id", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> handle_post(Req0, State);
        _ -> hecate_plugin_api:method_not_allowed(Req0)
    end.

handle_post(Req0, _State) ->
    VentureId = cowboy_req:binding(venture_id, Req0),
    case complete_domain_meditation_v1:new(#{venture_id => VentureId}) of
        {ok, Cmd} ->
            case maybe_complete_domain_meditation:dispatch(Cmd) of
                {ok, Version, Events} ->
                    Body = #{venture_id => VentureId, version => Version, events => Events},
                    hecate_plugin_api:json_reply(200, Body, Req0);
                {error, Reason} ->
                    hecate_plugin_api:json_error(422, Reason, Req0)
            end;
        {error, Reason} ->
            hecate_plugin_api:json_error(400, Reason, Req0)
    end.
