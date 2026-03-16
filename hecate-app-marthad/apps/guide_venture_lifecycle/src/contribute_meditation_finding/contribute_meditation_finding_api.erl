%%% @doc API handler: POST /contribute_meditation_finding/:venture_id
%%%
%%% Contributes a finding from meditation research.
%%% @end
-module(contribute_meditation_finding_api).

-export([init/2, routes/0]).

routes() -> [{"/contribute_meditation_finding/:venture_id", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> handle_post(Req0, State);
        _ -> hecate_plugin_api:method_not_allowed(Req0)
    end.

handle_post(Req0, _State) ->
    VentureId = cowboy_req:binding(venture_id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Params = json:decode(Body),
    Merged = Params#{<<"venture_id">> => VentureId},
    case contribute_meditation_finding_v1:from_map(Merged) of
        {ok, Cmd} ->
            case maybe_contribute_meditation_finding:dispatch(Cmd) of
                {ok, Version, Events} ->
                    Reply = #{venture_id => VentureId, version => Version, events => Events},
                    hecate_plugin_api:json_reply(201, Reply, Req1);
                {error, Reason} ->
                    hecate_plugin_api:json_error(422, Reason, Req1)
            end;
        {error, Reason} ->
            hecate_plugin_api:json_error(400, Reason, Req1)
    end.
