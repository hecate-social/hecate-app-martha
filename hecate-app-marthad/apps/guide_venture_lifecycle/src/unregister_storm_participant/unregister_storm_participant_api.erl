%%% @doc API handler: DELETE /unregister_storm_participant/:venture_id/:participant_id
%%%
%%% Removes a participant from the storm roster.
%%% @end
-module(unregister_storm_participant_api).

-export([init/2, routes/0]).

routes() -> [{"/unregister_storm_participant/:venture_id/:participant_id", ?MODULE, []}].

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"DELETE">> -> handle_delete(Req0, State);
        _ -> hecate_plugin_api:method_not_allowed(Req0)
    end.

handle_delete(Req0, _State) ->
    VentureId = cowboy_req:binding(venture_id, Req0),
    ParticipantId = cowboy_req:binding(participant_id, Req0),
    case unregister_storm_participant_v1:new(#{
        venture_id => VentureId,
        participant_id => ParticipantId
    }) of
        {ok, Cmd} ->
            case maybe_unregister_storm_participant:dispatch(Cmd) of
                {ok, Version, Events} ->
                    Reply = #{venture_id => VentureId, version => Version, events => Events},
                    hecate_plugin_api:json_reply(200, Reply, Req0);
                {error, Reason} ->
                    hecate_plugin_api:json_error(422, Reason, Req0)
            end;
        {error, Reason} ->
            hecate_plugin_api:json_error(400, Reason, Req0)
    end.
