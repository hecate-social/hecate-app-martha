%%% @doc Merged projection: storm lifecycle events -> storm_sessions ETS.
-module(storm_lifecycle_to_storm_sessions).
-behaviour(evoq_projection).

-export([interested_in/0, init/1, project/4]).

-define(TABLE, project_ventures_storm_sessions).

interested_in() ->
    [<<"big_picture_storm_started_v1">>,
     <<"big_picture_storm_shelved_v1">>,
     <<"big_picture_storm_resumed_v1">>,
     <<"big_picture_storm_archived_v1">>,
     <<"storm_phase_advanced_v1">>].

init(_Config) ->
    {ok, RM} = evoq_read_model:new(evoq_read_model_ets, #{name => ?TABLE}),
    {ok, #{}, RM}.

project(#{data := Data} = Event, _Metadata, State, RM) ->
    EventType = get_event_type(Event),
    do_project(EventType, Data, State, RM).

do_project(<<"big_picture_storm_started_v1">>, Data, State, RM) ->
    VentureId = gf(venture_id, Data),
    StormNumber = gf(storm_number, Data),
    Key = {VentureId, StormNumber},
    Session = #{
        venture_id   => VentureId,
        storm_number => StormNumber,
        phase        => <<"storm">>,
        started_at   => gf(started_at, Data),
        shelved_at   => undefined,
        completed_at => undefined
    },
    {ok, RM2} = evoq_read_model:put(Key, Session, RM),
    {ok, State, RM2};

do_project(<<"big_picture_storm_shelved_v1">>, Data, State, RM) ->
    update_latest_session(gf(venture_id, Data), fun(S) ->
        S#{shelved_at => gf(shelved_at, Data)}
    end, State, RM);

do_project(<<"big_picture_storm_resumed_v1">>, Data, State, RM) ->
    update_latest_session(gf(venture_id, Data), fun(S) ->
        S#{shelved_at => undefined}
    end, State, RM);

do_project(<<"big_picture_storm_archived_v1">>, Data, State, RM) ->
    ArchivedAt = gf(archived_at, Data, erlang:system_time(millisecond)),
    update_latest_session(gf(venture_id, Data), fun(S) ->
        S#{completed_at => ArchivedAt}
    end, State, RM);

do_project(<<"storm_phase_advanced_v1">>, Data, State, RM) ->
    update_latest_session(gf(venture_id, Data), fun(S) ->
        S#{phase => gf(phase, Data)}
    end, State, RM);

do_project(_Unknown, _Data, State, RM) ->
    {skip, State, RM}.

%%====================================================================
%% Internal
%%====================================================================

update_latest_session(VentureId, UpdateFun, State, RM) ->
    case find_latest_session(VentureId) of
        {ok, Key, Session} ->
            Updated = UpdateFun(Session),
            {ok, RM2} = evoq_read_model:put(Key, Updated, RM),
            {ok, State, RM2};
        {error, not_found} ->
            {skip, State, RM}
    end.

find_latest_session(VentureId) ->
    Sessions = [{K, S} || {K, #{venture_id := V} = S} <- ets:tab2list(?TABLE),
                          V =:= VentureId],
    case Sessions of
        [] -> {error, not_found};
        _ ->
            {LatestKey, LatestSession} = lists:foldl(fun({K, S}, {AccK, AccS}) ->
                case maps:get(storm_number, S, 0) > maps:get(storm_number, AccS, 0) of
                    true -> {K, S};
                    false -> {AccK, AccS}
                end
            end, hd(Sessions), tl(Sessions)),
            {ok, LatestKey, LatestSession}
    end.

get_event_type(#{event_type := T}) when is_binary(T) -> T;
get_event_type(_) -> undefined.

gf(Key, Data) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, undefined)
    end.

gf(Key, Data, Default) when is_atom(Key) ->
    case maps:find(Key, Data) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Data, Default)
    end.
