%%% @doc Emitter: discovery_resumed_v1 -> pg (internal pub/sub)
-module(discovery_resumed_v1_to_pg).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(PG_GROUP, discovery_resumed_v1).

interested_in() -> [<<"discovery_resumed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Members = pg:get_members(pg, ?PG_GROUP),
    Msg = {?PG_GROUP, Event},
    lists:foreach(fun(Pid) -> Pid ! Msg end, Members),
    app_marthad_web_events:broadcast(discovery_resumed, maps:get(data, Event)),
    {ok, State}.
