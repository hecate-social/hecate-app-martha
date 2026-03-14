%%% @doc Emitter: agent_session_archived_v1 -> pg (internal pub/sub)
-module(agent_session_archived_v1_to_pg).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(PG_GROUP, agent_session_archived_v1).

interested_in() -> [<<"agent_session_archived_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Members = pg:get_members(pg, ?PG_GROUP),
    Msg = {?PG_GROUP, Event},
    lists:foreach(fun(Pid) -> Pid ! Msg end, Members),
    app_marthad_web_events:broadcast(agent_session_archived, maps:get(data, Event)),
    {ok, State}.
