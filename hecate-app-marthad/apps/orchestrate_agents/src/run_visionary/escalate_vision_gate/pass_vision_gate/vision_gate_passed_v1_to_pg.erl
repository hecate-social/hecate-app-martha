%%% @doc Emitter: publishes vision_gate_passed_v1 events to pg group.
-module(vision_gate_passed_v1_to_pg).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(PG_GROUP, vision_gate_passed_v1).

interested_in() -> [<<"vision_gate_passed_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Members = pg:get_members(pg, ?PG_GROUP),
    Msg = {?PG_GROUP, Event},
    lists:foreach(fun(Pid) -> Pid ! Msg end, Members),
    {ok, State}.
