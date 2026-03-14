%%% @doc Emitter: event_sticky_unclustered_v1 -> Macula Mesh
-module(event_sticky_unclustered_v1_to_mesh).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(MESH_TOPIC, <<"hecate.venture.storm.sticky.unclustered">>).

interested_in() -> [<<"event_sticky_unclustered_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    hecate_mesh:publish(?MESH_TOPIC, Event),
    {ok, State}.
