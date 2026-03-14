%%% @doc Emitter: event_cluster_promoted_v1 -> Macula Mesh
-module(event_cluster_promoted_v1_to_mesh).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(MESH_TOPIC, <<"hecate.venture.storm.cluster.promoted">>).

interested_in() -> [<<"event_cluster_promoted_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    hecate_mesh:publish(?MESH_TOPIC, Event),
    {ok, State}.
