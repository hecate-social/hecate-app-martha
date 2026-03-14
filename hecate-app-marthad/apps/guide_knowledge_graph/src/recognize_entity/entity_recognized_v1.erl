%%% @doc Event: entity recognized in the knowledge graph.
-module(entity_recognized_v1).

-export([new/1, from_map/1, to_map/1]).

-record(entity_recognized_v1, {
    venture_id   :: binary(),
    entity_id    :: binary(),
    entity_type  :: binary(),
    name         :: binary(),
    description  :: binary() | undefined,
    source_agent :: binary() | undefined,
    captured_at  :: integer()
}).

-opaque entity_recognized_v1() :: #entity_recognized_v1{}.
-export_type([entity_recognized_v1/0]).

-spec new(map()) -> entity_recognized_v1().
new(Params) ->
    #entity_recognized_v1{
        venture_id = maps:get(venture_id, Params),
        entity_id = maps:get(entity_id, Params),
        entity_type = maps:get(entity_type, Params),
        name = maps:get(name, Params),
        description = maps:get(description, Params, undefined),
        source_agent = maps:get(source_agent, Params, undefined),
        captured_at = erlang:system_time(millisecond)
    }.

-spec from_map(map()) -> {ok, entity_recognized_v1()}.
from_map(Map) ->
    {ok, #entity_recognized_v1{
        venture_id = gv(venture_id, Map),
        entity_id = gv(entity_id, Map),
        entity_type = gv(entity_type, Map),
        name = gv(name, Map),
        description = gv(description, Map),
        source_agent = gv(source_agent, Map),
        captured_at = gv(captured_at, Map)
    }}.

-spec to_map(entity_recognized_v1()) -> map().
to_map(#entity_recognized_v1{} = E) ->
    #{event_type => <<"entity_recognized_v1">>,
      venture_id => E#entity_recognized_v1.venture_id,
      entity_id => E#entity_recognized_v1.entity_id,
      entity_type => E#entity_recognized_v1.entity_type,
      name => E#entity_recognized_v1.name,
      description => E#entity_recognized_v1.description,
      source_agent => E#entity_recognized_v1.source_agent,
      captured_at => E#entity_recognized_v1.captured_at}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
