%%% @doc Event: relationship drawn between entities in the knowledge graph.
-module(relationship_drawn_v1).

-export([new/1, from_map/1, to_map/1]).

-record(relationship_drawn_v1, {
    venture_id  :: binary(),
    rel_id      :: binary(),
    from_entity :: binary(),
    to_entity   :: binary(),
    rel_type    :: binary(),
    strength    :: number(),
    drawn_at    :: integer()
}).

-opaque relationship_drawn_v1() :: #relationship_drawn_v1{}.
-export_type([relationship_drawn_v1/0]).

-spec new(map()) -> relationship_drawn_v1().
new(Params) ->
    #relationship_drawn_v1{
        venture_id = maps:get(venture_id, Params),
        rel_id = maps:get(rel_id, Params),
        from_entity = maps:get(from_entity, Params),
        to_entity = maps:get(to_entity, Params),
        rel_type = maps:get(rel_type, Params),
        strength = maps:get(strength, Params, 1.0),
        drawn_at = erlang:system_time(millisecond)
    }.

-spec from_map(map()) -> {ok, relationship_drawn_v1()}.
from_map(Map) ->
    {ok, #relationship_drawn_v1{
        venture_id = gv(venture_id, Map),
        rel_id = gv(rel_id, Map),
        from_entity = gv(from_entity, Map),
        to_entity = gv(to_entity, Map),
        rel_type = gv(rel_type, Map),
        strength = gv(strength, Map, 1.0),
        drawn_at = gv(drawn_at, Map)
    }}.

-spec to_map(relationship_drawn_v1()) -> map().
to_map(#relationship_drawn_v1{} = E) ->
    #{event_type => <<"relationship_drawn_v1">>,
      venture_id => E#relationship_drawn_v1.venture_id,
      rel_id => E#relationship_drawn_v1.rel_id,
      from_entity => E#relationship_drawn_v1.from_entity,
      to_entity => E#relationship_drawn_v1.to_entity,
      rel_type => E#relationship_drawn_v1.rel_type,
      strength => E#relationship_drawn_v1.strength,
      drawn_at => E#relationship_drawn_v1.drawn_at}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
