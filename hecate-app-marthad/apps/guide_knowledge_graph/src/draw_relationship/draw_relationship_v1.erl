%%% @doc Command: draw a relationship between entities in the knowledge graph.
-module(draw_relationship_v1).

-export([new/1, from_map/1, to_map/1, validate/1]).
-export([get_venture_id/1, get_rel_id/1, get_from_entity/1,
         get_to_entity/1, get_rel_type/1, get_strength/1]).

-record(draw_relationship_v1, {
    venture_id  :: binary(),
    rel_id      :: binary(),
    from_entity :: binary(),
    to_entity   :: binary(),
    rel_type    :: binary(),
    strength    :: number()
}).

-opaque draw_relationship_v1() :: #draw_relationship_v1{}.
-export_type([draw_relationship_v1/0]).

-spec new(map()) -> {ok, draw_relationship_v1()} | {error, term()}.
new(#{venture_id := VId, from_entity := From, to_entity := To, rel_type := Type} = Params) ->
    RelId = maps:get(rel_id, Params, generate_id()),
    {ok, #draw_relationship_v1{
        venture_id = VId, rel_id = RelId, from_entity = From,
        to_entity = To, rel_type = Type,
        strength = maps:get(strength, Params, 1.0)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec from_map(map()) -> {ok, draw_relationship_v1()} | {error, term()}.
from_map(Map) ->
    VId = gv(venture_id, Map),
    From = gv(from_entity, Map),
    To = gv(to_entity, Map),
    Type = gv(rel_type, Map),
    case {VId, From, To, Type} of
        {undefined, _, _, _} -> {error, missing_required_fields};
        {_, undefined, _, _} -> {error, missing_required_fields};
        {_, _, undefined, _} -> {error, missing_required_fields};
        {_, _, _, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #draw_relationship_v1{
                venture_id = VId,
                rel_id = gv(rel_id, Map, generate_id()),
                from_entity = From,
                to_entity = To,
                rel_type = Type,
                strength = gv(strength, Map, 1.0)
            }}
    end.

-spec to_map(draw_relationship_v1()) -> map().
to_map(#draw_relationship_v1{} = C) ->
    #{command_type => <<"draw_relationship">>,
      venture_id => C#draw_relationship_v1.venture_id,
      rel_id => C#draw_relationship_v1.rel_id,
      from_entity => C#draw_relationship_v1.from_entity,
      to_entity => C#draw_relationship_v1.to_entity,
      rel_type => C#draw_relationship_v1.rel_type,
      strength => C#draw_relationship_v1.strength}.

-spec validate(draw_relationship_v1()) -> {ok, draw_relationship_v1()} | {error, term()}.
validate(#draw_relationship_v1{from_entity = F, to_entity = T}) when F =:= T ->
    {error, self_relationship};
validate(#draw_relationship_v1{} = Cmd) ->
    {ok, Cmd}.

get_venture_id(#draw_relationship_v1{venture_id = V}) -> V.
get_rel_id(#draw_relationship_v1{rel_id = V}) -> V.
get_from_entity(#draw_relationship_v1{from_entity = V}) -> V.
get_to_entity(#draw_relationship_v1{to_entity = V}) -> V.
get_rel_type(#draw_relationship_v1{rel_type = V}) -> V.
get_strength(#draw_relationship_v1{strength = V}) -> V.

generate_id() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"rel-", Ts/binary, "-", Rand/binary>>.

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
