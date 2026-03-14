%%% @doc Command: recognize an entity in the knowledge graph.
-module(recognize_entity_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, to_map/1, validate/1]).
-export([command_type/0]).
-export([get_venture_id/1, get_entity_id/1, get_entity_type/1,
         get_name/1, get_description/1, get_source_agent/1]).

-record(recognize_entity_v1, {
    venture_id   :: binary(),
    entity_id    :: binary(),
    entity_type  :: binary(),
    name         :: binary(),
    description  :: binary() | undefined,
    source_agent :: binary() | undefined
}).

-opaque recognize_entity_v1() :: #recognize_entity_v1{}.
-export_type([recognize_entity_v1/0]).

-spec new(map()) -> {ok, recognize_entity_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> recognize_entity_v1.

new(#{venture_id := VId, name := Name, entity_type := Type} = Params) ->
    EntityId = maps:get(entity_id, Params, generate_id()),
    {ok, #recognize_entity_v1{
        venture_id = VId, entity_id = EntityId, entity_type = Type,
        name = Name, description = maps:get(description, Params, undefined),
        source_agent = maps:get(source_agent, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec from_map(map()) -> {ok, recognize_entity_v1()} | {error, term()}.
from_map(Map) ->
    VId = gv(venture_id, Map),
    Name = gv(name, Map),
    Type = gv(entity_type, Map),
    case {VId, Name, Type} of
        {undefined, _, _} -> {error, missing_required_fields};
        {_, undefined, _} -> {error, missing_required_fields};
        {_, _, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #recognize_entity_v1{
                venture_id = VId,
                entity_id = gv(entity_id, Map, generate_id()),
                entity_type = Type,
                name = Name,
                description = gv(description, Map),
                source_agent = gv(source_agent, Map)
            }}
    end.

-spec to_map(recognize_entity_v1()) -> map().
to_map(#recognize_entity_v1{} = C) ->
    #{command_type => recognize_entity_v1,
      venture_id => C#recognize_entity_v1.venture_id,
      entity_id => C#recognize_entity_v1.entity_id,
      entity_type => C#recognize_entity_v1.entity_type,
      name => C#recognize_entity_v1.name,
      description => C#recognize_entity_v1.description,
      source_agent => C#recognize_entity_v1.source_agent}.

-spec validate(recognize_entity_v1()) -> {ok, recognize_entity_v1()} | {error, term()}.
validate(#recognize_entity_v1{name = N}) when not is_binary(N); byte_size(N) =:= 0 ->
    {error, invalid_name};
validate(#recognize_entity_v1{} = Cmd) ->
    {ok, Cmd}.

get_venture_id(#recognize_entity_v1{venture_id = V}) -> V.
get_entity_id(#recognize_entity_v1{entity_id = V}) -> V.
get_entity_type(#recognize_entity_v1{entity_type = V}) -> V.
get_name(#recognize_entity_v1{name = V}) -> V.
get_description(#recognize_entity_v1{description = V}) -> V.
get_source_agent(#recognize_entity_v1{source_agent = V}) -> V.

generate_id() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"entity-", Ts/binary, "-", Rand/binary>>.

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
