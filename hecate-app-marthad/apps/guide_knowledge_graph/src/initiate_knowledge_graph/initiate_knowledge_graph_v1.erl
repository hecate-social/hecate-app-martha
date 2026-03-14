%%% @doc Command: initiate a knowledge graph for a venture.
-module(initiate_knowledge_graph_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, to_map/1, validate/1]).
-export([command_type/0]).
-export([get_venture_id/1]).
-export([generate_id/0]).

-record(initiate_knowledge_graph_v1, {
    venture_id :: binary()
}).

-opaque initiate_knowledge_graph_v1() :: #initiate_knowledge_graph_v1{}.
-export_type([initiate_knowledge_graph_v1/0]).

-spec new(map()) -> {ok, initiate_knowledge_graph_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> initiate_knowledge_graph_v1.

new(#{venture_id := VentureId}) ->
    {ok, #initiate_knowledge_graph_v1{venture_id = VentureId}};
new(_) ->
    {error, missing_required_fields}.

-spec from_map(map()) -> {ok, initiate_knowledge_graph_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ -> {ok, #initiate_knowledge_graph_v1{venture_id = VentureId}}
    end.

-spec to_map(initiate_knowledge_graph_v1()) -> map().
to_map(#initiate_knowledge_graph_v1{venture_id = VId}) ->
    #{command_type => initiate_knowledge_graph_v1,
      venture_id => VId}.

-spec validate(initiate_knowledge_graph_v1()) -> {ok, initiate_knowledge_graph_v1()} | {error, term()}.
validate(#initiate_knowledge_graph_v1{venture_id = V}) when not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#initiate_knowledge_graph_v1{} = Cmd) ->
    {ok, Cmd}.

-spec get_venture_id(initiate_knowledge_graph_v1()) -> binary().
get_venture_id(#initiate_knowledge_graph_v1{venture_id = V}) -> V.

-spec generate_id() -> binary().
generate_id() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"kg-", Ts/binary, "-", Rand/binary>>.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
