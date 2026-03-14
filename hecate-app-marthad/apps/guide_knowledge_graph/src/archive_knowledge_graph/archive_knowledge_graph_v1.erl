%%% @doc Command: archive a knowledge graph.
-module(archive_knowledge_graph_v1).

-export([new/1, from_map/1, to_map/1, validate/1]).
-export([get_venture_id/1, get_reason/1]).

-record(archive_knowledge_graph_v1, {
    venture_id :: binary(),
    reason     :: binary() | undefined
}).

-opaque archive_knowledge_graph_v1() :: #archive_knowledge_graph_v1{}.
-export_type([archive_knowledge_graph_v1/0]).

-spec new(map()) -> {ok, archive_knowledge_graph_v1()} | {error, term()}.
new(#{venture_id := VId} = Params) ->
    {ok, #archive_knowledge_graph_v1{
        venture_id = VId,
        reason = maps:get(reason, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec from_map(map()) -> {ok, archive_knowledge_graph_v1()} | {error, term()}.
from_map(Map) ->
    VId = gv(venture_id, Map),
    case VId of
        undefined -> {error, missing_required_fields};
        _ ->
            {ok, #archive_knowledge_graph_v1{
                venture_id = VId,
                reason = gv(reason, Map)
            }}
    end.

-spec to_map(archive_knowledge_graph_v1()) -> map().
to_map(#archive_knowledge_graph_v1{} = C) ->
    #{command_type => <<"archive_knowledge_graph">>,
      venture_id => C#archive_knowledge_graph_v1.venture_id,
      reason => C#archive_knowledge_graph_v1.reason}.

-spec validate(archive_knowledge_graph_v1()) -> {ok, archive_knowledge_graph_v1()} | {error, term()}.
validate(#archive_knowledge_graph_v1{} = Cmd) ->
    {ok, Cmd}.

get_venture_id(#archive_knowledge_graph_v1{venture_id = V}) -> V.
get_reason(#archive_knowledge_graph_v1{reason = V}) -> V.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
