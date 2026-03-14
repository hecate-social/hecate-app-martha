%%% @doc Event: knowledge graph archived.
-module(knowledge_graph_archived_v1).

-behaviour(evoq_event).

-export([new/1, from_map/1, to_map/1]).
-export([event_type/0]).

-record(knowledge_graph_archived_v1, {
    venture_id  :: binary(),
    reason      :: binary() | undefined,
    archived_at :: integer()
}).

-opaque knowledge_graph_archived_v1() :: #knowledge_graph_archived_v1{}.
-export_type([knowledge_graph_archived_v1/0]).

-spec new(map()) -> knowledge_graph_archived_v1().
-spec event_type() -> atom().
event_type() -> knowledge_graph_archived_v1.

new(Params) ->
    #knowledge_graph_archived_v1{
        venture_id = maps:get(venture_id, Params),
        reason = maps:get(reason, Params, undefined),
        archived_at = erlang:system_time(millisecond)
    }.

-spec from_map(map()) -> {ok, knowledge_graph_archived_v1()}.
from_map(Map) ->
    {ok, #knowledge_graph_archived_v1{
        venture_id = gv(venture_id, Map),
        reason = gv(reason, Map),
        archived_at = gv(archived_at, Map)
    }}.

-spec to_map(knowledge_graph_archived_v1()) -> map().
to_map(#knowledge_graph_archived_v1{} = E) ->
    #{event_type => knowledge_graph_archived_v1,
      venture_id => E#knowledge_graph_archived_v1.venture_id,
      reason => E#knowledge_graph_archived_v1.reason,
      archived_at => E#knowledge_graph_archived_v1.archived_at}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
