%%% @doc Event: knowledge graph initiated for a venture.
-module(knowledge_graph_initiated_v1).

-export([new/1, from_map/1, to_map/1]).
-export([get_venture_id/1, get_initiated_at/1]).

-record(knowledge_graph_initiated_v1, {
    venture_id   :: binary(),
    initiated_at :: integer()
}).

-opaque knowledge_graph_initiated_v1() :: #knowledge_graph_initiated_v1{}.
-export_type([knowledge_graph_initiated_v1/0]).

-spec new(map()) -> knowledge_graph_initiated_v1().
new(#{venture_id := VentureId}) ->
    #knowledge_graph_initiated_v1{
        venture_id = VentureId,
        initiated_at = erlang:system_time(millisecond)
    }.

-spec from_map(map()) -> {ok, knowledge_graph_initiated_v1()} | {error, term()}.
from_map(Map) ->
    {ok, #knowledge_graph_initiated_v1{
        venture_id = gv(venture_id, Map),
        initiated_at = gv(initiated_at, Map)
    }}.

-spec to_map(knowledge_graph_initiated_v1()) -> map().
to_map(#knowledge_graph_initiated_v1{} = E) ->
    #{event_type => <<"knowledge_graph_initiated_v1">>,
      venture_id => E#knowledge_graph_initiated_v1.venture_id,
      initiated_at => E#knowledge_graph_initiated_v1.initiated_at}.

-spec get_venture_id(knowledge_graph_initiated_v1()) -> binary().
get_venture_id(#knowledge_graph_initiated_v1{venture_id = V}) -> V.

-spec get_initiated_at(knowledge_graph_initiated_v1()) -> integer().
get_initiated_at(#knowledge_graph_initiated_v1{initiated_at = T}) -> T.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
