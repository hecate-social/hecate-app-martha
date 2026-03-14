%%% @doc Event: insight captured in the knowledge graph.
-module(insight_captured_v1).

-behaviour(evoq_event).

-export([new/1, from_map/1, to_map/1]).
-export([event_type/0]).

-record(insight_captured_v1, {
    venture_id     :: binary(),
    insight_id     :: binary(),
    content        :: binary(),
    source_agent   :: binary() | undefined,
    source_session :: binary() | undefined,
    insight_type   :: binary() | undefined,
    captured_at    :: integer()
}).

-opaque insight_captured_v1() :: #insight_captured_v1{}.
-export_type([insight_captured_v1/0]).

-spec new(map()) -> insight_captured_v1().
-spec event_type() -> atom().
event_type() -> insight_captured_v1.

new(Params) ->
    #insight_captured_v1{
        venture_id = maps:get(venture_id, Params),
        insight_id = maps:get(insight_id, Params),
        content = maps:get(content, Params),
        source_agent = maps:get(source_agent, Params, undefined),
        source_session = maps:get(source_session, Params, undefined),
        insight_type = maps:get(insight_type, Params, <<"general">>),
        captured_at = erlang:system_time(millisecond)
    }.

-spec from_map(map()) -> {ok, insight_captured_v1()}.
from_map(Map) ->
    {ok, #insight_captured_v1{
        venture_id = gv(venture_id, Map),
        insight_id = gv(insight_id, Map),
        content = gv(content, Map),
        source_agent = gv(source_agent, Map),
        source_session = gv(source_session, Map),
        insight_type = gv(insight_type, Map),
        captured_at = gv(captured_at, Map)
    }}.

-spec to_map(insight_captured_v1()) -> map().
to_map(#insight_captured_v1{} = E) ->
    #{event_type => insight_captured_v1,
      venture_id => E#insight_captured_v1.venture_id,
      insight_id => E#insight_captured_v1.insight_id,
      content => E#insight_captured_v1.content,
      source_agent => E#insight_captured_v1.source_agent,
      source_session => E#insight_captured_v1.source_session,
      insight_type => E#insight_captured_v1.insight_type,
      captured_at => E#insight_captured_v1.captured_at}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
