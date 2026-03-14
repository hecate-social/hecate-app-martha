%%% @doc Event: insight superseded by a newer insight.
-module(insight_superseded_v1).

-behaviour(evoq_event).

-export([new/1, from_map/1, to_map/1]).
-export([event_type/0]).

-record(insight_superseded_v1, {
    venture_id    :: binary(),
    insight_id    :: binary(),
    superseded_by :: binary(),
    reason        :: binary() | undefined,
    superseded_at :: integer()
}).

-opaque insight_superseded_v1() :: #insight_superseded_v1{}.
-export_type([insight_superseded_v1/0]).

-spec new(map()) -> insight_superseded_v1().
-spec event_type() -> atom().
event_type() -> insight_superseded_v1.

new(Params) ->
    #insight_superseded_v1{
        venture_id = maps:get(venture_id, Params),
        insight_id = maps:get(insight_id, Params),
        superseded_by = maps:get(superseded_by, Params),
        reason = maps:get(reason, Params, undefined),
        superseded_at = erlang:system_time(millisecond)
    }.

-spec from_map(map()) -> {ok, insight_superseded_v1()}.
from_map(Map) ->
    {ok, #insight_superseded_v1{
        venture_id = gv(venture_id, Map),
        insight_id = gv(insight_id, Map),
        superseded_by = gv(superseded_by, Map),
        reason = gv(reason, Map),
        superseded_at = gv(superseded_at, Map)
    }}.

-spec to_map(insight_superseded_v1()) -> map().
to_map(#insight_superseded_v1{} = E) ->
    #{event_type => insight_superseded_v1,
      venture_id => E#insight_superseded_v1.venture_id,
      insight_id => E#insight_superseded_v1.insight_id,
      superseded_by => E#insight_superseded_v1.superseded_by,
      reason => E#insight_superseded_v1.reason,
      superseded_at => E#insight_superseded_v1.superseded_at}.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
