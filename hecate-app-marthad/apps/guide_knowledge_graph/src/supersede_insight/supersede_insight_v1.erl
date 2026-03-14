%%% @doc Command: supersede an insight with a newer one.
-module(supersede_insight_v1).

-export([new/1, from_map/1, to_map/1, validate/1]).
-export([get_venture_id/1, get_insight_id/1, get_superseded_by/1, get_reason/1]).

-record(supersede_insight_v1, {
    venture_id    :: binary(),
    insight_id    :: binary(),
    superseded_by :: binary(),
    reason        :: binary() | undefined
}).

-opaque supersede_insight_v1() :: #supersede_insight_v1{}.
-export_type([supersede_insight_v1/0]).

-spec new(map()) -> {ok, supersede_insight_v1()} | {error, term()}.
new(#{venture_id := VId, insight_id := IId, superseded_by := By} = Params) ->
    {ok, #supersede_insight_v1{
        venture_id = VId, insight_id = IId, superseded_by = By,
        reason = maps:get(reason, Params, undefined)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec from_map(map()) -> {ok, supersede_insight_v1()} | {error, term()}.
from_map(Map) ->
    VId = gv(venture_id, Map),
    IId = gv(insight_id, Map),
    By = gv(superseded_by, Map),
    case {VId, IId, By} of
        {undefined, _, _} -> {error, missing_required_fields};
        {_, undefined, _} -> {error, missing_required_fields};
        {_, _, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #supersede_insight_v1{
                venture_id = VId, insight_id = IId, superseded_by = By,
                reason = gv(reason, Map)
            }}
    end.

-spec to_map(supersede_insight_v1()) -> map().
to_map(#supersede_insight_v1{} = C) ->
    #{command_type => <<"supersede_insight">>,
      venture_id => C#supersede_insight_v1.venture_id,
      insight_id => C#supersede_insight_v1.insight_id,
      superseded_by => C#supersede_insight_v1.superseded_by,
      reason => C#supersede_insight_v1.reason}.

-spec validate(supersede_insight_v1()) -> {ok, supersede_insight_v1()} | {error, term()}.
validate(#supersede_insight_v1{insight_id = I, superseded_by = S}) when I =:= S ->
    {error, cannot_supersede_self};
validate(#supersede_insight_v1{} = Cmd) ->
    {ok, Cmd}.

get_venture_id(#supersede_insight_v1{venture_id = V}) -> V.
get_insight_id(#supersede_insight_v1{insight_id = V}) -> V.
get_superseded_by(#supersede_insight_v1{superseded_by = V}) -> V.
get_reason(#supersede_insight_v1{reason = V}) -> V.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
