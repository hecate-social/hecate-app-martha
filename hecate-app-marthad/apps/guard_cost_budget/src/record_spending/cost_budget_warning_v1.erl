%%% @doc cost_budget_warning_v1 event
%%% Emitted when spending exceeds the warning threshold but not the full budget.
-module(cost_budget_warning_v1).

-behaviour(evoq_event).

-export([new/1, to_map/1, from_map/1]).
-export([event_type/0]).
-export([get_venture_id/1, get_spent_usd/1, get_budget_usd/1,
         get_warning_pct/1, get_warned_at/1]).

-record(cost_budget_warning_v1, {
    venture_id  :: binary(),
    spent_usd   :: float(),
    budget_usd  :: float(),
    warning_pct :: float(),
    warned_at   :: integer()
}).

-export_type([cost_budget_warning_v1/0]).
-opaque cost_budget_warning_v1() :: #cost_budget_warning_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> cost_budget_warning_v1().
-spec event_type() -> atom().
event_type() -> cost_budget_warning_v1.

new(#{venture_id := VentureId} = Params) ->
    #cost_budget_warning_v1{
        venture_id  = VentureId,
        spent_usd   = to_float(maps:get(spent_usd, Params, 0.0)),
        budget_usd  = to_float(maps:get(budget_usd, Params, 0.0)),
        warning_pct = to_float(maps:get(warning_pct, Params, 0.8)),
        warned_at   = erlang:system_time(millisecond)
    }.

-spec to_map(cost_budget_warning_v1()) -> map().
to_map(#cost_budget_warning_v1{} = E) ->
    #{
        event_type => cost_budget_warning_v1,
        venture_id  => E#cost_budget_warning_v1.venture_id,
        spent_usd   => E#cost_budget_warning_v1.spent_usd,
        budget_usd  => E#cost_budget_warning_v1.budget_usd,
        warning_pct => E#cost_budget_warning_v1.warning_pct,
        warned_at   => E#cost_budget_warning_v1.warned_at
    }.

-spec from_map(map()) -> {ok, cost_budget_warning_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #cost_budget_warning_v1{
                venture_id  = VentureId,
                spent_usd   = to_float(gv(spent_usd, Map, 0.0)),
                budget_usd  = to_float(gv(budget_usd, Map, 0.0)),
                warning_pct = to_float(gv(warning_pct, Map, 0.8)),
                warned_at   = gv(warned_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_venture_id(cost_budget_warning_v1()) -> binary().
get_venture_id(#cost_budget_warning_v1{venture_id = V}) -> V.

-spec get_spent_usd(cost_budget_warning_v1()) -> float().
get_spent_usd(#cost_budget_warning_v1{spent_usd = V}) -> V.

-spec get_budget_usd(cost_budget_warning_v1()) -> float().
get_budget_usd(#cost_budget_warning_v1{budget_usd = V}) -> V.

-spec get_warning_pct(cost_budget_warning_v1()) -> float().
get_warning_pct(#cost_budget_warning_v1{warning_pct = V}) -> V.

-spec get_warned_at(cost_budget_warning_v1()) -> integer().
get_warned_at(#cost_budget_warning_v1{warned_at = V}) -> V.

%% Internal
gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(BinKey, Map, Default)
    end.

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> V * 1.0;
to_float(V) -> V.
