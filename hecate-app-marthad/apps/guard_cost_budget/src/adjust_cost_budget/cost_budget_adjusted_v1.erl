%%% @doc cost_budget_adjusted_v1 event
%%% Emitted when a cost budget limit is raised or lowered.
-module(cost_budget_adjusted_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_venture_id/1, get_new_budget_usd/1, get_adjusted_at/1]).

-record(cost_budget_adjusted_v1, {
    venture_id     :: binary(),
    new_budget_usd :: float(),
    adjusted_at    :: integer()
}).

-export_type([cost_budget_adjusted_v1/0]).
-opaque cost_budget_adjusted_v1() :: #cost_budget_adjusted_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> cost_budget_adjusted_v1().
new(#{venture_id := VentureId, new_budget_usd := NewBudgetUsd}) ->
    #cost_budget_adjusted_v1{
        venture_id     = VentureId,
        new_budget_usd = to_float(NewBudgetUsd),
        adjusted_at    = erlang:system_time(millisecond)
    }.

-spec to_map(cost_budget_adjusted_v1()) -> map().
to_map(#cost_budget_adjusted_v1{} = E) ->
    #{
        event_type     => <<"cost_budget_adjusted_v1">>,
        venture_id     => E#cost_budget_adjusted_v1.venture_id,
        new_budget_usd => E#cost_budget_adjusted_v1.new_budget_usd,
        adjusted_at    => E#cost_budget_adjusted_v1.adjusted_at
    }.

-spec from_map(map()) -> {ok, cost_budget_adjusted_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #cost_budget_adjusted_v1{
                venture_id     = VentureId,
                new_budget_usd = to_float(gv(new_budget_usd, Map, 0.0)),
                adjusted_at    = gv(adjusted_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_venture_id(cost_budget_adjusted_v1()) -> binary().
get_venture_id(#cost_budget_adjusted_v1{venture_id = V}) -> V.

-spec get_new_budget_usd(cost_budget_adjusted_v1()) -> float().
get_new_budget_usd(#cost_budget_adjusted_v1{new_budget_usd = V}) -> V.

-spec get_adjusted_at(cost_budget_adjusted_v1()) -> integer().
get_adjusted_at(#cost_budget_adjusted_v1{adjusted_at = V}) -> V.

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
