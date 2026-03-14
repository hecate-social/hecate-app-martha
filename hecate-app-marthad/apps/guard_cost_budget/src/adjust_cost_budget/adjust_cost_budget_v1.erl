%%% @doc adjust_cost_budget_v1 command
%%% Adjusts the budget limit for a venture (raise or lower).
%%% Required: venture_id, new_budget_usd.
-module(adjust_cost_budget_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([command_type/0]).
-export([get_venture_id/1, get_new_budget_usd/1]).

-record(adjust_cost_budget_v1, {
    venture_id     :: binary(),
    new_budget_usd :: float()
}).

-export_type([adjust_cost_budget_v1/0]).
-opaque adjust_cost_budget_v1() :: #adjust_cost_budget_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, adjust_cost_budget_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> adjust_cost_budget_v1.

new(#{venture_id := VentureId, new_budget_usd := NewBudgetUsd}) ->
    {ok, #adjust_cost_budget_v1{
        venture_id     = VentureId,
        new_budget_usd = to_float(NewBudgetUsd)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(adjust_cost_budget_v1()) -> {ok, adjust_cost_budget_v1()} | {error, term()}.
validate(#adjust_cost_budget_v1{venture_id = V}) when not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#adjust_cost_budget_v1{new_budget_usd = B}) when not is_number(B); B =< 0 ->
    {error, invalid_new_budget_usd};
validate(#adjust_cost_budget_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(adjust_cost_budget_v1()) -> map().
to_map(#adjust_cost_budget_v1{} = Cmd) ->
    #{
        command_type => adjust_cost_budget_v1,
        venture_id => Cmd#adjust_cost_budget_v1.venture_id,
        new_budget_usd => Cmd#adjust_cost_budget_v1.new_budget_usd
    }.

-spec from_map(map()) -> {ok, adjust_cost_budget_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    NewBudgetUsd = gv(new_budget_usd, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ when NewBudgetUsd =:= undefined -> {error, missing_required_fields};
        _ ->
            {ok, #adjust_cost_budget_v1{
                venture_id     = VentureId,
                new_budget_usd = to_float(NewBudgetUsd)
            }}
    end.

%% Accessors
-spec get_venture_id(adjust_cost_budget_v1()) -> binary().
get_venture_id(#adjust_cost_budget_v1{venture_id = V}) -> V.

-spec get_new_budget_usd(adjust_cost_budget_v1()) -> float().
get_new_budget_usd(#adjust_cost_budget_v1{new_budget_usd = V}) -> V.

%% Internal
gv(Key, Map) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(BinKey, Map, undefined)
    end.

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> V * 1.0;
to_float(V) -> V.
