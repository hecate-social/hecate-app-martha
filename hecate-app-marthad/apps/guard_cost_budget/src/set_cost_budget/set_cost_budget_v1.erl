%%% @doc set_cost_budget_v1 command
%%% Configures a cost budget for a venture.
%%% Required: venture_id, budget_usd.
%%% Optional: warning_pct (default 0.8), model_policy.
-module(set_cost_budget_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([command_type/0]).
-export([get_venture_id/1, get_budget_usd/1, get_warning_pct/1, get_model_policy/1]).

-record(set_cost_budget_v1, {
    venture_id   :: binary(),
    budget_usd   :: float(),
    warning_pct  :: float(),
    model_policy :: map()
}).

-export_type([set_cost_budget_v1/0]).
-opaque set_cost_budget_v1() :: #set_cost_budget_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, set_cost_budget_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> set_cost_budget_v1.

new(#{venture_id := VentureId, budget_usd := BudgetUsd} = Params) ->
    {ok, #set_cost_budget_v1{
        venture_id   = VentureId,
        budget_usd   = to_float(BudgetUsd),
        warning_pct  = to_float(maps:get(warning_pct, Params, 0.8)),
        model_policy = maps:get(model_policy, Params, #{})
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(set_cost_budget_v1()) -> {ok, set_cost_budget_v1()} | {error, term()}.
validate(#set_cost_budget_v1{venture_id = V}) when not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#set_cost_budget_v1{budget_usd = B}) when not is_number(B); B =< 0 ->
    {error, invalid_budget_usd};
validate(#set_cost_budget_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(set_cost_budget_v1()) -> map().
to_map(#set_cost_budget_v1{} = Cmd) ->
    #{
        command_type => set_cost_budget_v1,
        venture_id => Cmd#set_cost_budget_v1.venture_id,
        budget_usd => Cmd#set_cost_budget_v1.budget_usd,
        warning_pct => Cmd#set_cost_budget_v1.warning_pct,
        model_policy => Cmd#set_cost_budget_v1.model_policy
    }.

-spec from_map(map()) -> {ok, set_cost_budget_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    BudgetUsd = gv(budget_usd, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ when BudgetUsd =:= undefined -> {error, missing_required_fields};
        _ ->
            {ok, #set_cost_budget_v1{
                venture_id   = VentureId,
                budget_usd   = to_float(BudgetUsd),
                warning_pct  = to_float(gv(warning_pct, Map, 0.8)),
                model_policy = gv(model_policy, Map, #{})
            }}
    end.

%% Accessors
-spec get_venture_id(set_cost_budget_v1()) -> binary().
get_venture_id(#set_cost_budget_v1{venture_id = V}) -> V.

-spec get_budget_usd(set_cost_budget_v1()) -> float().
get_budget_usd(#set_cost_budget_v1{budget_usd = V}) -> V.

-spec get_warning_pct(set_cost_budget_v1()) -> float().
get_warning_pct(#set_cost_budget_v1{warning_pct = V}) -> V.

-spec get_model_policy(set_cost_budget_v1()) -> map().
get_model_policy(#set_cost_budget_v1{model_policy = V}) -> V.

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
