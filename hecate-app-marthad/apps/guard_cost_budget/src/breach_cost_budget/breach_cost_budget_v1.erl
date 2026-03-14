%%% @doc breach_cost_budget_v1 command
%%% Explicitly breaches a cost budget (manual override).
%%% Required: venture_id.
-module(breach_cost_budget_v1).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([get_venture_id/1]).

-record(breach_cost_budget_v1, {
    venture_id :: binary()
}).

-export_type([breach_cost_budget_v1/0]).
-opaque breach_cost_budget_v1() :: #breach_cost_budget_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, breach_cost_budget_v1()} | {error, term()}.
new(#{venture_id := VentureId}) ->
    {ok, #breach_cost_budget_v1{venture_id = VentureId}};
new(_) ->
    {error, missing_required_fields}.

-spec validate(breach_cost_budget_v1()) -> {ok, breach_cost_budget_v1()} | {error, term()}.
validate(#breach_cost_budget_v1{venture_id = V}) when not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#breach_cost_budget_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(breach_cost_budget_v1()) -> map().
to_map(#breach_cost_budget_v1{} = Cmd) ->
    #{
        command_type => <<"breach_cost_budget">>,
        venture_id => Cmd#breach_cost_budget_v1.venture_id
    }.

-spec from_map(map()) -> {ok, breach_cost_budget_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ ->
            {ok, #breach_cost_budget_v1{venture_id = VentureId}}
    end.

%% Accessors
-spec get_venture_id(breach_cost_budget_v1()) -> binary().
get_venture_id(#breach_cost_budget_v1{venture_id = V}) -> V.

%% Internal
gv(Key, Map) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(BinKey, Map, undefined)
    end.
