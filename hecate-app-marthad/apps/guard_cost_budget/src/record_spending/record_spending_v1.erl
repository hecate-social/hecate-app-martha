%%% @doc record_spending_v1 command
%%% Records a spending amount against a venture's cost budget.
%%% Required: venture_id, amount_usd, model, session_id.
-module(record_spending_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([command_type/0]).
-export([get_venture_id/1, get_amount_usd/1, get_model/1, get_session_id/1]).

-record(record_spending_v1, {
    venture_id :: binary(),
    amount_usd :: float(),
    model      :: binary(),
    session_id :: binary()
}).

-export_type([record_spending_v1/0]).
-opaque record_spending_v1() :: #record_spending_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, record_spending_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> record_spending_v1.

new(#{venture_id := VentureId, amount_usd := AmountUsd, model := Model} = Params) ->
    {ok, #record_spending_v1{
        venture_id = VentureId,
        amount_usd = to_float(AmountUsd),
        model      = Model,
        session_id = maps:get(session_id, Params, <<>>)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(record_spending_v1()) -> {ok, record_spending_v1()} | {error, term()}.
validate(#record_spending_v1{venture_id = V}) when not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#record_spending_v1{amount_usd = A}) when not is_number(A); A =< 0 ->
    {error, invalid_amount_usd};
validate(#record_spending_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(record_spending_v1()) -> map().
to_map(#record_spending_v1{} = Cmd) ->
    #{
        command_type => record_spending_v1,
        venture_id => Cmd#record_spending_v1.venture_id,
        amount_usd => Cmd#record_spending_v1.amount_usd,
        model => Cmd#record_spending_v1.model,
        session_id => Cmd#record_spending_v1.session_id
    }.

-spec from_map(map()) -> {ok, record_spending_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    AmountUsd = gv(amount_usd, Map),
    Model = gv(model, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ when AmountUsd =:= undefined -> {error, missing_required_fields};
        _ when Model =:= undefined -> {error, missing_required_fields};
        _ ->
            {ok, #record_spending_v1{
                venture_id = VentureId,
                amount_usd = to_float(AmountUsd),
                model      = Model,
                session_id = gv(session_id, Map, <<>>)
            }}
    end.

%% Accessors
-spec get_venture_id(record_spending_v1()) -> binary().
get_venture_id(#record_spending_v1{venture_id = V}) -> V.

-spec get_amount_usd(record_spending_v1()) -> float().
get_amount_usd(#record_spending_v1{amount_usd = V}) -> V.

-spec get_model(record_spending_v1()) -> binary().
get_model(#record_spending_v1{model = V}) -> V.

-spec get_session_id(record_spending_v1()) -> binary().
get_session_id(#record_spending_v1{session_id = V}) -> V.

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
