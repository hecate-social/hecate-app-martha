%%% @doc spending_recorded_v1 event
%%% Emitted when spending is recorded against a cost budget.
-module(spending_recorded_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_venture_id/1, get_amount_usd/1, get_new_total_usd/1,
         get_model/1, get_session_id/1, get_recorded_at/1]).

-record(spending_recorded_v1, {
    venture_id    :: binary(),
    amount_usd    :: float(),
    new_total_usd :: float(),
    model         :: binary(),
    session_id    :: binary(),
    recorded_at   :: integer()
}).

-export_type([spending_recorded_v1/0]).
-opaque spending_recorded_v1() :: #spending_recorded_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> spending_recorded_v1().
new(#{venture_id := VentureId, amount_usd := AmountUsd,
      new_total_usd := NewTotalUsd, model := Model} = Params) ->
    #spending_recorded_v1{
        venture_id    = VentureId,
        amount_usd    = to_float(AmountUsd),
        new_total_usd = to_float(NewTotalUsd),
        model         = Model,
        session_id    = maps:get(session_id, Params, <<>>),
        recorded_at   = erlang:system_time(millisecond)
    }.

-spec to_map(spending_recorded_v1()) -> map().
to_map(#spending_recorded_v1{} = E) ->
    #{
        event_type    => <<"spending_recorded_v1">>,
        venture_id    => E#spending_recorded_v1.venture_id,
        amount_usd    => E#spending_recorded_v1.amount_usd,
        new_total_usd => E#spending_recorded_v1.new_total_usd,
        model         => E#spending_recorded_v1.model,
        session_id    => E#spending_recorded_v1.session_id,
        recorded_at   => E#spending_recorded_v1.recorded_at
    }.

-spec from_map(map()) -> {ok, spending_recorded_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = gv(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #spending_recorded_v1{
                venture_id    = VentureId,
                amount_usd    = to_float(gv(amount_usd, Map, 0.0)),
                new_total_usd = to_float(gv(new_total_usd, Map, 0.0)),
                model         = gv(model, Map, <<>>),
                session_id    = gv(session_id, Map, <<>>),
                recorded_at   = gv(recorded_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_venture_id(spending_recorded_v1()) -> binary().
get_venture_id(#spending_recorded_v1{venture_id = V}) -> V.

-spec get_amount_usd(spending_recorded_v1()) -> float().
get_amount_usd(#spending_recorded_v1{amount_usd = V}) -> V.

-spec get_new_total_usd(spending_recorded_v1()) -> float().
get_new_total_usd(#spending_recorded_v1{new_total_usd = V}) -> V.

-spec get_model(spending_recorded_v1()) -> binary().
get_model(#spending_recorded_v1{model = V}) -> V.

-spec get_session_id(spending_recorded_v1()) -> binary().
get_session_id(#spending_recorded_v1{session_id = V}) -> V.

-spec get_recorded_at(spending_recorded_v1()) -> integer().
get_recorded_at(#spending_recorded_v1{recorded_at = V}) -> V.

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
