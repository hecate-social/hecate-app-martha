%%% @doc planning_submitted_v1 event
%%% Emitted when a division planning dossier is submitted.
%%% Signals readiness to crafting. Does not close planning.
%%% Subsequent design/plan work clears the SUBMITTED flag.
%%% Carries aggregate identity (venture_id, context_name) for downstream PMs.
-module(planning_submitted_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_division_id/1, get_venture_id/1, get_context_name/1, get_submitted_at/1]).

-record(planning_submitted_v1, {
    division_id  :: binary(),
    venture_id   :: binary(),
    context_name :: binary(),
    submitted_at :: non_neg_integer()
}).

-export_type([planning_submitted_v1/0]).
-opaque planning_submitted_v1() :: #planning_submitted_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> planning_submitted_v1().
new(#{division_id := DivisionId, venture_id := VentureId, context_name := ContextName}) ->
    #planning_submitted_v1{
        division_id = DivisionId,
        venture_id = VentureId,
        context_name = ContextName,
        submitted_at = erlang:system_time(millisecond)
    }.

-spec to_map(planning_submitted_v1()) -> map().
to_map(#planning_submitted_v1{} = E) ->
    #{
        <<"event_type">> => <<"planning_submitted_v1">>,
        <<"division_id">> => E#planning_submitted_v1.division_id,
        <<"venture_id">> => E#planning_submitted_v1.venture_id,
        <<"context_name">> => E#planning_submitted_v1.context_name,
        <<"submitted_at">> => E#planning_submitted_v1.submitted_at
    }.

-spec from_map(map()) -> {ok, planning_submitted_v1()} | {error, term()}.
from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    case DivisionId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #planning_submitted_v1{
                division_id = DivisionId,
                venture_id = get_value(venture_id, Map),
                context_name = get_value(context_name, Map),
                submitted_at = get_value(submitted_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_division_id(planning_submitted_v1()) -> binary().
get_division_id(#planning_submitted_v1{division_id = V}) -> V.

-spec get_venture_id(planning_submitted_v1()) -> binary().
get_venture_id(#planning_submitted_v1{venture_id = V}) -> V.

-spec get_context_name(planning_submitted_v1()) -> binary().
get_context_name(#planning_submitted_v1{context_name = V}) -> V.

-spec get_submitted_at(planning_submitted_v1()) -> non_neg_integer().
get_submitted_at(#planning_submitted_v1{submitted_at = V}) -> V.

%% Internal
get_value(Key, Map) ->
    get_value(Key, Map, undefined).

get_value(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            case maps:find(BinKey, Map) of
                {ok, V} -> V;
                error -> Default
            end
    end.
