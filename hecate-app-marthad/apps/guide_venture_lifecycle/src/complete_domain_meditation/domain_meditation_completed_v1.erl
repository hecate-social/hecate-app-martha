%%% @doc domain_meditation_completed_v1 event
%%% Emitted when domain meditation phase ends.
-module(domain_meditation_completed_v1).

-behaviour(evoq_event).

-export([new/1, from_map/1, to_map/1, event_type/0]).
-export([get_venture_id/1, get_completed_at/1]).

-record(domain_meditation_completed_v1, {
    venture_id   :: binary(),
    completed_at :: non_neg_integer()
}).

-export_type([domain_meditation_completed_v1/0]).
-opaque domain_meditation_completed_v1() :: #domain_meditation_completed_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec event_type() -> atom().
event_type() -> domain_meditation_completed_v1.

-spec new(map()) -> domain_meditation_completed_v1().
new(#{venture_id := VentureId} = Params) ->
    #domain_meditation_completed_v1{
        venture_id = VentureId,
        completed_at = maps:get(completed_at, Params, erlang:system_time(millisecond))
    }.

-spec to_map(domain_meditation_completed_v1()) -> map().
to_map(#domain_meditation_completed_v1{venture_id = V, completed_at = C}) ->
    #{
        event_type => domain_meditation_completed_v1,
        venture_id => V,
        completed_at => C
    }.

-spec from_map(map()) -> {ok, domain_meditation_completed_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #domain_meditation_completed_v1{
                venture_id = VentureId,
                completed_at = get_value(completed_at, Map, erlang:system_time(millisecond))
            }}
    end.

-spec get_venture_id(domain_meditation_completed_v1()) -> binary().
get_venture_id(#domain_meditation_completed_v1{venture_id = V}) -> V.

-spec get_completed_at(domain_meditation_completed_v1()) -> non_neg_integer().
get_completed_at(#domain_meditation_completed_v1{completed_at = V}) -> V.

%% Internal
get_value(Key, Map) -> get_value(Key, Map, undefined).

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
