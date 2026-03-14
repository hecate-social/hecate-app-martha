%%% @doc retry_attempted_v1 event.
%%% Emitted when a retry attempt is made with adjustments.
-module(retry_attempted_v1).
-export([new/1, to_map/1, from_map/1]).

-record(retry_attempted_v1, {
    session_id     :: binary(),
    attempt_number :: non_neg_integer(),
    adjustment     :: map(),
    attempted_at   :: integer()
}).

-opaque retry_attempted_v1() :: #retry_attempted_v1{}.
-export_type([retry_attempted_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

new(#{session_id := SId} = P) ->
    #retry_attempted_v1{
        session_id = SId,
        attempt_number = maps:get(attempt_number, P, 1),
        adjustment = maps:get(adjustment, P, #{}),
        attempted_at = erlang:system_time(millisecond)
    }.

to_map(#retry_attempted_v1{} = E) ->
    #{
        event_type => <<"retry_attempted_v1">>,
        session_id => E#retry_attempted_v1.session_id,
        attempt_number => E#retry_attempted_v1.attempt_number,
        adjustment => E#retry_attempted_v1.adjustment,
        attempted_at => E#retry_attempted_v1.attempted_at
    }.

from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #retry_attempted_v1{
                session_id = SId,
                attempt_number = gv(attempt_number, Map, 1),
                adjustment = gv(adjustment, Map, #{}),
                attempted_at = gv(attempted_at, Map, 0)
            }}
    end.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
