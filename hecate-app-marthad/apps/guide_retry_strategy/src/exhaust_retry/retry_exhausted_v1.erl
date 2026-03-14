%%% @doc retry_exhausted_v1 event.
%%% Emitted when all retry attempts have been used up.
-module(retry_exhausted_v1).
-export([new/1, to_map/1, from_map/1]).

-record(retry_exhausted_v1, {
    session_id   :: binary(),
    exhausted_at :: integer()
}).

-opaque retry_exhausted_v1() :: #retry_exhausted_v1{}.
-export_type([retry_exhausted_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

new(#{session_id := SId}) ->
    #retry_exhausted_v1{
        session_id = SId,
        exhausted_at = erlang:system_time(millisecond)
    }.

to_map(#retry_exhausted_v1{} = E) ->
    #{
        event_type => <<"retry_exhausted_v1">>,
        session_id => E#retry_exhausted_v1.session_id,
        exhausted_at => E#retry_exhausted_v1.exhausted_at
    }.

from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #retry_exhausted_v1{
                session_id = SId,
                exhausted_at = gv(exhausted_at, Map, 0)
            }}
    end.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
