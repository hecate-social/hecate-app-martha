%%% @doc retry_succeeded_v1 event.
%%% Emitted when a retried agent session succeeds.
-module(retry_succeeded_v1).

-behaviour(evoq_event).
-export([new/1, to_map/1, from_map/1]).
-export([event_type/0]).

-record(retry_succeeded_v1, {
    session_id   :: binary(),
    succeeded_at :: integer()
}).

-opaque retry_succeeded_v1() :: #retry_succeeded_v1{}.
-export_type([retry_succeeded_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec event_type() -> atom().
event_type() -> retry_succeeded_v1.

new(#{session_id := SId}) ->
    #retry_succeeded_v1{
        session_id = SId,
        succeeded_at = erlang:system_time(millisecond)
    }.

to_map(#retry_succeeded_v1{} = E) ->
    #{
        event_type => retry_succeeded_v1,
        session_id => E#retry_succeeded_v1.session_id,
        succeeded_at => E#retry_succeeded_v1.succeeded_at
    }.

from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #retry_succeeded_v1{
                session_id = SId,
                succeeded_at = gv(succeeded_at, Map, 0)
            }}
    end.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
