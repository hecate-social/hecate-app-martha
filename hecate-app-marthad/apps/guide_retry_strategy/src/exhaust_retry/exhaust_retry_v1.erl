%%% @doc exhaust_retry_v1 command.
%%% Marks a retry strategy as exhausted (no more attempts).
-module(exhaust_retry_v1).

-behaviour(evoq_command).
-export([new/1, from_map/1, to_map/1]).
-export([command_type/0]).
-export([get_session_id/1]).

-record(exhaust_retry_v1, {
    session_id :: binary()
}).

-opaque exhaust_retry_v1() :: #exhaust_retry_v1{}.
-export_type([exhaust_retry_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec command_type() -> atom().
command_type() -> exhaust_retry_v1.

new(#{session_id := SId}) ->
    {ok, #exhaust_retry_v1{session_id = SId}};
new(_) ->
    {error, missing_required_fields}.

to_map(#exhaust_retry_v1{} = C) ->
    #{
        command_type => exhaust_retry_v1,
        session_id => C#exhaust_retry_v1.session_id
    }.

from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, missing_required_fields};
        _ -> {ok, #exhaust_retry_v1{session_id = SId}}
    end.

get_session_id(#exhaust_retry_v1{session_id = V}) -> V.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
