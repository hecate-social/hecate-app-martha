%%% @doc succeed_retry_v1 command.
%%% Marks a retry strategy as succeeded (agent completed after retry).
-module(succeed_retry_v1).

-behaviour(evoq_command).
-export([new/1, from_map/1, to_map/1]).
-export([command_type/0]).
-export([get_session_id/1]).

-record(succeed_retry_v1, {
    session_id :: binary()
}).

-opaque succeed_retry_v1() :: #succeed_retry_v1{}.
-export_type([succeed_retry_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec command_type() -> atom().
command_type() -> succeed_retry_v1.

new(#{session_id := SId}) ->
    {ok, #succeed_retry_v1{session_id = SId}};
new(_) ->
    {error, missing_required_fields}.

to_map(#succeed_retry_v1{} = C) ->
    #{
        command_type => succeed_retry_v1,
        session_id => C#succeed_retry_v1.session_id
    }.

from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, missing_required_fields};
        _ -> {ok, #succeed_retry_v1{session_id = SId}}
    end.

get_session_id(#succeed_retry_v1{session_id = V}) -> V.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
