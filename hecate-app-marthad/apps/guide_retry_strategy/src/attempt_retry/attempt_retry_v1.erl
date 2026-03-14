%%% @doc attempt_retry_v1 command.
%%% Records an attempt to retry with specific adjustments.
-module(attempt_retry_v1).

-behaviour(evoq_command).
-export([new/1, from_map/1, to_map/1]).
-export([command_type/0]).
-export([get_session_id/1, get_adjustment/1]).

-record(attempt_retry_v1, {
    session_id  :: binary(),
    adjustment  :: map()
}).

-opaque attempt_retry_v1() :: #attempt_retry_v1{}.
-export_type([attempt_retry_v1/0]).
-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, attempt_retry_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> attempt_retry_v1.

new(#{session_id := SId} = P) ->
    {ok, #attempt_retry_v1{
        session_id = SId,
        adjustment = maps:get(adjustment, P, #{})
    }};
new(_) ->
    {error, missing_required_fields}.

to_map(#attempt_retry_v1{} = C) ->
    #{
        command_type => attempt_retry_v1,
        session_id => C#attempt_retry_v1.session_id,
        adjustment => C#attempt_retry_v1.adjustment
    }.

from_map(Map) ->
    SId = gv(session_id, Map),
    case SId of
        undefined -> {error, missing_required_fields};
        _ ->
            {ok, #attempt_retry_v1{
                session_id = SId,
                adjustment = gv(adjustment, Map, #{})
            }}
    end.

get_session_id(#attempt_retry_v1{session_id = V}) -> V.
get_adjustment(#attempt_retry_v1{adjustment = V}) -> V.

gv(Key, Map) -> gv(Key, Map, undefined).
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
