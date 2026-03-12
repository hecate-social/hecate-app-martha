-module(division_archived_v1).
-export([new/1, to_map/1, from_map/1]).
-export([get_division_id/1, get_archived_at/1]).

-record(division_archived_v1, {
    division_id :: binary(),
    archived_at :: non_neg_integer()
}).
-export_type([division_archived_v1/0]).
-opaque division_archived_v1() :: #division_archived_v1{}.

new(Params) ->
    #division_archived_v1{
        division_id = maps:get(division_id, Params),
        archived_at = erlang:system_time(millisecond)
    }.

to_map(#division_archived_v1{} = E) ->
    #{<<"event_type">> => <<"division_archived_v1">>,
      <<"division_id">> => E#division_archived_v1.division_id,
      <<"archived_at">> => E#division_archived_v1.archived_at}.

from_map(Map) ->
    {ok, #division_archived_v1{
        division_id = gv(division_id, Map),
        archived_at = gv(archived_at, Map)
    }}.

get_division_id(#division_archived_v1{division_id = V}) -> V.
get_archived_at(#division_archived_v1{archived_at = V}) -> V.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
