-module(division_initiated_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_division_id/1, get_venture_id/1, get_context_name/1,
         get_initiated_by/1, get_initiated_at/1]).

-record(division_initiated_v1, {
    division_id  :: binary(),
    venture_id   :: binary(),
    context_name :: binary(),
    initiated_by :: binary() | undefined,
    initiated_at :: non_neg_integer()
}).

-export_type([division_initiated_v1/0]).
-opaque division_initiated_v1() :: #division_initiated_v1{}.

-spec new(map()) -> division_initiated_v1().
new(Params) ->
    #division_initiated_v1{
        division_id = maps:get(division_id, Params),
        venture_id = maps:get(venture_id, Params),
        context_name = maps:get(context_name, Params),
        initiated_by = maps:get(initiated_by, Params, undefined),
        initiated_at = erlang:system_time(millisecond)
    }.

-spec to_map(division_initiated_v1()) -> map().
to_map(#division_initiated_v1{} = E) ->
    #{
        event_type => <<"division_initiated_v1">>,
        division_id => E#division_initiated_v1.division_id,
        venture_id => E#division_initiated_v1.venture_id,
        context_name => E#division_initiated_v1.context_name,
        initiated_by => E#division_initiated_v1.initiated_by,
        initiated_at => E#division_initiated_v1.initiated_at
    }.

-spec from_map(map()) -> {ok, division_initiated_v1()} | {error, term()}.
from_map(Map) ->
    {ok, #division_initiated_v1{
        division_id = gv(division_id, Map),
        venture_id = gv(venture_id, Map),
        context_name = gv(context_name, Map),
        initiated_by = gv(initiated_by, Map),
        initiated_at = gv(initiated_at, Map)
    }}.

get_division_id(#division_initiated_v1{division_id = V}) -> V.
get_venture_id(#division_initiated_v1{venture_id = V}) -> V.
get_context_name(#division_initiated_v1{context_name = V}) -> V.
get_initiated_by(#division_initiated_v1{initiated_by = V}) -> V.
get_initiated_at(#division_initiated_v1{initiated_at = V}) -> V.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
