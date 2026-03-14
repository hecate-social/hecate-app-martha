%%% @doc Integration fact for venture_initiated_v1 events.
%%% Translates the domain event into a serializable payload
%%% for external consumption via pg/mesh.
-module(venture_initiated_fact_v1).
-behaviour(evoq_fact).

-export([fact_type/0, from_event/3]).
-export([serialize/1, deserialize/1]).

-spec fact_type() -> binary().
fact_type() -> <<"hecate.venture.initiated">>.

-spec from_event(atom(), map(), map()) -> {ok, map()} | skip.
from_event(venture_initiated_v1, Data, _Metadata) ->
    {ok, #{
        <<"venture_id">> => maps:get(venture_id, Data),
        <<"name">> => maps:get(name, Data),
        <<"brief">> => maps:get(brief, Data, null),
        <<"initiated_by">> => maps:get(initiated_by, Data, null),
        <<"initiated_at">> => maps:get(initiated_at, Data)
    }};
from_event(_Other, _Data, _Metadata) ->
    skip.

-spec serialize(map()) -> {ok, binary()} | {error, term()}.
serialize(Payload) ->
    evoq_fact:default_serialize(Payload).

-spec deserialize(binary()) -> {ok, map()} | {error, term()}.
deserialize(Binary) ->
    evoq_fact:default_deserialize(Binary).
