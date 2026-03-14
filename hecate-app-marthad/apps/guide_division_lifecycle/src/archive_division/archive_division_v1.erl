-module(archive_division_v1).

-behaviour(evoq_command).
-export([new/1, from_map/1, validate/1, to_map/1]).
-export([command_type/0]).
-export([get_division_id/1]).

-record(archive_division_v1, {division_id :: binary()}).
-export_type([archive_division_v1/0]).
-opaque archive_division_v1() :: #archive_division_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec command_type() -> atom().
command_type() -> archive_division_v1.

new(#{division_id := DivisionId}) ->
    {ok, #archive_division_v1{division_id = DivisionId}};
new(_) -> {error, missing_required_fields}.

validate(#archive_division_v1{division_id = D}) when not is_binary(D); byte_size(D) =:= 0 ->
    {error, invalid_division_id};
validate(#archive_division_v1{} = Cmd) -> {ok, Cmd}.

to_map(#archive_division_v1{} = Cmd) ->
    #{command_type => archive_division_v1,
      division_id => Cmd#archive_division_v1.division_id}.

from_map(Map) ->
    DivisionId = get_value(division_id, Map),
    case DivisionId of
        undefined -> {error, missing_required_fields};
        _ -> {ok, #archive_division_v1{division_id = DivisionId}}
    end.

get_division_id(#archive_division_v1{division_id = V}) -> V.

get_value(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
