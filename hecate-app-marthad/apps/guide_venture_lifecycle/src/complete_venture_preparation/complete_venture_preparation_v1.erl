%%% @doc complete_venture_preparation_v1 command
%%% Marks venture knowledge preparation as complete.
%%% Can be triggered manually or by a PM when all research agents finish.
-module(complete_venture_preparation_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([command_type/0]).
-export([get_venture_id/1]).

-record(complete_venture_preparation_v1, {
    venture_id :: binary()
}).

-export_type([complete_venture_preparation_v1/0]).
-opaque complete_venture_preparation_v1() :: #complete_venture_preparation_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, complete_venture_preparation_v1()} | {error, term()}.
-spec command_type() -> atom().
command_type() -> complete_venture_preparation_v1.

new(#{venture_id := VentureId}) when is_binary(VentureId) ->
    {ok, #complete_venture_preparation_v1{venture_id = VentureId}};
new(_) ->
    {error, missing_required_fields}.

-spec validate(complete_venture_preparation_v1()) ->
    {ok, complete_venture_preparation_v1()} | {error, term()}.
validate(#complete_venture_preparation_v1{venture_id = V}) when
    not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#complete_venture_preparation_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(complete_venture_preparation_v1()) -> map().
to_map(#complete_venture_preparation_v1{} = Cmd) ->
    #{
        command_type => complete_venture_preparation_v1,
        venture_id => Cmd#complete_venture_preparation_v1.venture_id
    }.

-spec from_map(map()) -> {ok, complete_venture_preparation_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ ->
            {ok, #complete_venture_preparation_v1{venture_id = VentureId}}
    end.

%% Accessors
-spec get_venture_id(complete_venture_preparation_v1()) -> binary().
get_venture_id(#complete_venture_preparation_v1{venture_id = V}) -> V.

%% Internal
get_value(Key, Map) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            case maps:find(BinKey, Map) of
                {ok, V} -> V;
                error -> undefined
            end
    end.
