%%% @doc start_domain_meditation_v1 command
%%% Starts the domain meditation phase before Big Picture Event Storming.
-module(start_domain_meditation_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1, command_type/0]).
-export([get_venture_id/1]).

-record(start_domain_meditation_v1, {
    venture_id :: binary()
}).

-export_type([start_domain_meditation_v1/0]).
-opaque start_domain_meditation_v1() :: #start_domain_meditation_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec command_type() -> atom().
command_type() -> start_domain_meditation_v1.

-spec new(map()) -> {ok, start_domain_meditation_v1()} | {error, term()}.
new(#{venture_id := VentureId}) ->
    Cmd = #start_domain_meditation_v1{venture_id = VentureId},
    case validate(Cmd) of
        ok -> {ok, Cmd};
        {error, _} = Err -> Err
    end;
new(_) ->
    {error, missing_required_fields}.

-spec validate(start_domain_meditation_v1()) -> ok | {error, term()}.
validate(#start_domain_meditation_v1{venture_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, venture_id}};
validate(_) -> ok.

-spec to_map(start_domain_meditation_v1()) -> map().
to_map(#start_domain_meditation_v1{venture_id = V}) ->
    #{command_type => start_domain_meditation_v1, venture_id => V}.

-spec from_map(map()) -> {ok, start_domain_meditation_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ -> new(#{venture_id => VentureId})
    end.

-spec get_venture_id(start_domain_meditation_v1()) -> binary().
get_venture_id(#start_domain_meditation_v1{venture_id = V}) -> V.

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
