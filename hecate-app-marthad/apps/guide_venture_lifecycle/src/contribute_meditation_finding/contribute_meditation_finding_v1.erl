%%% @doc contribute_meditation_finding_v1 command
%%% A storm participant contributes a structured finding from domain research.
-module(contribute_meditation_finding_v1).

-behaviour(evoq_command).

-export([new/1, from_map/1, validate/1, to_map/1, command_type/0]).
-export([get_venture_id/1, get_participant_id/1, get_finding_type/1,
         get_content/1, get_sources/1]).

-record(contribute_meditation_finding_v1, {
    venture_id     :: binary(),
    participant_id :: binary(),
    finding_type   :: binary(),
    content        :: binary(),
    sources = []   :: [map()]
}).

-export_type([contribute_meditation_finding_v1/0]).
-opaque contribute_meditation_finding_v1() :: #contribute_meditation_finding_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-define(VALID_FINDING_TYPES, [
    <<"domain_concept">>, <<"business_rule">>, <<"industry_pattern">>,
    <<"risk">>, <<"terminology">>, <<"prior_art">>
]).

-spec command_type() -> atom().
command_type() -> contribute_meditation_finding_v1.

-spec new(map()) -> {ok, contribute_meditation_finding_v1()} | {error, term()}.
new(#{venture_id := VId, participant_id := PId, finding_type := FT, content := C} = P) ->
    Cmd = #contribute_meditation_finding_v1{
        venture_id = VId,
        participant_id = PId,
        finding_type = FT,
        content = C,
        sources = maps:get(sources, P, [])
    },
    case validate(Cmd) of
        ok -> {ok, Cmd};
        {error, _} = Err -> Err
    end;
new(_) ->
    {error, missing_required_fields}.

-spec validate(contribute_meditation_finding_v1()) -> ok | {error, term()}.
validate(#contribute_meditation_finding_v1{venture_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, venture_id}};
validate(#contribute_meditation_finding_v1{participant_id = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, participant_id}};
validate(#contribute_meditation_finding_v1{content = V}) when not is_binary(V); V =:= <<>> ->
    {error, {invalid_field, content}};
validate(#contribute_meditation_finding_v1{finding_type = FT}) ->
    case lists:member(FT, ?VALID_FINDING_TYPES) of
        true -> ok;
        false -> {error, {invalid_finding_type, FT}}
    end.

-spec to_map(contribute_meditation_finding_v1()) -> map().
to_map(#contribute_meditation_finding_v1{} = C) ->
    #{
        command_type => contribute_meditation_finding_v1,
        venture_id => C#contribute_meditation_finding_v1.venture_id,
        participant_id => C#contribute_meditation_finding_v1.participant_id,
        finding_type => C#contribute_meditation_finding_v1.finding_type,
        content => C#contribute_meditation_finding_v1.content,
        sources => C#contribute_meditation_finding_v1.sources
    }.

-spec from_map(map()) -> {ok, contribute_meditation_finding_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    ParticipantId = get_value(participant_id, Map),
    FindingType = get_value(finding_type, Map),
    Content = get_value(content, Map),
    Sources = get_value(sources, Map, []),
    case {VentureId, ParticipantId, FindingType, Content} of
        {undefined, _, _, _} -> {error, missing_required_fields};
        {_, undefined, _, _} -> {error, missing_required_fields};
        {_, _, undefined, _} -> {error, missing_required_fields};
        {_, _, _, undefined} -> {error, missing_required_fields};
        _ -> new(#{
            venture_id => VentureId,
            participant_id => ParticipantId,
            finding_type => FindingType,
            content => Content,
            sources => Sources
        })
    end.

-spec get_venture_id(contribute_meditation_finding_v1()) -> binary().
get_venture_id(#contribute_meditation_finding_v1{venture_id = V}) -> V.

-spec get_participant_id(contribute_meditation_finding_v1()) -> binary().
get_participant_id(#contribute_meditation_finding_v1{participant_id = V}) -> V.

-spec get_finding_type(contribute_meditation_finding_v1()) -> binary().
get_finding_type(#contribute_meditation_finding_v1{finding_type = V}) -> V.

-spec get_content(contribute_meditation_finding_v1()) -> binary().
get_content(#contribute_meditation_finding_v1{content = V}) -> V.

-spec get_sources(contribute_meditation_finding_v1()) -> [map()].
get_sources(#contribute_meditation_finding_v1{sources = V}) -> V.

%% Internal
get_value(Key, Map) -> get_value(Key, Map, undefined).

get_value(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error ->
            case maps:find(BinKey, Map) of
                {ok, V} -> V;
                error -> Default
            end
    end.
