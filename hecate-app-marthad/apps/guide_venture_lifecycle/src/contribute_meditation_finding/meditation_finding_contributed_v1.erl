%%% @doc meditation_finding_contributed_v1 event
%%% Emitted when a participant contributes a research finding during meditation.
-module(meditation_finding_contributed_v1).

-behaviour(evoq_event).

-export([new/1, from_map/1, to_map/1, event_type/0]).
-export([get_venture_id/1, get_participant_id/1, get_finding_type/1,
         get_content/1, get_sources/1, get_contributed_at/1]).

-record(meditation_finding_contributed_v1, {
    venture_id     :: binary(),
    participant_id :: binary(),
    finding_type   :: binary(),
    content        :: binary(),
    sources = []   :: [map()],
    contributed_at :: non_neg_integer()
}).

-export_type([meditation_finding_contributed_v1/0]).
-opaque meditation_finding_contributed_v1() :: #meditation_finding_contributed_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec event_type() -> atom().
event_type() -> meditation_finding_contributed_v1.

-spec new(map()) -> meditation_finding_contributed_v1().
new(#{venture_id := VId, participant_id := PId, finding_type := FT, content := C} = P) ->
    #meditation_finding_contributed_v1{
        venture_id = VId,
        participant_id = PId,
        finding_type = FT,
        content = C,
        sources = maps:get(sources, P, []),
        contributed_at = maps:get(contributed_at, P, erlang:system_time(millisecond))
    }.

-spec to_map(meditation_finding_contributed_v1()) -> map().
to_map(#meditation_finding_contributed_v1{} = E) ->
    #{
        event_type => meditation_finding_contributed_v1,
        venture_id => E#meditation_finding_contributed_v1.venture_id,
        participant_id => E#meditation_finding_contributed_v1.participant_id,
        finding_type => E#meditation_finding_contributed_v1.finding_type,
        content => E#meditation_finding_contributed_v1.content,
        sources => E#meditation_finding_contributed_v1.sources,
        contributed_at => E#meditation_finding_contributed_v1.contributed_at
    }.

-spec from_map(map()) -> {ok, meditation_finding_contributed_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #meditation_finding_contributed_v1{
                venture_id = VentureId,
                participant_id = get_value(participant_id, Map),
                finding_type = get_value(finding_type, Map),
                content = get_value(content, Map),
                sources = get_value(sources, Map, []),
                contributed_at = get_value(contributed_at, Map, erlang:system_time(millisecond))
            }}
    end.

-spec get_venture_id(meditation_finding_contributed_v1()) -> binary().
get_venture_id(#meditation_finding_contributed_v1{venture_id = V}) -> V.

-spec get_participant_id(meditation_finding_contributed_v1()) -> binary().
get_participant_id(#meditation_finding_contributed_v1{participant_id = V}) -> V.

-spec get_finding_type(meditation_finding_contributed_v1()) -> binary().
get_finding_type(#meditation_finding_contributed_v1{finding_type = V}) -> V.

-spec get_content(meditation_finding_contributed_v1()) -> binary().
get_content(#meditation_finding_contributed_v1{content = V}) -> V.

-spec get_sources(meditation_finding_contributed_v1()) -> [map()].
get_sources(#meditation_finding_contributed_v1{sources = V}) -> V.

-spec get_contributed_at(meditation_finding_contributed_v1()) -> non_neg_integer().
get_contributed_at(#meditation_finding_contributed_v1{contributed_at = V}) -> V.

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
