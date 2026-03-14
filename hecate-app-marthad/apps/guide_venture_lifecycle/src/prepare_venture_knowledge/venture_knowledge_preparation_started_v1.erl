%%% @doc venture_knowledge_preparation_started_v1 event
%%% Emitted when knowledge preparation begins for a venture.
%%% Domain expert agents will research the identified topics.
-module(venture_knowledge_preparation_started_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_venture_id/1, get_research_topics/1, get_started_at/1]).

-record(venture_knowledge_preparation_started_v1, {
    venture_id      :: binary(),
    research_topics :: [binary()],
    started_at      :: integer()
}).

-export_type([venture_knowledge_preparation_started_v1/0]).
-opaque venture_knowledge_preparation_started_v1() ::
    #venture_knowledge_preparation_started_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> venture_knowledge_preparation_started_v1().
new(#{venture_id := VentureId, research_topics := Topics}) ->
    #venture_knowledge_preparation_started_v1{
        venture_id = VentureId,
        research_topics = Topics,
        started_at = erlang:system_time(millisecond)
    }.

-spec to_map(venture_knowledge_preparation_started_v1()) -> map().
to_map(#venture_knowledge_preparation_started_v1{} = E) ->
    #{
        event_type => <<"venture_knowledge_preparation_started_v1">>,
        venture_id => E#venture_knowledge_preparation_started_v1.venture_id,
        research_topics => E#venture_knowledge_preparation_started_v1.research_topics,
        started_at => E#venture_knowledge_preparation_started_v1.started_at
    }.

-spec from_map(map()) -> {ok, venture_knowledge_preparation_started_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #venture_knowledge_preparation_started_v1{
                venture_id = VentureId,
                research_topics = get_value(research_topics, Map, []),
                started_at = get_value(started_at, Map, erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_venture_id(venture_knowledge_preparation_started_v1()) -> binary().
get_venture_id(#venture_knowledge_preparation_started_v1{venture_id = V}) -> V.

-spec get_research_topics(venture_knowledge_preparation_started_v1()) -> [binary()].
get_research_topics(#venture_knowledge_preparation_started_v1{research_topics = V}) -> V.

-spec get_started_at(venture_knowledge_preparation_started_v1()) -> integer().
get_started_at(#venture_knowledge_preparation_started_v1{started_at = V}) -> V.

%% Internal
get_value(Key, Map) ->
    get_value(Key, Map, undefined).

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
