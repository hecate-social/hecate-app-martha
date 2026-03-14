%%% @doc research_brief_contributed_v1 event
%%% Emitted when an agent contributes a research brief for a topic.
-module(research_brief_contributed_v1).

-export([new/1, to_map/1, from_map/1]).
-export([get_venture_id/1, get_topic/1, get_brief/1, get_agent_role/1,
         get_contributed_at/1]).

-record(research_brief_contributed_v1, {
    venture_id     :: binary(),
    topic          :: binary(),
    brief          :: binary(),
    agent_role     :: binary(),
    contributed_at :: integer()
}).

-export_type([research_brief_contributed_v1/0]).
-opaque research_brief_contributed_v1() :: #research_brief_contributed_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> research_brief_contributed_v1().
new(#{venture_id := VentureId, topic := Topic, brief := Brief,
      agent_role := Role}) ->
    #research_brief_contributed_v1{
        venture_id = VentureId,
        topic = Topic,
        brief = Brief,
        agent_role = Role,
        contributed_at = erlang:system_time(millisecond)
    }.

-spec to_map(research_brief_contributed_v1()) -> map().
to_map(#research_brief_contributed_v1{} = E) ->
    #{
        event_type => <<"research_brief_contributed_v1">>,
        venture_id => E#research_brief_contributed_v1.venture_id,
        topic => E#research_brief_contributed_v1.topic,
        brief => E#research_brief_contributed_v1.brief,
        agent_role => E#research_brief_contributed_v1.agent_role,
        contributed_at => E#research_brief_contributed_v1.contributed_at
    }.

-spec from_map(map()) -> {ok, research_brief_contributed_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    case VentureId of
        undefined -> {error, invalid_event};
        _ ->
            {ok, #research_brief_contributed_v1{
                venture_id = VentureId,
                topic = get_value(topic, Map),
                brief = get_value(brief, Map),
                agent_role = get_value(agent_role, Map),
                contributed_at = get_value(contributed_at, Map,
                    erlang:system_time(millisecond))
            }}
    end.

%% Accessors
-spec get_venture_id(research_brief_contributed_v1()) -> binary().
get_venture_id(#research_brief_contributed_v1{venture_id = V}) -> V.

-spec get_topic(research_brief_contributed_v1()) -> binary().
get_topic(#research_brief_contributed_v1{topic = V}) -> V.

-spec get_brief(research_brief_contributed_v1()) -> binary().
get_brief(#research_brief_contributed_v1{brief = V}) -> V.

-spec get_agent_role(research_brief_contributed_v1()) -> binary().
get_agent_role(#research_brief_contributed_v1{agent_role = V}) -> V.

-spec get_contributed_at(research_brief_contributed_v1()) -> integer().
get_contributed_at(#research_brief_contributed_v1{contributed_at = V}) -> V.

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
