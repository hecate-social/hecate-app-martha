%%% @doc prepare_venture_knowledge_v1 command
%%% Initiates knowledge preparation for a venture.
%%% Domain expert agents will research topics derived from the vision.
-module(prepare_venture_knowledge_v1).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([get_venture_id/1, get_research_topics/1]).

-record(prepare_venture_knowledge_v1, {
    venture_id      :: binary(),
    research_topics :: [binary()]
}).

-export_type([prepare_venture_knowledge_v1/0]).
-opaque prepare_venture_knowledge_v1() :: #prepare_venture_knowledge_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, prepare_venture_knowledge_v1()} | {error, term()}.
new(#{venture_id := VentureId, research_topics := Topics}) when
    is_binary(VentureId), is_list(Topics) ->
    {ok, #prepare_venture_knowledge_v1{
        venture_id = VentureId,
        research_topics = Topics
    }};
new(#{venture_id := VentureId}) when is_binary(VentureId) ->
    {ok, #prepare_venture_knowledge_v1{
        venture_id = VentureId,
        research_topics = []
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(prepare_venture_knowledge_v1()) ->
    {ok, prepare_venture_knowledge_v1()} | {error, term()}.
validate(#prepare_venture_knowledge_v1{venture_id = VentureId}) when
    not is_binary(VentureId); byte_size(VentureId) =:= 0 ->
    {error, invalid_venture_id};
validate(#prepare_venture_knowledge_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(prepare_venture_knowledge_v1()) -> map().
to_map(#prepare_venture_knowledge_v1{} = Cmd) ->
    #{
        <<"command_type">> => <<"prepare_venture_knowledge">>,
        <<"venture_id">> => Cmd#prepare_venture_knowledge_v1.venture_id,
        <<"research_topics">> => Cmd#prepare_venture_knowledge_v1.research_topics
    }.

-spec from_map(map()) -> {ok, prepare_venture_knowledge_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    Topics = get_value(research_topics, Map, []),
    case VentureId of
        undefined -> {error, missing_required_fields};
        _ ->
            {ok, #prepare_venture_knowledge_v1{
                venture_id = VentureId,
                research_topics = Topics
            }}
    end.

%% Accessors
-spec get_venture_id(prepare_venture_knowledge_v1()) -> binary().
get_venture_id(#prepare_venture_knowledge_v1{venture_id = V}) -> V.

-spec get_research_topics(prepare_venture_knowledge_v1()) -> [binary()].
get_research_topics(#prepare_venture_knowledge_v1{research_topics = V}) -> V.

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
