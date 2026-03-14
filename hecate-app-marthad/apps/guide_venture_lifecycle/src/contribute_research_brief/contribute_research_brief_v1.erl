%%% @doc contribute_research_brief_v1 command
%%% An agent contributes a research brief for a specific topic.
-module(contribute_research_brief_v1).

-export([new/1, from_map/1, validate/1, to_map/1]).
-export([get_venture_id/1, get_topic/1, get_brief/1, get_agent_role/1]).

-record(contribute_research_brief_v1, {
    venture_id :: binary(),
    topic      :: binary(),
    brief      :: binary(),
    agent_role :: binary()
}).

-export_type([contribute_research_brief_v1/0]).
-opaque contribute_research_brief_v1() :: #contribute_research_brief_v1{}.

-dialyzer({nowarn_function, [new/1, from_map/1]}).

-spec new(map()) -> {ok, contribute_research_brief_v1()} | {error, term()}.
new(#{venture_id := VentureId, topic := Topic, brief := Brief, agent_role := Role}) when
    is_binary(VentureId), is_binary(Topic), is_binary(Brief), is_binary(Role) ->
    {ok, #contribute_research_brief_v1{
        venture_id = VentureId,
        topic = Topic,
        brief = Brief,
        agent_role = Role
    }};
new(_) ->
    {error, missing_required_fields}.

-spec validate(contribute_research_brief_v1()) ->
    {ok, contribute_research_brief_v1()} | {error, term()}.
validate(#contribute_research_brief_v1{venture_id = V}) when
    not is_binary(V); byte_size(V) =:= 0 ->
    {error, invalid_venture_id};
validate(#contribute_research_brief_v1{topic = T}) when
    not is_binary(T); byte_size(T) =:= 0 ->
    {error, invalid_topic};
validate(#contribute_research_brief_v1{brief = B}) when
    not is_binary(B); byte_size(B) =:= 0 ->
    {error, invalid_brief};
validate(#contribute_research_brief_v1{} = Cmd) ->
    {ok, Cmd}.

-spec to_map(contribute_research_brief_v1()) -> map().
to_map(#contribute_research_brief_v1{} = Cmd) ->
    #{
        command_type => <<"contribute_research_brief">>,
        venture_id => Cmd#contribute_research_brief_v1.venture_id,
        topic => Cmd#contribute_research_brief_v1.topic,
        brief => Cmd#contribute_research_brief_v1.brief,
        agent_role => Cmd#contribute_research_brief_v1.agent_role
    }.

-spec from_map(map()) -> {ok, contribute_research_brief_v1()} | {error, term()}.
from_map(Map) ->
    VentureId = get_value(venture_id, Map),
    Topic = get_value(topic, Map),
    Brief = get_value(brief, Map),
    Role = get_value(agent_role, Map),
    case {VentureId, Topic, Brief, Role} of
        {undefined, _, _, _} -> {error, missing_required_fields};
        {_, undefined, _, _} -> {error, missing_required_fields};
        {_, _, undefined, _} -> {error, missing_required_fields};
        {_, _, _, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #contribute_research_brief_v1{
                venture_id = VentureId,
                topic = Topic,
                brief = Brief,
                agent_role = Role
            }}
    end.

%% Accessors
-spec get_venture_id(contribute_research_brief_v1()) -> binary().
get_venture_id(#contribute_research_brief_v1{venture_id = V}) -> V.

-spec get_topic(contribute_research_brief_v1()) -> binary().
get_topic(#contribute_research_brief_v1{topic = V}) -> V.

-spec get_brief(contribute_research_brief_v1()) -> binary().
get_brief(#contribute_research_brief_v1{brief = V}) -> V.

-spec get_agent_role(contribute_research_brief_v1()) -> binary().
get_agent_role(#contribute_research_brief_v1{agent_role = V}) -> V.

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
