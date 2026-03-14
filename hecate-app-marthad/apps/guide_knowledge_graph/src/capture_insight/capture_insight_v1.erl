%%% @doc Command: capture an insight into the knowledge graph.
-module(capture_insight_v1).

-export([new/1, from_map/1, to_map/1, validate/1]).
-export([get_venture_id/1, get_insight_id/1, get_content/1,
         get_source_agent/1, get_source_session/1, get_insight_type/1]).

-record(capture_insight_v1, {
    venture_id     :: binary(),
    insight_id     :: binary(),
    content        :: binary(),
    source_agent   :: binary() | undefined,
    source_session :: binary() | undefined,
    insight_type   :: binary() | undefined
}).

-opaque capture_insight_v1() :: #capture_insight_v1{}.
-export_type([capture_insight_v1/0]).

-spec new(map()) -> {ok, capture_insight_v1()} | {error, term()}.
new(#{venture_id := VId, content := Content} = Params) ->
    InsightId = maps:get(insight_id, Params, generate_id()),
    {ok, #capture_insight_v1{
        venture_id = VId,
        insight_id = InsightId,
        content = Content,
        source_agent = maps:get(source_agent, Params, undefined),
        source_session = maps:get(source_session, Params, undefined),
        insight_type = maps:get(insight_type, Params, <<"general">>)
    }};
new(_) ->
    {error, missing_required_fields}.

-spec from_map(map()) -> {ok, capture_insight_v1()} | {error, term()}.
from_map(Map) ->
    VId = gv(venture_id, Map),
    Content = gv(content, Map),
    case {VId, Content} of
        {undefined, _} -> {error, missing_required_fields};
        {_, undefined} -> {error, missing_required_fields};
        _ ->
            {ok, #capture_insight_v1{
                venture_id = VId,
                insight_id = gv(insight_id, Map, generate_id()),
                content = Content,
                source_agent = gv(source_agent, Map),
                source_session = gv(source_session, Map),
                insight_type = gv(insight_type, Map, <<"general">>)
            }}
    end.

-spec to_map(capture_insight_v1()) -> map().
to_map(#capture_insight_v1{} = C) ->
    #{command_type => <<"capture_insight">>,
      venture_id => C#capture_insight_v1.venture_id,
      insight_id => C#capture_insight_v1.insight_id,
      content => C#capture_insight_v1.content,
      source_agent => C#capture_insight_v1.source_agent,
      source_session => C#capture_insight_v1.source_session,
      insight_type => C#capture_insight_v1.insight_type}.

-spec validate(capture_insight_v1()) -> {ok, capture_insight_v1()} | {error, term()}.
validate(#capture_insight_v1{content = C}) when not is_binary(C); byte_size(C) =:= 0 ->
    {error, invalid_content};
validate(#capture_insight_v1{} = Cmd) ->
    {ok, Cmd}.

get_venture_id(#capture_insight_v1{venture_id = V}) -> V.
get_insight_id(#capture_insight_v1{insight_id = V}) -> V.
get_content(#capture_insight_v1{content = V}) -> V.
get_source_agent(#capture_insight_v1{source_agent = V}) -> V.
get_source_session(#capture_insight_v1{source_session = V}) -> V.
get_insight_type(#capture_insight_v1{insight_type = V}) -> V.

generate_id() ->
    Ts = integer_to_binary(erlang:system_time(millisecond)),
    Rand = binary:encode_hex(crypto:strong_rand_bytes(8)),
    <<"insight-", Ts/binary, "-", Rand/binary>>.

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
gv(Key, Map, Default) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, Default)
    end.
