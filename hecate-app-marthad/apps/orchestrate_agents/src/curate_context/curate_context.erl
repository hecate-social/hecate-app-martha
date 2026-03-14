%%% @doc Context curator — relevance-ranked knowledge injection into agent prompts.
%%% Stateless desk in serve_llm. Fetches the knowledge graph for a venture,
%%% scores entries by role relevance, and returns a formatted markdown section
%%% suitable for appending to a system prompt.
-module(curate_context).
-export([curate/3]).

-define(DEFAULT_MAX_BYTES, 8000).  %% ~2000 tokens

%% @doc Curate relevant knowledge context for an agent role.
%% Returns a formatted markdown binary to append to the system prompt.
%% Opts: #{max_bytes => integer()}
-spec curate(binary(), binary(), map()) -> {ok, binary()}.
curate(VentureId, AgentRole, Opts) ->
    MaxBytes = maps:get(max_bytes, Opts, ?DEFAULT_MAX_BYTES),
    case project_knowledge_graph_store:get_graph(VentureId) of
        {ok, Graph} ->
            Insights = get_active_insights(Graph),
            Entities = get_entity_list(Graph),
            {ok, format_context(AgentRole, Insights, Entities, MaxBytes)};
        {error, not_found} ->
            {ok, <<>>}
    end.

%% --- Internal ---

get_active_insights(#{insights := Insights}) when is_list(Insights) ->
    [I || I <- Insights, not is_superseded(I)];
get_active_insights(#{<<"insights">> := Insights}) when is_list(Insights) ->
    [I || I <- Insights, not is_superseded(I)];
get_active_insights(_) ->
    [].

is_superseded(#{superseded := true}) -> true;
is_superseded(#{<<"superseded">> := true}) -> true;
is_superseded(_) -> false.

get_entity_list(#{entities := Entities}) when is_map(Entities) ->
    maps:values(Entities);
get_entity_list(#{<<"entities">> := Entities}) when is_map(Entities) ->
    maps:values(Entities);
get_entity_list(_) ->
    [].

format_context(Role, Insights, Entities, MaxBytes) ->
    ScoredInsights = curate_context_relevance:score_insights(Role, Insights),
    ScoredEntities = curate_context_relevance:score_entities(Role, Entities),
    %% Filter to relevant items only (score > 1)
    RelevantInsights = [{S, I} || {S, I} <- ScoredInsights, S > 1],
    RelevantEntities = [{S, E} || {S, E} <- ScoredEntities, S > 1],
    build_markdown(RelevantInsights, RelevantEntities, MaxBytes).

build_markdown([], [], _MaxBytes) ->
    <<>>;
build_markdown(Insights, Entities, MaxBytes) ->
    Header = <<"\n\n---\n## Prior Knowledge (from previous agents)\n\n">>,
    InsightSection = format_insights(Insights),
    EntitySection = format_entities(Entities),
    Full = <<Header/binary, InsightSection/binary, EntitySection/binary>>,
    truncate(Full, MaxBytes).

format_insights([]) ->
    <<>>;
format_insights(Insights) ->
    Header = <<"### Key Insights\n\n">>,
    Lines = [format_insight(I) || {_Score, I} <- Insights],
    Body = iolist_to_binary(Lines),
    <<Header/binary, Body/binary, "\n">>.

format_insight(Insight) ->
    Content = gv(<<"content">>, Insight, <<>>),
    Type = gv(<<"insight_type">>, Insight, <<"general">>),
    Source = gv(<<"source_agent">>, Insight, <<"unknown">>),
    iolist_to_binary([<<"- [">>, Type, <<"] (from ">>, Source, <<"): ">>, Content, <<"\n">>]).

format_entities([]) ->
    <<>>;
format_entities(Entities) ->
    Header = <<"### Known Entities\n\n">>,
    Lines = [format_entity(E) || {_Score, E} <- Entities],
    Body = iolist_to_binary(Lines),
    <<Header/binary, Body/binary, "\n">>.

format_entity(Entity) ->
    Name = gv(<<"name">>, Entity, <<>>),
    Type = gv(<<"type">>, Entity, <<>>),
    Desc = gv(<<"description">>, Entity, <<>>),
    case Desc of
        <<>> -> iolist_to_binary([<<"- **">>, Name, <<"** (">>, Type, <<")\n">>]);
        _ -> iolist_to_binary([<<"- **">>, Name, <<"** (">>, Type, <<"): ">>, Desc, <<"\n">>])
    end.

truncate(Bin, MaxBytes) when byte_size(Bin) =< MaxBytes ->
    Bin;
truncate(Bin, MaxBytes) ->
    Truncated = binary:part(Bin, 0, MaxBytes),
    <<Truncated/binary, "\n\n[... truncated for token budget]\n">>.

gv(Key, Map, Default) when is_binary(Key) ->
    maps:get(Key, Map, maps:get(binary_to_atom(Key), Map, Default)).
