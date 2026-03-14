%%% @doc Rule definitions for quality auditing agent outputs.
%%% Pure heuristic checks — no LLM needed.
%%% Checks that new agent output references entities/concepts from prior agents.
-module(audit_agent_quality_rules).
-export([check/3]).

%% @doc Check agent output against knowledge graph for quality issues.
%% Returns list of warning binaries.
-spec check(binary(), binary(), map()) -> [binary()].
check(AgentRole, Output, Graph) ->
    Entities = get_entity_names(Graph),
    Insights = get_insight_contents(Graph),
    lists:flatten([
        check_entity_coverage(AgentRole, Output, Entities),
        check_insight_alignment(AgentRole, Output, Insights)
    ]).

%% --- Entity coverage checks ---
%% Downstream agents should reference entities from upstream agents

check_entity_coverage(<<"architect">>, Output, Entities) ->
    %% Architect should reference explorer-identified entities
    ExplorerEntities = [N || {N, #{<<"source_agent">> := <<"explorer">>}} <- Entities],
    missing_references(<<"architect">>, Output, ExplorerEntities, <<"explorer entity">>);

check_entity_coverage(<<"stormer">>, Output, Entities) ->
    %% Stormer should reference architect aggregates
    ArchEntities = [N || {N, #{<<"type">> := T}} <- Entities,
                         T =:= <<"aggregate">> orelse T =:= <<"bounded_context">>],
    missing_references(<<"stormer">>, Output, ArchEntities, <<"architect aggregate">>);

check_entity_coverage(<<"erlang_coder">>, Output, Entities) ->
    %% Coder should reference stormer events and aggregates
    EventEntities = [N || {N, #{<<"type">> := T}} <- Entities,
                          T =:= <<"domain_event">> orelse T =:= <<"command">>],
    missing_references(<<"erlang_coder">>, Output, EventEntities, <<"domain event/command">>);

check_entity_coverage(_Role, _Output, _Entities) ->
    [].

%% --- Insight alignment checks ---
%% Agents should not contradict gate decisions

check_insight_alignment(<<"explorer">>, Output, Insights) ->
    check_gate_respect(Output, Insights, <<"vision">>);
check_insight_alignment(<<"architect">>, Output, Insights) ->
    check_gate_respect(Output, Insights, <<"boundary">>);
check_insight_alignment(<<"stormer">>, Output, Insights) ->
    check_gate_respect(Output, Insights, <<"design">>);
check_insight_alignment(_Role, _Output, _Insights) ->
    [].

%% --- Helpers ---

missing_references(Role, Output, EntityNames, EntityType) ->
    LowerOutput = string:lowercase(Output),
    Missing = [N || N <- EntityNames,
                    N =/= <<>>,
                    string:find(LowerOutput, string:lowercase(N)) =:= nomatch],
    case {Missing, length(EntityNames)} of
        {_, 0} -> [];
        {[], _} -> [];
        {M, Total} when length(M) > Total div 2 ->
            [iolist_to_binary([
                Role, <<" output does not reference ">>,
                integer_to_binary(length(M)), <<" of ">>,
                integer_to_binary(Total), <<" known ">>,
                EntityType, <<" entities. Missing: ">>,
                join_names(lists:sublist(M, 5))
            ])];
        _ ->
            []
    end.

check_gate_respect(Output, Insights, GatePrefix) ->
    GateDecisions = [C || {C, Type} <- Insights,
                          Type =:= <<"gate_decision">> orelse Type =:= <<"gate_rejection">>,
                          string:find(C, GatePrefix) =/= nomatch],
    LowerOutput = string:lowercase(Output),
    Warnings = [iolist_to_binary([
        <<"Output may contradict gate decision: ">>,
        binary:part(D, 0, min(100, byte_size(D)))
    ]) || D <- GateDecisions,
          Keywords <- [extract_keywords(D)],
          contradicts(LowerOutput, Keywords)],
    Warnings.

extract_keywords(Text) ->
    %% Extract key nouns/phrases from gate decisions
    Words = binary:split(string:lowercase(Text), [<<" ">>, <<",">>, <<".">>], [global, trim_all]),
    [W || W <- Words, byte_size(W) > 4].

contradicts(Output, Keywords) ->
    %% Simple heuristic: if gate mentioned "scope" negatively but output expands scope
    NegativePatterns = [<<"not ">>, <<"avoid ">>, <<"don't ">>, <<"without ">>],
    lists:any(fun(Kw) ->
        lists:any(fun(Neg) ->
            NegKw = <<Neg/binary, Kw/binary>>,
            string:find(Output, NegKw) =:= nomatch andalso
            string:find(Output, Kw) =/= nomatch
        end, NegativePatterns)
    end, lists:sublist(Keywords, 5)).

join_names([]) -> <<>>;
join_names([H]) -> H;
join_names([H | T]) ->
    lists:foldl(fun(N, Acc) -> <<Acc/binary, ", ", N/binary>> end, H, T).

get_entity_names(#{entities := Entities}) when is_map(Entities) ->
    [{maps:get(<<"name">>, V, maps:get(name, V, <<>>)), V}
     || {_Id, V} <- maps:to_list(Entities)];
get_entity_names(#{<<"entities">> := Entities}) when is_map(Entities) ->
    [{maps:get(<<"name">>, V, maps:get(name, V, <<>>)), V}
     || {_Id, V} <- maps:to_list(Entities)];
get_entity_names(_) ->
    [].

get_insight_contents(#{insights := Insights}) when is_list(Insights) ->
    [{maps:get(<<"content">>, I, maps:get(content, I, <<>>)),
      maps:get(<<"insight_type">>, I, maps:get(insight_type, I, <<>>))}
     || I <- Insights, not is_superseded(I)];
get_insight_contents(#{<<"insights">> := Insights}) when is_list(Insights) ->
    [{maps:get(<<"content">>, I, maps:get(content, I, <<>>)),
      maps:get(<<"insight_type">>, I, maps:get(insight_type, I, <<>>))}
     || I <- Insights, not is_superseded(I)];
get_insight_contents(_) ->
    [].

is_superseded(#{superseded := true}) -> true;
is_superseded(#{<<"superseded">> := true}) -> true;
is_superseded(_) -> false.
