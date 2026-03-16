%%% @doc Process Manager: on domain meditation started, run parallel LLM research sessions.
%%%
%%% Reacts to domain_meditation_started_v1 events. For EACH participant in the
%%% snapshot, spawns an independent LLM session with web search tools.
%%% Each participant:
%%%   1. Loads storm role file
%%%   2. Curates context (vision + existing knowledge)
%%%   3. Runs multi-turn agent loop with web_search + web_fetch tools
%%%   4. Parses structured findings from LLM output
%%%   5. Dispatches contribute_meditation_finding_v1 commands
%%% @end
-module(on_domain_meditation_started_run_meditation).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"domain_meditation_started_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    VentureId = gv(venture_id, Data),
    Participants = gv(participants, Data),
    logger:info("[~s] meditation started: venture=~s, ~B participants",
                [?MODULE, VentureId, map_size(Participants)]),
    %% Spawn one process per participant — they research in parallel
    maps:foreach(fun(ParticipantId, ParticipantData) ->
        spawn_link(fun() ->
            safe_run_meditation(VentureId, ParticipantId, ParticipantData)
        end)
    end, Participants),
    {ok, State}.

%% Internal

safe_run_meditation(VentureId, ParticipantId, ParticipantData) ->
    try
        run_meditation(VentureId, ParticipantId, ParticipantData)
    catch
        Class:Reason:Stack ->
            logger:error("[~s] CRASH in meditation runner ~s/~s: ~p:~p~n~p",
                        [?MODULE, VentureId, ParticipantId, Class, Reason, Stack])
    end.

run_meditation(VentureId, ParticipantId, ParticipantData) ->
    Role = maps:get(role, ParticipantData, maps:get(<<"role">>, ParticipantData, <<"domain_expert">>)),
    CustomInstructions = maps:get(custom_instructions, ParticipantData,
                            maps:get(<<"custom_instructions">>, ParticipantData, undefined)),
    RolePath = <<"storm/", Role/binary>>,
    logger:info("[~s] loading role ~s for participant ~s", [?MODULE, RolePath, ParticipantId]),
    case load_agent_role:load(RolePath) of
        {ok, RoleContent} ->
            KnowledgeCtx = case curate_context:curate(VentureId, Role, #{}) of
                {ok, Ctx} -> Ctx;
                _ -> <<>>
            end,
            SystemPrompt = build_system_prompt(RoleContent, KnowledgeCtx, CustomInstructions),
            UserPrompt = <<"Begin your domain research now. Use web_search and web_fetch tools to gather information. Report all findings in the structured format described in your role instructions.">>,
            Messages = [
                #{role => <<"system">>, content => SystemPrompt},
                #{role => <<"user">>, content => UserPrompt}
            ],
            Tools = [web_search:tool_definition(), web_fetch:tool_definition()],
            Model = default_model(),
            Opts = #{
                venture_id => VentureId,
                agent_id => ParticipantId,
                max_iterations => 8
            },
            logger:info("[~s] starting LLM research for ~s (role=~s, model=~s)",
                        [?MODULE, ParticipantId, Role, Model]),
            case run_agent_with_tools:run(Model, Messages, Tools, Opts) of
                {ok, Content} ->
                    logger:info("[~s] LLM research completed for ~s", [?MODULE, ParticipantId]),
                    Findings = parse_findings(Content),
                    dispatch_findings(VentureId, ParticipantId, Findings);
                {error, Reason} ->
                    logger:warning("[~s] LLM research failed for ~s: ~p",
                                  [?MODULE, ParticipantId, Reason])
            end;
        {error, Reason} ->
            logger:error("[~s] failed to load role ~s: ~p", [?MODULE, RolePath, Reason])
    end.

build_system_prompt(RoleContent, KnowledgeCtx, undefined) ->
    <<RoleContent/binary, "\n\n## Existing Knowledge\n", KnowledgeCtx/binary>>;
build_system_prompt(RoleContent, KnowledgeCtx, CustomInstructions) ->
    <<RoleContent/binary,
      "\n\n## Custom Instructions\n", CustomInstructions/binary,
      "\n\n## Existing Knowledge\n", KnowledgeCtx/binary>>.

default_model() ->
    case os:getenv("MEDITATION_MODEL") of
        false ->
            case os:getenv("DEFAULT_LLM_MODEL") of
                false -> <<"gemma3:12b">>;
                Model -> list_to_binary(Model)
            end;
        Model -> list_to_binary(Model)
    end.

parse_findings(Content) ->
    %% Parse ```finding blocks from LLM output
    case re:run(Content, <<"```finding\\s*\\n(.*?)```">>,
                [global, dotall, {capture, [1], binary}]) of
        {match, Matches} ->
            lists:filtermap(fun([Block]) -> parse_single_finding(Block) end, Matches);
        nomatch ->
            []
    end.

parse_single_finding(Block) ->
    Type = extract_field(Block, <<"type">>),
    ContentField = extract_field(Block, <<"content">>),
    case {Type, ContentField} of
        {undefined, _} -> false;
        {_, undefined} -> false;
        _ ->
            Sources = extract_sources(Block),
            {true, #{
                finding_type => Type,
                content => ContentField,
                sources => Sources
            }}
    end.

extract_field(Block, FieldName) ->
    Pattern = <<FieldName/binary, ":\\s*(.+?)\\n">>,
    case re:run(Block, Pattern, [{capture, [1], binary}]) of
        {match, [Value]} -> string:trim(Value);
        nomatch -> undefined
    end.

extract_sources(Block) ->
    %% Extract source blocks: - url: ...\n    title: ...\n    snippet: ...
    case re:run(Block, <<"- url:\\s*(.+?)\\n\\s*title:\\s*(.+?)\\n\\s*snippet:\\s*(.+?)(?:\\n|$)">>,
                [global, {capture, [1, 2, 3], binary}]) of
        {match, Matches} ->
            [#{url => string:trim(U), title => string:trim(T), snippet => string:trim(S)}
             || [U, T, S] <- Matches];
        nomatch ->
            []
    end.

dispatch_findings(VentureId, ParticipantId, Findings) ->
    lists:foreach(fun(Finding) ->
        Params = Finding#{
            venture_id => VentureId,
            participant_id => ParticipantId
        },
        case contribute_meditation_finding_v1:from_map(Params) of
            {ok, Cmd} ->
                case maybe_contribute_meditation_finding:dispatch(Cmd) of
                    {ok, _, _} ->
                        logger:info("[~s] dispatched finding for ~s: ~s",
                                   [?MODULE, ParticipantId, maps:get(finding_type, Finding)]);
                    {error, Reason} ->
                        logger:warning("[~s] failed to dispatch finding: ~p", [?MODULE, Reason])
                end;
            {error, Reason} ->
                logger:warning("[~s] failed to create finding command: ~p", [?MODULE, Reason])
        end
    end, Findings),
    logger:info("[~s] dispatched ~B findings for ~s", [?MODULE, length(Findings), ParticipantId]).

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
