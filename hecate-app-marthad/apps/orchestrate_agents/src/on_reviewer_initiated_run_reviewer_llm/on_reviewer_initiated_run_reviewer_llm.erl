%%% @doc LLM runner event handler: on reviewer initiated, run LLM and complete/fail.
%%% Reacts to reviewer_initiated_v1 events.
%%% Loads role spec, builds prompt, calls chat_to_llm, parses notation,
%%% then dispatches complete or fail command.
-module(on_reviewer_initiated_run_reviewer_llm).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

-define(ROLE, <<"reviewer">>).

interested_in() -> [<<"reviewer_initiated_v1">>].

init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    SessionId = gv(session_id, Data),
    VentureId = gv(venture_id, Data),
    Model = gv(model, Data),
    InputContext = gv(input_context, Data),
    spawn_link(fun() -> run_llm(SessionId, VentureId, Model, InputContext) end),
    {ok, State}.

%% Internal

run_llm(SessionId, VentureId, Model, InputContext) ->
    case load_agent_role:load(?ROLE) of
        {ok, RoleContent} ->
            {ok, KnowledgeCtx} = curate_context:curate(VentureId, ?ROLE, #{}),
            SystemPrompt = <<RoleContent/binary, KnowledgeCtx/binary>>,
            Messages = [
                #{role => <<"system">>, content => SystemPrompt},
                #{role => <<"user">>, content => ensure_binary(InputContext)}
            ],
            Opts = #{
                venture_id => VentureId,
                agent_id => SessionId
            },
            case chat_to_llm:chat(Model, Messages, Opts) of
                {ok, Response} ->
                    handle_llm_success(SessionId, Response);
                {error, Reason} ->
                    handle_llm_failure(SessionId, Reason)
            end;
        {error, Reason} ->
            handle_llm_failure(SessionId, {role_load_failed, Reason})
    end.

handle_llm_success(SessionId, Response) ->
    Content = extract_content(Response),
    TokensIn = extract_tokens_in(Response),
    TokensOut = extract_tokens_out(Response),
    {ParsedTerms, NotationOutput} = parse_notation(Content),
    CmdParams = #{
        session_id => SessionId,
        notation_output => NotationOutput,
        parsed_terms => ParsedTerms,
        tokens_in => TokensIn,
        tokens_out => TokensOut
    },
    case complete_reviewer_v1:new(CmdParams) of
        {ok, Cmd} ->
            case maybe_complete_reviewer:dispatch(Cmd) of
                {ok, _, _} ->
                    logger:info("[~s] completed reviewer session ~s", [?MODULE, SessionId]);
                {error, Reason} ->
                    logger:warning("[~s] failed to dispatch complete for ~s: ~p",
                                  [?MODULE, SessionId, Reason])
            end;
        {error, Reason} ->
            logger:warning("[~s] failed to create complete command for ~s: ~p",
                          [?MODULE, SessionId, Reason])
    end.

handle_llm_failure(SessionId, Reason) ->
    ErrorBin = iolist_to_binary(io_lib:format("~p", [Reason])),
    CmdParams = #{
        session_id => SessionId,
        error_reason => ErrorBin,
        tokens_in => 0,
        tokens_out => 0
    },
    case fail_reviewer_v1:new(CmdParams) of
        {ok, Cmd} ->
            case maybe_fail_reviewer:dispatch(Cmd) of
                {ok, _, _} ->
                    logger:warning("[~s] reviewer session ~s failed: ~s",
                                  [?MODULE, SessionId, ErrorBin]);
                {error, DispErr} ->
                    logger:error("[~s] failed to dispatch fail for ~s: ~p",
                               [?MODULE, SessionId, DispErr])
            end;
        {error, CmdErr} ->
            logger:error("[~s] failed to create fail command for ~s: ~p",
                        [?MODULE, SessionId, CmdErr])
    end.

extract_content(#{message := #{content := C}}) -> C;
extract_content(#{content := [#{text := T} | _]}) -> T;
extract_content(#{content := C}) when is_binary(C) -> C;
extract_content(_) -> <<>>.

extract_tokens_in(#{usage := #{input_tokens := N}}) -> N;
extract_tokens_in(#{prompt_eval_count := N}) -> N;
extract_tokens_in(_) -> 0.

extract_tokens_out(#{usage := #{output_tokens := N}}) -> N;
extract_tokens_out(#{eval_count := N}) -> N;
extract_tokens_out(_) -> 0.

parse_notation(Content) when is_binary(Content), byte_size(Content) > 0 ->
    case martha_notation:parse(Content) of
        {ok, Terms} -> {Terms, Content};
        {error, _} -> {[], Content}
    end;
parse_notation(_) ->
    {[], <<>>}.

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(undefined) -> <<>>;
ensure_binary(V) -> iolist_to_binary(io_lib:format("~p", [V])).

gv(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key), Map, undefined)
    end.
