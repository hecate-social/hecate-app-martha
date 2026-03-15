%%% @doc Bridge between martha notation terms and hecate_plugin_codegen.
%%%
%%% Takes parsed notation terms (from martha_notation:parse/1) and drives
%%% code generation by calling the appropriate hecate_plugin_codegen functions.
%%%
%%% Notation terms are mapped as follows:
%%%   {app, _, cmd|prj|qry, _} + {agg, _, _, _} -> division/1
%%%   {desk, Verb, Event, Fields} (nested in AGG) -> desk/1 (CMD + PRJ)
%%%   {agg, Subject, _, _} -> desk/1 (QRY page + by_id)
%%%   {pm, Name, Target} (nested in AGG) -> integration/1 (process_manager)
%%%   {emit, Name} -> integration/1 (emitter)
%%%   {division_consumes, Fact, From} -> integration/1 (listener)
%%% @end
-module(martha_codegen_bridge).

-export([scaffold/2]).

-spec scaffold([term()], binary()) -> {ok, [string()]}.
scaffold(ParsedTerms, RepoPath) when is_binary(RepoPath) ->
    scaffold(ParsedTerms, binary_to_list(RepoPath));
scaffold(ParsedTerms, RepoPath) when is_list(RepoPath) ->
    Apps = collect_apps(ParsedTerms),
    Aggs = collect_aggs(ParsedTerms),
    Emits = collect_emits(ParsedTerms),
    Consumes = collect_consumes(ParsedTerms),
    Delivers = collect_delivers(ParsedTerms),
    Versions = collect_versions(ParsedTerms),
    AppsDir = filename:join(RepoPath, "apps"),
    AllFiles = lists:append([
        scaffold_divisions(Apps, Aggs, AppsDir),
        scaffold_cmd_desks(Apps, Aggs, AppsDir),
        scaffold_walk_desks(Apps, Aggs, AppsDir),
        scaffold_prj_desks(Apps, Aggs, AppsDir),
        scaffold_qry_desks(Apps, Aggs, AppsDir),
        scaffold_pms(Apps, Aggs, AppsDir),
        scaffold_emitters(Apps, Emits, AppsDir),
        scaffold_listeners(Apps, Consumes, AppsDir),
        scaffold_delivery(Delivers, RepoPath)
    ]),
    scaffold_versions(Versions, RepoPath),
    {ok, AllFiles}.

%% ===================================================================
%% Collectors — extract typed terms from the flat list
%% ===================================================================

collect_apps(Terms) ->
    [{Name, Dept} || {app, Name, Dept, _} <- Terms].

collect_aggs(Terms) ->
    [Agg || {agg, _, _, _} = Agg <- Terms].

collect_emits(Terms) ->
    [Name || {emit, Name} <- Terms].

collect_consumes(Terms) ->
    [{Fact, From} || {division_consumes, Fact, From} <- Terms].

collect_delivers(Terms) ->
    [{Name, Type, Props} || {deliver, Name, Type, Props} <- Terms].

collect_versions(Terms) ->
    [{Vsn, Changelog} || {version, Vsn, Changelog} <- Terms].

%% ===================================================================
%% Scaffolding functions
%% ===================================================================

scaffold_divisions(Apps, Aggs, AppsDir) ->
    CmdApp = find_app(cmd, Apps),
    PrjApp = find_app(prj, Apps),
    QryApp = find_app(qry, Apps),
    case {CmdApp, PrjApp, QryApp} of
        {undefined, _, _} -> [];
        {_, undefined, _} -> [];
        {_, _, undefined} -> [];
        {C, P, Q} ->
            lists:flatmap(fun({agg, Subject, _Stream, Details}) ->
                Flags = maps:get(flags, Details, []),
                Opts = #{
                    subject => Subject,
                    cmd_app => C,
                    prj_app => P,
                    qry_app => Q,
                    output_dir => AppsDir,
                    flags => Flags
                },
                case hecate_plugin_codegen:division(Opts) of
                    {ok, Files} -> Files;
                    _ -> []
                end
            end, Aggs)
    end.

scaffold_cmd_desks(Apps, Aggs, AppsDir) ->
    CmdApp = find_app(cmd, Apps),
    case CmdApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(CmdApp), "src"]),
            lists:flatmap(fun({agg, Subject, _Stream, Details}) ->
                Desks = maps:get(desks, Details, []),
                lists:flatmap(fun({desk, DeskName, Event, _Fields}) ->
                    {Verb, _Subj} = split_verb_subject(DeskName),
                    PastVerb = extract_past_verb(Event, Subject),
                    StoreId = maps:get(store_id, Details, <<"martha_store">>),
                    Opts = #{
                        dept => cmd,
                        verb => Verb,
                        subject => Subject,
                        past_verb => PastVerb,
                        store_id => StoreId,
                        output_dir => SrcDir
                    },
                    case hecate_plugin_codegen:desk(Opts) of
                        {ok, Files} -> Files;
                        _ -> []
                    end
                end, Desks)
            end, Aggs)
    end.

scaffold_prj_desks(Apps, Aggs, AppsDir) ->
    PrjApp = find_app(prj, Apps),
    case PrjApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(PrjApp), "src"]),
            Plural = fun(S) -> hecate_plugin_codegen:pluralize(str(S)) end,
            lists:flatmap(fun({agg, Subject, _Stream, Details}) ->
                Desks = maps:get(desks, Details, []),
                lists:flatmap(fun({desk, _DeskName, Event, _Fields}) ->
                    EventBase = strip_version(Event),
                    Opts = #{
                        dept => prj,
                        event => EventBase,
                        target => list_to_binary(Plural(Subject)),
                        output_dir => SrcDir
                    },
                    case hecate_plugin_codegen:desk(Opts) of
                        {ok, Files} -> Files;
                        _ -> []
                    end
                end, Desks)
            end, Aggs)
    end.

scaffold_qry_desks(Apps, Aggs, AppsDir) ->
    QryApp = find_app(qry, Apps),
    case QryApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(QryApp), "src"]),
            lists:flatmap(fun({agg, Subject, _Stream, _Details}) ->
                PageOpts = #{
                    dept => qry,
                    subject => Subject,
                    type => page,
                    output_dir => SrcDir
                },
                ByIdOpts = #{
                    dept => qry,
                    subject => Subject,
                    type => by_id,
                    output_dir => SrcDir
                },
                PageFiles = case hecate_plugin_codegen:desk(PageOpts) of
                    {ok, F1} -> F1; _ -> []
                end,
                ByIdFiles = case hecate_plugin_codegen:desk(ByIdOpts) of
                    {ok, F2} -> F2; _ -> []
                end,
                PageFiles ++ ByIdFiles
            end, Aggs)
    end.

scaffold_pms(Apps, Aggs, AppsDir) ->
    CmdApp = find_app(cmd, Apps),
    case CmdApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(CmdApp), "src"]),
            lists:flatmap(fun({agg, Subject, _Stream, Details}) ->
                Pms = maps:get(pms, Details, []),
                lists:flatmap(fun({pm, PmName, _Target}) ->
                    {PmVerb, PmSubject} = split_verb_subject(PmName),
                    Opts = #{
                        type => process_manager,
                        source_event => Subject,
                        verb => PmVerb,
                        subject => PmSubject,
                        output_dir => SrcDir
                    },
                    case hecate_plugin_codegen:integration(Opts) of
                        {ok, Files} -> Files;
                        _ -> []
                    end;
                   (_) -> []
                end, Pms)
            end, Aggs)
    end.

scaffold_emitters(Apps, Emits, AppsDir) ->
    CmdApp = find_app(cmd, Apps),
    case CmdApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(CmdApp), "src"]),
            lists:flatmap(fun(EmitName) ->
                {Event, Target} = parse_emit_name(EmitName),
                Opts = #{
                    type => emitter,
                    event => Event,
                    target => Target,
                    output_dir => SrcDir
                },
                case hecate_plugin_codegen:integration(Opts) of
                    {ok, Files} -> Files;
                    _ -> []
                end
            end, Emits)
    end.

scaffold_listeners(Apps, Consumes, AppsDir) ->
    CmdApp = find_app(cmd, Apps),
    case CmdApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(CmdApp), "src"]),
            lists:flatmap(fun({Fact, _From}) ->
                Opts = #{
                    type => listener,
                    fact => Fact,
                    output_dir => SrcDir
                },
                case hecate_plugin_codegen:integration(Opts) of
                    {ok, Files} -> Files;
                    _ -> []
                end
            end, Consumes)
    end.

scaffold_walk_desks(Apps, Aggs, AppsDir) ->
    CmdApp = find_app(cmd, Apps),
    case CmdApp of
        undefined -> [];
        _ ->
            SrcDir = filename:join([AppsDir, str(CmdApp), "src"]),
            lists:flatmap(fun({agg, Subject, _Stream, Details}) ->
                WalkNames = maps:get(walk, Details, []),
                ExplicitDesks = maps:get(desks, Details, []),
                ExplicitDeskNames = [DN || {desk, DN, _, _} <- ExplicitDesks],
                lists:flatmap(fun(WalkName) ->
                    case lists:member(WalkName, ExplicitDeskNames) of
                        true -> [];  %% skip — already covered by explicit DESK
                        false ->
                            {Verb, _Subj} = split_verb_subject(WalkName),
                            PastVerb = past_verb(Verb),
                            Opts = #{
                                dept => cmd,
                                verb => Verb,
                                subject => Subject,
                                past_verb => PastVerb,
                                output_dir => SrcDir
                            },
                            case hecate_plugin_codegen:desk(Opts) of
                                {ok, Files} -> Files;
                                _ -> []
                            end
                    end
                end, WalkNames)
            end, Aggs)
    end.

scaffold_delivery(Delivers, RepoPath) ->
    lists:flatmap(fun({Name, Type, Props}) ->
        OtpVsn = maps:get(<<"OTP">>, Props, <<"27">>),
        Port = maps:get(<<"PORT">>, Props, <<"4444">>),
        Opts = #{
            plugin_name => Name,
            plugin_type => Type,
            otp_version => OtpVsn,
            port => Port,
            output_dir => RepoPath
        },
        case hecate_plugin_codegen:delivery(Opts) of
            {ok, Files} -> Files;
            _ -> []
        end
    end, Delivers).

scaffold_versions(Versions, RepoPath) ->
    lists:foreach(fun({Vsn, Changelog}) ->
        hecate_plugin_codegen_delivery:bump_version(#{
            version => Vsn,
            changelog => Changelog,
            output_dir => RepoPath
        })
    end, Versions).

past_verb(<<"initiate">>) -> <<"initiated">>;
past_verb(<<"archive">>) -> <<"archived">>;
past_verb(<<"activate">>) -> <<"activated">>;
past_verb(<<"suspend">>) -> <<"suspended">>;
past_verb(<<"complete">>) -> <<"completed">>;
past_verb(<<"cancel">>) -> <<"cancelled">>;
past_verb(Verb) -> <<Verb/binary, "d">>.

%% ===================================================================
%% Helpers
%% ===================================================================

find_app(Dept, Apps) ->
    case [Name || {Name, D} <- Apps, D =:= Dept] of
        [Name | _] -> Name;
        [] -> undefined
    end.

split_verb_subject(Name) ->
    Str = str(Name),
    case string:split(Str, "_", leading) of
        [Verb, Rest] -> {list_to_binary(Verb), list_to_binary(Rest)};
        [Single] -> {list_to_binary(Single), <<>>}
    end.

extract_past_verb(Event, Subject) ->
    EvtStr = strip_version_str(str(Event)),
    SubStr = str(Subject) ++ "_",
    case lists:prefix(SubStr, EvtStr) of
        true -> list_to_binary(lists:nthtail(length(SubStr), EvtStr));
        false -> list_to_binary(EvtStr)
    end.

strip_version(Name) ->
    list_to_binary(strip_version_str(str(Name))).

strip_version_str(Str) ->
    case lists:suffix("_v1", Str) of
        true -> lists:sublist(Str, length(Str) - 3);
        false -> Str
    end.

parse_emit_name(Name) ->
    Str = str(Name),
    case lists:suffix("_to_pg", Str) of
        true ->
            Event = lists:sublist(Str, length(Str) - 6),
            {strip_version(Event), pg};
        false ->
            case lists:suffix("_to_mesh", Str) of
                true ->
                    Event = lists:sublist(Str, length(Str) - 8),
                    {strip_version(Event), mesh};
                false ->
                    {strip_version(Name), pg}
            end
    end.

str(V) when is_binary(V) -> binary_to_list(V);
str(V) when is_atom(V) -> atom_to_list(V);
str(V) when is_list(V) -> V.
